namespace Effect

open System.Collections.Generic
open System.Threading

/// Port of repos/effect-smol/packages/effect/src/TxRef.ts — transactional
/// references with **version-based optimistic concurrency** (the same algorithm
/// as Effect's `TxRef` and FSharpx's `Stm`). Validated against the proven spike
/// at `spikes/stm/Stm.fs`.
///
/// Model:
///   - A `TxRef<'a>` is a mutable cell `{ value; version }`.
///   - A transaction (`Stm<'a>`) runs its body against a fresh per-tx *journal*
///     (read set + pending writes) with NO locks held (optimistic).
///   - `atomically` then takes a single global lock and VALIDATES: every TxRef the
///     tx touched must still be at the version it was seen at. If all valid, the
///     pending writes are applied (bumping versions) and waiters are `PulseAll`ed;
///     otherwise the journal is discarded and the whole tx re-runs.
///   - `retry` aborts the tx; under the global lock it `Monitor.Wait`s until a
///     committed write pulses, then re-runs.
///
/// Porting adaptations vs upstream (noted per CONVENTIONS):
///   - Upstream threads the transaction journal through the `Effect` runtime
///     (`Effect.tx`/`Effect.atomic`/`Effect.txRetry`). Our async `Effect` core has
///     no transaction state yet, so v1 models the transaction as a dedicated
///     `Stm<'a>` monad and exposes `atomically : Stm<'a> -> Effect<'a,'E,'R>` as
///     the boundary. Transactional ops therefore return `Stm<_>` (composable with
///     `stm { }`), not `Effect<_>`.
///   - v1 **thread-blocks** on `retry` (`Monitor.Wait`). Replacing this with
///     fiber-park is a later upgrade once the fiber runtime lands.
///   - The **seqlock memory fix** from the spike is mandatory: a naive
///     unsynchronized read can pair a fresh `version` with a stale `value` on a
///     weak-memory CPU (arm64) and silently lose updates. See `TxRef` below.
///   - HKT/variance plumbing and JS-runtime machinery (`TypeId`, `Proto`,
///     `toJSON`, `pipe`/`dual`) are dropped; ops take `self` first.
/// Non-generic view of a `TxRef` so a heterogeneous journal can validate/commit
/// without knowing the element type.
type internal ITxVar =
    abstract member Version: int
    /// Apply a boxed value as the new committed value and bump the version.
    abstract member CommitBoxed: obj -> unit

/// A transactional reference: a value tagged with a monotonically increasing
/// version. Reference identity (not structural) keys the journal.
///
/// Memory model: body reads run WITHOUT the global lock, so on a weak-memory CPU
/// (e.g. arm64) a naive read can pair a fresh `version` with a stale `value`,
/// passing validation against the wrong base and silently losing an update. We
/// prevent this with a **seqlock**: `version` is a volatile field written *after*
/// `value` on commit, and `ReadConsistent` re-reads until the version is stable
/// around the value snapshot. Validate/commit themselves run under the global
/// lock, whose fences make version reads there authoritative.
[<Sealed>]
type TxRef<'a> internal (initial: 'a) =
    [<VolatileField>]
    let mutable version = 0

    let mutable value = initial // guarded by `version` (seqlock)

    /// Live (non-transactional) read — for edges/inspection only.
    member _.ValueUnsafe = value

    /// Lock-free consistent snapshot of `(version, value)`.
    member internal _.ReadConsistent() : struct (int * 'a) =
        let mutable v1 = version
        let mutable snap = value
        let mutable v2 = version

        while v1 <> v2 do
            v1 <- version
            snap <- value
            v2 <- version

        struct (v1, snap)

    interface ITxVar with
        member _.Version = version

        member _.CommitBoxed boxed =
            value <- (boxed :?> 'a) // publish value first ...
            version <- version + 1 // ... then the volatile version write releases it

/// One journal slot per touched `TxRef`.
type internal Entry =
    { Var: ITxVar
      SeenVersion: int
      mutable Boxed: obj
      mutable HasWrite: bool }

/// Per-transaction journal: the read set + pending writes, keyed by `TxRef`
/// reference identity.
type internal TxLog() =
    let entries = Dictionary<ITxVar, Entry>(HashIdentity.Reference)

    member _.Entries = entries

    member _.TryGet(v: ITxVar) : Entry option =
        match entries.TryGetValue v with
        | true, e -> Some e
        | _ -> None

    member _.Add(e: Entry) = entries[e.Var] <- e

    /// Every touched `TxRef` must still be at the version we observed.
    member _.IsValid() =
        let mutable ok = true

        for kv in entries do
            if kv.Value.Var.Version <> kv.Value.SeenVersion then
                ok <- false

        ok

    /// Apply pending writes (must run under the global commit lock).
    member _.Commit() =
        for kv in entries do
            let e = kv.Value

            if e.HasWrite then
                e.Var.CommitBoxed e.Boxed

    /// Shallow snapshot of the current entries (for `orElse` rollback).
    member _.Snapshot() =
        let copy = Dictionary<ITxVar, Entry>(HashIdentity.Reference)

        for kv in entries do
            copy[kv.Key] <- { kv.Value with Boxed = kv.Value.Boxed }

        copy

    member _.Restore(snapshot: Dictionary<ITxVar, Entry>) =
        entries.Clear()

        for kv in snapshot do
            entries[kv.Key] <- kv.Value

/// Signals an explicit `retry` (or an `orElse` left-branch abort).
exception internal RetryException

/// A transaction: a function from the journal to a result. May raise
/// `RetryException` to abort. Public type, internal representation (like `Effect`).
type Stm<'a> = internal Stm of (TxLog -> 'a)

/// Computation-expression builder for composing transactions: `stm { let! ... }`.
type StmBuilder() =
    member _.Return(x: 'a) : Stm<'a> = Stm(fun _ -> x)
    member _.ReturnFrom(s: Stm<'a>) : Stm<'a> = s
    member _.Zero() : Stm<unit> = Stm(fun _ -> ())

    member _.Bind(Stm m, f: 'a -> Stm<'b>) : Stm<'b> =
        Stm(fun log ->
            let (Stm m2) = f (m log)
            m2 log)

    member _.Delay(f: unit -> Stm<'a>) : Stm<'a> =
        Stm(fun log ->
            let (Stm m) = f ()
            m log)

    member _.Combine(Stm a, Stm b) : Stm<'b> =
        Stm(fun log ->
            a log |> ignore
            b log)

[<AutoOpen>]
module StmBuilderModule =
    /// `stm { let! x = TxRef.get r; do! TxRef.set r (x + 1) }`
    let stm = StmBuilder()

[<RequireQualifiedAccess>]
module TxRef =

    /// The single global lock guarding validate+commit and serving as the retry
    /// wait/pulse monitor.
    let private commitLock = obj ()

    // --- constructors ---

    /// Allocate a `TxRef` outside the `Effect`/transaction context.
    /// (TxRef.makeUnsafe)
    let makeUnsafe (initial: 'a) : TxRef<'a> = TxRef<'a>(initial)

    /// Allocate a `TxRef`, wrapped in an `Effect`. (TxRef.make)
    let make (initial: 'a) : Effect<TxRef<'a>, 'E, 'R> =
        Effect.sync (fun () -> makeUnsafe initial)

    /// Allocate a fresh `TxRef` *inside* a transaction. Creation never conflicts,
    /// so it is performed immediately and not journaled.
    let makeStm (initial: 'a) : Stm<TxRef<'a>> = Stm(fun _ -> TxRef<'a>(initial))

    /// Live (non-transactional) read. (TxRef.getUnsafe)
    let getUnsafe (self: TxRef<'a>) : 'a = self.ValueUnsafe

    // --- transactional operations (compose with `stm { }`) ---

    /// Read within the current transaction. The first read records the seen
    /// version; later reads return the journaled (seen or pending-write) value.
    /// (TxRef.get)
    let get (self: TxRef<'a>) : Stm<'a> =
        Stm(fun log ->
            let iv = self :> ITxVar

            match log.TryGet iv with
            | Some e -> e.Boxed :?> 'a
            | None ->
                // Consistent (seqlock) snapshot: the seen version corresponds to
                // the value recorded, so validation against it is sound.
                let struct (seen, v) = self.ReadConsistent()

                log.Add
                    { Var = iv
                      SeenVersion = seen
                      Boxed = box v
                      HasWrite = false }

                v)

    /// Write within the current transaction. A blind write (no prior read)
    /// records the current version, so write-write conflicts are still detected.
    /// (TxRef.set)
    let set (self: TxRef<'a>) (value: 'a) : Stm<unit> =
        Stm(fun log ->
            let iv = self :> ITxVar

            match log.TryGet iv with
            | Some e ->
                e.Boxed <- box value
                e.HasWrite <- true
            | None ->
                log.Add
                    { Var = iv
                      SeenVersion = iv.Version
                      Boxed = box value
                      HasWrite = true })

    /// Read-modify-write returning a separate result: `f current -> (result, next)`.
    /// (TxRef.modify)
    let modify (self: TxRef<'a>) (f: 'a -> 'r * 'a) : Stm<'r> =
        stm {
            let! current = get self
            let (result, next) = f current
            do! set self next
            return result
        }

    /// Update the value with `f`. (TxRef.update)
    let update (self: TxRef<'a>) (f: 'a -> 'a) : Stm<unit> = modify self (fun c -> ((), f c))

    // --- control ---

    /// Abort the transaction and block until a committed write changes one of the
    /// TxRefs it read, then re-run. (Effect.txRetry)
    let retry<'a> : Stm<'a> = Stm(fun _ -> raise RetryException)

    /// Try `first`; if it calls `retry`, roll back its journal effects and run
    /// `second` instead. (Effect STM `orElse`)
    let orElse (first: Stm<'a>) (second: Stm<'a>) : Stm<'a> =
        let (Stm runFirst) = first
        let (Stm runSecond) = second

        Stm(fun log ->
            let snapshot = log.Snapshot()

            try
                runFirst log
            with RetryException ->
                log.Restore snapshot
                runSecond log)

    // --- runners (the Stm -> Effect boundary) ---

    /// Run a transaction to commit, returning its result directly. Loops on
    /// conflict; thread-blocks on `retry` until a relevant commit pulses.
    let atomicallyUnsafe (Stm body) : 'a =
        let mutable result = ValueNone

        while result.IsNone do
            let log = TxLog()

            let outcome =
                try
                    Ok(body log)
                with RetryException ->
                    Error()

            // Validate + commit (or wait) atomically under the global lock so a
            // committer's PulseAll cannot be lost between our validate and wait.
            Monitor.Enter commitLock

            try
                match outcome with
                | Ok value when log.IsValid() ->
                    log.Commit()
                    Monitor.PulseAll commitLock
                    result <- ValueSome value
                | Ok _ -> () // read/write conflict — loop and re-run
                | Error() ->
                    // Explicit retry: only block if our read set is still
                    // consistent; if it already changed, re-run immediately.
                    if log.IsValid() then
                        Monitor.Wait commitLock |> ignore
            finally
                Monitor.Exit commitLock

        result.Value

    /// Run a transaction as an `Effect`. This is the `Effect.atomic` boundary.
    /// (Effect.tx / Effect.atomic)
    let atomically (txn: Stm<'a>) : Effect<'a, 'E, 'R> =
        Effect.sync (fun () -> atomicallyUnsafe txn)
