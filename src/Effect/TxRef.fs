namespace Effect

open System.Collections.Generic

/// Port of repos/effect-smol/packages/effect/src/TxRef.ts — transactional
/// references with version-based optimistic concurrency.
///
/// Model:
///   - A `TxRef<'a>` is a mutable cell `{ value; version }`.
///   - A transaction (`Stm<'a>`) runs its body against a fresh per-tx *journal*
///     (read set + pending writes) with NO locks held (optimistic).
///   - `atomically` then validates under a single commit gate: every TxRef the
///     tx touched must still be at the version it was seen at. If all valid, the
///     pending writes are applied (bumping versions) and waiters are resumed;
///     otherwise the journal is discarded and the whole tx re-runs.
///   - `retry` aborts the tx and suspends asynchronously until one of the TxRefs
///     read by the transaction is committed, then re-runs.
///
/// Porting adaptations vs upstream (noted per CONVENTIONS):
///   - Upstream threads the transaction journal through the `Effect` runtime
///     (`Effect.tx`/`Effect.atomic`/`Effect.txRetry`). Our async `Effect` core has
///     no transaction state yet, so v1 models the transaction as a dedicated
///     `Stm<'a>` monad and exposes `atomically : Stm<'a> -> Effect<'a,'E,'R>` as
///     the boundary. Transactional ops therefore return `Stm<_>` (composable with
///     `stm { }`), not `Effect<_>`.
///   - The retry path is Fable/Node-safe: it uses `Cell` waiters instead of
///     blocking an OS thread.
///   - The seqlock memory fix is mandatory: a naive unsynchronized read can
///     pair a fresh `version` with a stale `value` on a weak-memory CPU (arm64)
///     and silently lose updates. See `TxRef` below.
///   - HKT/variance plumbing and JS-runtime machinery (`TypeId`, `Proto`,
///     `toJSON`, `pipe`/`dual`) are dropped; ops take `self` first.
/// Non-generic view of a `TxRef` so a heterogeneous journal can validate/commit
/// without knowing the element type.
type internal ITxVar =
    abstract member Version: int
    /// Apply a boxed value as the new committed value and bump the version.
    abstract member CommitBoxed: obj -> unit
    /// Register a waiter resumed when this TxRef commits a value.
    abstract member AddPending: Cell<unit> -> unit
    /// Resume and clear waiters after this TxRef commits a value.
    abstract member CompletePending: unit -> unit

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
    let pending = ResizeArray<Cell<unit>>()

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

        member _.AddPending cell = pending.Add cell

        member _.CompletePending() =
            let waiters = pending.ToArray()
            pending.Clear()

            for waiter in waiters do
                Cell.trySet waiter () |> ignore

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

        for kv in entries do
            let e = kv.Value

            if e.HasWrite then
                e.Var.CompletePending()

    /// Register `cell` on every TxRef read by this transaction.
    member _.AddPending(cell: Cell<unit>) =
        for kv in entries do
            kv.Value.Var.AddPending cell

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

    /// The single global gate guarding validate+commit and retry registration.
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

    type private Attempt<'a> =
        | Done of 'a
        | Rerun
        | Suspend of Cell<unit>

    let private tryCommit (body: TxLog -> 'a) : Attempt<'a> =
        let log = TxLog()

        let outcome =
            try
                Ok(body log)
            with RetryException ->
                Error()

        lock commitLock (fun () ->
            match outcome with
            | Ok value when log.IsValid() ->
                log.Commit()
                Done value
            | Ok _ -> Rerun
            | Error() ->
                if log.IsValid() then
                    let cell = Cell.make ()
                    log.AddPending cell
                    Suspend cell
                else
                    Rerun)

    /// Run a transaction to commit asynchronously, returning its result directly.
    /// Conflicts rerun immediately; `retry` suspends until a touched TxRef commits.
    let atomicallyAsync (Stm body) : Async<'a> =
        let rec loop () =
            async {
                match tryCommit body with
                | Done value -> return value
                | Rerun -> return! loop ()
                | Suspend cell ->
                    do! Cell.await cell
                    return! loop ()
            }

        loop ()

    /// Run a transaction synchronously. This is only for operations that are known
    /// not to `retry`; retrying transactions must use `atomically`.
    let atomicallyUnsafe (txn: Stm<'a>) : 'a =
#if FABLE_COMPILER
        failwith "TxRef.atomicallyUnsafe is not available on Fable; use TxRef.atomically"
#else
        Async.RunSynchronously(atomicallyAsync txn)
#endif

    /// Run a transaction as an `Effect`. This is the `Effect.atomic` boundary.
    /// (Effect.tx / Effect.atomic)
    let atomically (txn: Stm<'a>) : Effect<'a, 'E, 'R> =
        Effect(fun _ _ ->
            async {
                let! value = atomicallyAsync txn
                return Success value
            })
