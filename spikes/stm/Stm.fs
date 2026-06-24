module Stm

open System.Collections.Generic
open System.Threading

/// STM spike — version-based optimistic concurrency (the same algorithm used by
/// Effect's `TxRef.ts` and FSharpx's `Stm.fs`).
///
/// Model:
///   - A `TVar<'a>` is a mutable cell `{ value; version }`.
///   - A transaction runs its body against a fresh per-tx *journal* (read set +
///     pending writes) with NO locks held (optimistic).
///   - `atomically` then takes a single global lock and VALIDATES: every TVar the
///     tx touched must still be at the version it was seen at. If all valid, the
///     pending writes are applied (bumping versions) and waiters are `PulseAll`ed.
///     Otherwise the journal is discarded and the whole tx re-runs.
///   - `retry` aborts the tx; under the global lock it `Monitor.Wait`s until a
///     committed write pulses, then re-runs. (v1 THREAD-BLOCKS on retry — there
///     is no fiber runtime yet.)

/// Non-generic view of a TVar so a heterogeneous journal can validate/commit
/// without knowing the element type.
type internal ITVar =
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
type TVar<'a> internal (initial: 'a) =
    [<VolatileField>]
    let mutable version = 0

    let mutable value = initial // guarded by `version` (seqlock)

    /// Live (non-transactional) read — used by tests/benchmarks at the edges.
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

    interface ITVar with
        member _.Version = version

        member _.CommitBoxed boxed =
            value <- (boxed :?> 'a) // publish value first ...
            version <- version + 1 // ... then the volatile version write releases it

/// One journal slot per touched TVar.
type internal Entry =
    { Var: ITVar
      SeenVersion: int
      mutable Boxed: obj
      mutable HasWrite: bool }

/// Per-transaction journal: the read set + pending writes, keyed by TVar
/// reference identity.
type internal TxLog() =
    let entries = Dictionary<ITVar, Entry>(HashIdentity.Reference)

    member _.Entries = entries

    member _.TryGet(v: ITVar) : Entry option =
        match entries.TryGetValue v with
        | true, e -> Some e
        | _ -> None

    member _.Add(e: Entry) = entries[e.Var] <- e

    /// Every touched TVar must still be at the version we observed.
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
            if e.HasWrite then e.Var.CommitBoxed e.Boxed

    /// Shallow snapshot of the current entries (for `orElse` rollback).
    member _.Snapshot() =
        let copy = Dictionary<ITVar, Entry>(HashIdentity.Reference)

        for kv in entries do
            copy[kv.Key] <-
                { kv.Value with
                    Boxed = kv.Value.Boxed }

        copy

    member _.Restore(snapshot: Dictionary<ITVar, Entry>) =
        entries.Clear()
        for kv in snapshot do entries[kv.Key] <- kv.Value

/// Signals an explicit `retry` (or an `orElse` left-branch abort).
exception internal RetryException

/// A transaction is a function from the journal to a result. It may raise
/// `RetryException` to abort.
type Stm<'a> = internal Stm of (TxLog -> 'a)

/// The single global lock guarding validate+commit and serving as the retry
/// wait/pulse monitor.
let private commitLock = obj ()

// --- core operations ---

/// Allocate a fresh `TVar` outside any transaction.
let newTVarUnsafe (initial: 'a) : TVar<'a> = TVar<'a>(initial)

/// Allocate a fresh `TVar` inside a transaction. Creation never conflicts, so it
/// is performed immediately and not journaled.
let newTVar (initial: 'a) : Stm<TVar<'a>> = Stm(fun _ -> TVar<'a>(initial))

/// Read a `TVar`. The first read records its seen version; later reads in the
/// same tx return the journaled (seen or pending-write) value.
let readTVar (self: TVar<'a>) : Stm<'a> =
    Stm(fun log ->
        let iv = self :> ITVar

        match log.TryGet iv with
        | Some e -> e.Boxed :?> 'a
        | None ->
            // Consistent (seqlock) snapshot: the seen version is guaranteed to
            // correspond to the value we record, so validation against it is sound.
            let struct (seen, v) = self.ReadConsistent()

            log.Add
                { Var = iv
                  SeenVersion = seen
                  Boxed = box v
                  HasWrite = false }

            v)

/// Write a `TVar`. A blind write (no prior read) records the current version, so
/// write-write conflicts are still detected at commit.
let writeTVar (self: TVar<'a>) (value: 'a) : Stm<unit> =
    Stm(fun log ->
        let iv = self :> ITVar

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

/// Abort the transaction and block until a committed write changes one of the
/// TVars it read, then re-run.
let retry<'a> : Stm<'a> = Stm(fun _ -> raise RetryException)

/// Try `first`; if it calls `retry`, roll back its journal effects and run
/// `second` instead.
let orElse (Stm first) (Stm second) : Stm<'a> =
    Stm(fun log ->
        let snapshot = log.Snapshot()

        try
            first log
        with RetryException ->
            log.Restore snapshot
            second log)

// --- runner ---

/// Run a transaction to commit. Loops on conflict; thread-blocks on `retry`
/// until a relevant commit pulses.
let atomically (Stm body) : 'a =
    let mutable result = ValueNone

    while result.IsNone do
        let log = TxLog()

        let outcome =
            try
                Ok(body log)
            with RetryException ->
                Error()

        // Validate + commit (or wait) atomically under the global lock so a
        // committer's PulseAll cannot be lost between our validate and our wait.
        Monitor.Enter commitLock

        try
            match outcome with
            | Ok value when log.IsValid() ->
                log.Commit()
                Monitor.PulseAll commitLock
                result <- ValueSome value
            | Ok _ -> () // read/write conflict — loop and re-run
            | Error() ->
                // Explicit retry: only block if our read set is still consistent;
                // if it already changed, re-run immediately.
                if log.IsValid() then
                    Monitor.Wait commitLock |> ignore
        finally
            Monitor.Exit commitLock

    result.Value

// --- computation expression ---

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
    let stm = StmBuilder()

// --- derived helpers (mirror TxRef.ts: modify / update / get / set) ---

/// Read-modify-write returning a separate result: `f current -> (result, next)`.
let modify (self: TVar<'a>) (f: 'a -> 'r * 'a) : Stm<'r> =
    stm {
        let! current = readTVar self
        let (result, next) = f current
        do! writeTVar self next
        return result
    }

let update (self: TVar<'a>) (f: 'a -> 'a) : Stm<unit> = modify self (fun c -> ((), f c))
let get (self: TVar<'a>) : Stm<'a> = readTVar self
let set (self: TVar<'a>) (value: 'a) : Stm<unit> = update self (fun _ -> value)
