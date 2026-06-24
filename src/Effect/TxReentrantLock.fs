namespace Effect

open System.Threading
open System.Runtime.CompilerServices

/// Port of repos/effect-smol/packages/effect/src/TxReentrantLock.ts — a
/// transactional reentrant read/write lock. Many fibers may hold read locks
/// concurrently, or one fiber holds the write lock exclusively; a fiber that
/// already holds the lock can re-acquire it (reentrancy), and a write holder may
/// take read locks. Ownership is tracked per fiber; attempts that cannot proceed
/// `retry` transactionally.
///
/// Porting adaptations (per CONVENTIONS):
///   - Upstream keys ownership by `fiber.id` (an `int`). Our fiber kernel's
///     `FiberRuntime` carries no id, so each operation captures the running
///     `FiberRuntime` and maps it — by **reference identity** — to a stable unique
///     int via a `ConditionalWeakTable` (`fiberId` below). Forked fibers get a
///     fresh `FiberRuntime` (so a distinct id); all effects on one fiber share it.
///   - Each op is its own atomic transaction (`TxRef.atomicallyUnsafe`, wrapped in
///     an `Effect` that supplies the captured fiber id). Blocking acquires
///     thread-block on `retry` (as the whole STM layer does pending fiber-park).
///   - JS-runtime machinery (`TypeId`, `Proto`, `toJSON`, `isTxReentrantLock`,
///     `pipe`/`dual`) is dropped; ops take `self` first. `readLock`/`writeLock`
///     take an explicit `Scope` rather than requiring it from the environment.

/// The lock's ownership state: per-fiber read counts and an optional writer
/// `(fiberId, count)`.
type internal LockState =
    { Readers: HashMap<int, int>
      Writer: (int * int) option }

type TxReentrantLock = internal { StateRef: TxRef<LockState> }

[<RequireQualifiedAccess>]
module TxReentrantLock =

    let private emptyState: LockState = { Readers = HashMap.empty; Writer = None }

    // --- fiber identity (reference-keyed, since FiberRuntime has no id) ---

    let private fiberIds = ConditionalWeakTable<FiberRuntime, obj>()
    let private nextId = ref 0

    let private fiberId (fib: FiberRuntime) : int =
        unbox (fiberIds.GetValue(fib, fun _ -> box (Interlocked.Increment nextId)))

    /// Run an `Stm` built from the *current fiber's* id as its own transaction.
    let private txWithFiber (build: int -> Stm<'a>) : Effect<'a, 'E, 'R> =
        Effect(fun fib _ ->
            async {
                try
                    return Success(TxRef.atomicallyUnsafe (build (fiberId fib)))
                with ex ->
                    return Exit.die (box ex)
            })

    let private readerCount (state: LockState) (fid: int) : int =
        match HashMap.get state.Readers fid with
        | Some c -> c
        | None -> 0

    // --- constructors ---

    /// Create a new, unlocked reentrant lock. (TxReentrantLock.make)
    let make () : Effect<TxReentrantLock, 'E, 'R> =
        TxRef.make emptyState |> Effect.map (fun ref -> { StateRef = ref })

    // --- mutations ---

    /// Acquire a read lock (reentrant). Retries while another fiber holds the
    /// write lock. Returns this fiber's read-lock count. (TxReentrantLock.acquireRead)
    let acquireRead (self: TxReentrantLock) : Effect<int, 'E, 'R> =
        txWithFiber (fun fid ->
            stm {
                let! state = TxRef.get self.StateRef

                match state.Writer with
                | Some(wid, _) when wid <> fid -> return! TxRef.retry
                | _ ->
                    let next = readerCount state fid + 1
                    do! TxRef.set self.StateRef { state with Readers = HashMap.set state.Readers fid next }
                    return next
            })

    /// Acquire the write lock (reentrant / upgrade from read). Retries while any
    /// other fiber holds a read or write lock. Returns this fiber's write-lock
    /// count. (TxReentrantLock.acquireWrite)
    let acquireWrite (self: TxReentrantLock) : Effect<int, 'E, 'R> =
        txWithFiber (fun fid ->
            stm {
                let! state = TxRef.get self.StateRef

                let blockedByWriter =
                    match state.Writer with
                    | Some(wid, _) -> wid <> fid
                    | None -> false

                let blockedByReader =
                    HashMap.keys state.Readers
                    |> Seq.exists (fun rid -> rid <> fid && readerCount state rid > 0)

                if blockedByWriter || blockedByReader then
                    return! TxRef.retry
                else
                    match state.Writer with
                    | Some(_, c) ->
                        let next = c + 1
                        do! TxRef.set self.StateRef { state with Writer = Some(fid, next) }
                        return next
                    | None ->
                        do! TxRef.set self.StateRef { state with Writer = Some(fid, 1) }
                        return 1
            })

    /// Release one read lock held by the current fiber. Returns the remaining
    /// count (0 if none was held). (TxReentrantLock.releaseRead)
    let releaseRead (self: TxReentrantLock) : Effect<int, 'E, 'R> =
        txWithFiber (fun fid ->
            stm {
                let! state = TxRef.get self.StateRef
                let current = readerCount state fid

                if current <= 0 then
                    return 0
                else
                    let next = current - 1

                    let readers =
                        if next = 0 then
                            HashMap.remove state.Readers fid
                        else
                            HashMap.set state.Readers fid next

                    do! TxRef.set self.StateRef { state with Readers = readers }
                    return next
            })

    /// Release one write lock held by the current fiber. Returns the remaining
    /// count (0 if none was held). (TxReentrantLock.releaseWrite)
    let releaseWrite (self: TxReentrantLock) : Effect<int, 'E, 'R> =
        txWithFiber (fun fid ->
            stm {
                let! state = TxRef.get self.StateRef

                match state.Writer with
                | Some(wid, c) when wid = fid ->
                    let next = c - 1
                    let writer = if next <= 0 then None else Some(fid, next)
                    do! TxRef.set self.StateRef { state with Writer = writer }
                    return next
                | _ -> return 0
            })

    // --- scoped / wrapped usage ---

    /// Hold a read lock for the lifetime of `scope`. (TxReentrantLock.readLock)
    let readLock (self: TxReentrantLock) (scope: Scope<'E, 'R>) : Effect<int, 'E, 'R> =
        Scope.acquireRelease scope (acquireRead self) (fun _ _ -> releaseRead self |> Effect.map ignore)

    /// Hold a write lock for the lifetime of `scope`. (TxReentrantLock.writeLock)
    let writeLock (self: TxReentrantLock) (scope: Scope<'E, 'R>) : Effect<int, 'E, 'R> =
        Scope.acquireRelease scope (acquireWrite self) (fun _ _ -> releaseWrite self |> Effect.map ignore)

    /// Run `effect` while holding a read lock, released afterwards even on failure
    /// or interruption. (TxReentrantLock.withReadLock)
    let withReadLock (self: TxReentrantLock) (effect: Effect<'A, 'E, 'R>) : Effect<'A, 'E, 'R> =
        Effect.acquireUseRelease (acquireRead self) (fun _ -> effect) (fun _ _ -> releaseRead self |> Effect.map ignore)

    /// Run `effect` while holding a write lock, released afterwards even on
    /// failure or interruption. (TxReentrantLock.withWriteLock)
    let withWriteLock (self: TxReentrantLock) (effect: Effect<'A, 'E, 'R>) : Effect<'A, 'E, 'R> =
        Effect.acquireUseRelease
            (acquireWrite self)
            (fun _ -> effect)
            (fun _ _ -> releaseWrite self |> Effect.map ignore)

    /// Alias for `withWriteLock`. (TxReentrantLock.withLock)
    let withLock (self: TxReentrantLock) (effect: Effect<'A, 'E, 'R>) : Effect<'A, 'E, 'R> = withWriteLock self effect

    // --- getters ---

    /// Total read locks held across all fibers. (TxReentrantLock.readLocks)
    let readLocks (self: TxReentrantLock) : Effect<int, 'E, 'R> =
        TxRef.atomically (TxRef.get self.StateRef)
        |> Effect.map (fun state -> HashMap.values state.Readers |> Seq.sum)

    /// Write-lock depth held (0 or the reentrant count). (TxReentrantLock.writeLocks)
    let writeLocks (self: TxReentrantLock) : Effect<int, 'E, 'R> =
        TxRef.atomically (TxRef.get self.StateRef)
        |> Effect.map (fun state ->
            match state.Writer with
            | Some(_, c) -> c
            | None -> 0)

    /// Whether any fiber holds a read or write lock. (TxReentrantLock.locked)
    let locked (self: TxReentrantLock) : Effect<bool, 'E, 'R> =
        TxRef.atomically (TxRef.get self.StateRef)
        |> Effect.map (fun state -> HashMap.size state.Readers > 0 || Option.isSome state.Writer)

    /// Whether any fiber holds a read lock. (TxReentrantLock.readLocked)
    let readLocked (self: TxReentrantLock) : Effect<bool, 'E, 'R> =
        TxRef.atomically (TxRef.get self.StateRef)
        |> Effect.map (fun state -> HashMap.size state.Readers > 0)

    /// Whether any fiber holds the write lock. (TxReentrantLock.writeLocked)
    let writeLocked (self: TxReentrantLock) : Effect<bool, 'E, 'R> =
        TxRef.atomically (TxRef.get self.StateRef)
        |> Effect.map (fun state -> Option.isSome state.Writer)
