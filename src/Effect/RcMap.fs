namespace Effect

/// `RcMap` — a reference-counted map of scoped resources.
///
/// Port of repos/effect-smol/packages/effect/src/RcMap.ts. An `RcMap` runs a
/// `lookup` effect the first time a key is requested, shares the acquired
/// resource with later callers for the same key, and tracks each caller through
/// its `Scope`. When the last reference for a key is released the resource is
/// released — immediately, after an idle time-to-live, or never (kept alive),
/// and capacity limits / `invalidate` can evict eagerly.
///
/// **Idiomatic adaptation to this port's kernel (NOT a transliteration).**
/// Upstream reads the caller's `Scope` and the current `Clock` from the ambient
/// fiber (`Effect.withFiber` / `Scope` ∈ `R`) and forks lookups with
/// `runForkWith` / `Fiber.runIn`. This kernel has none of that machinery, so:
///   * `get`/`make` take the relevant `Scope` **explicitly** (this repo's `Scope`
///     is already an explicit value, not an ambient `R` capability).
///   * The `lookup` is `key -> entryScope -> Effect<'A,'E,unit>`: it acquires the
///     resource into the entry's scope (typically via `Scope.acquireRelease`),
///     mirroring upstream's `lookup: key -> Effect<A, E, Scope>`.
///   * Idle-TTL eviction uses **real time** (the live `Clock` + `Effect.sleep`)
///     and a forked timer fiber — there is no `TestClock` to drive virtual time,
///     so the ported tests use small real delays with generous margins.
///   * Capacity-exceeded is surfaced as a **defect** carrying `ExceededCapacityError`
///     rather than a second typed-error arm (`E | ExceededCapacityError`), since
///     this port has no row-typed error channel. Observably it is still an
///     `Exit` failure carrying the error.
///   * Shared state is guarded by a lock (`Gate`); upstream relies on JS's single
///     thread + `uninterruptibleMask`, but .NET fibers run on real threads.
///
/// Omissions vs upstream (per CONVENTIONS): the `TypeId` brand, `Pipeable`, and
/// the HKT/`dual` plumbing are dropped; environment is fixed to `unit`.
type ExceededCapacityError(message: string) =
    inherit System.Exception(message)

/// One stored resource and its reference-counting / idle-lifecycle metadata.
type internal RcMapEntry<'A, 'E> =
    { Deferred: Deferred<'A, 'E>
      Scope: Scope<'E, unit>
      IdleTimeToLive: Duration
      mutable Fiber: Fiber<unit, 'E> option
      mutable ExpiresAt: int64
      mutable RefCount: int }

type internal RcMapState<'K, 'A, 'E when 'K: equality> =
    | Open of MutableHashMap<'K, RcMapEntry<'A, 'E>>
    | Closed

/// A reference-counted map of scoped resources.
type RcMap<'K, 'A, 'E when 'K: equality> =
    internal
        {
            Lookup: 'K -> Scope<'E, unit> -> Effect<'A, 'E, unit>
            IdleTimeToLive: 'K -> Duration
            /// `None` means unbounded capacity.
            Capacity: int option
            Clock: Clock
            Gate: obj
            mutable State: RcMapState<'K, 'A, 'E>
        }

[<RequireQualifiedAccess>]
module RcMap =

    /// Effect that completes as interrupted — returned by operations on a closed
    /// map (upstream returns `Effect.interrupt`).
    let private interruptedEffect<'A, 'E> : Effect<'A, 'E, unit> =
        Effect.failCause (Cause.interrupt None)

    let private closeEntry (entry: RcMapEntry<'A, 'E>) : Effect<unit, 'E, unit> =
        Scope.close entry.Scope (Success(box ()))

    let private interruptTimer (entry: RcMapEntry<'A, 'E>) : Effect<unit, 'E, unit> =
        match entry.Fiber with
        | Some f -> Effect.interrupt f
        | None -> Effect.succeed ()

    // The decision taken (under lock) when a reference is released.
    type private ReleaseAction =
        | NoOp
        | Close
        | Schedule

    /// Decrement a key's reference count; when it reaches zero, release now (TTL
    /// zero / already evicted), keep forever (infinite TTL), or schedule a timed
    /// eviction. Registered as a finalizer on each caller scope. (RcMap.release)
    let rec private release (self: RcMap<'K, 'A, 'E>) (key: 'K) (entry: RcMapEntry<'A, 'E>) : Effect<unit, 'E, unit> =
        Effect.suspend (fun () ->
            let action =
                lock self.Gate (fun () ->
                    entry.RefCount <- entry.RefCount - 1

                    if entry.RefCount > 0 then
                        NoOp
                    else
                        match self.State with
                        | Closed -> Close
                        | Open map ->
                            if not (MutableHashMap.has map key) || Duration.isZero entry.IdleTimeToLive then
                                MutableHashMap.remove map key |> ignore
                                Close
                            elif not (Duration.isFinite entry.IdleTimeToLive) then
                                NoOp
                            else
                                entry.ExpiresAt <-
                                    self.Clock.CurrentTimeMillisUnsafe()
                                    + int64 (Duration.toMillis entry.IdleTimeToLive)

                                if entry.Fiber.IsSome then NoOp else Schedule)

            match action with
            | NoOp -> Effect.succeed ()
            | Close -> closeEntry entry
            | Schedule ->
                let timer: Effect<unit, 'E, unit> =
                    evictionLoop self key entry
                    |> Effect.ensuring (Effect.sync (fun () -> lock self.Gate (fun () -> entry.Fiber <- None)))

                Effect.fork timer
                |> Effect.flatMap (fun fib ->
                    Effect.sync (fun () -> lock self.Gate (fun () -> entry.Fiber <- Some fib))))

    /// The timed-eviction loop: sleep until the entry expires, re-checking
    /// `ExpiresAt` each wake (so `touch` can extend it), then close the resource
    /// if it is still idle. (RcMap.release inner loop)
    and private evictionLoop (self: RcMap<'K, 'A, 'E>) (key: 'K) (entry: RcMapEntry<'A, 'E>) : Effect<unit, 'E, unit> =
        Effect.suspend (fun () ->
            let now = self.Clock.CurrentTimeMillisUnsafe()
            let remaining = entry.ExpiresAt - now

            if remaining <= 0L then
                let shouldClose =
                    lock self.Gate (fun () ->
                        match self.State with
                        | Closed -> false
                        | Open map ->
                            if entry.RefCount > 0 then
                                false
                            else
                                MutableHashMap.remove map key |> ignore
                                true)

                if shouldClose then closeEntry entry else Effect.succeed ()
            else
                clockSleep self.Clock remaining |> Effect.zipRight (evictionLoop self key entry))

    /// Sleep `millis` against the map's `Clock` (the live clock in production, a
    /// `TestClock` under test), staying cooperatively interruptible so a closing
    /// map / `invalidate` can stop a parked timer. Mirrors the kernel's
    /// `Effect.sleep` loop, but waits on the `Clock`'s task so virtual time
    /// controls eviction.
    and private clockSleep (clock: Clock) (millis: int64) : Effect<unit, 'E, unit> =
        Effect(fun fib _ ->
            async {
                let task = clock.SleepUnsafe(Duration.millis (float millis))
                let mutable interrupted = false

                while not task.IsCompleted && not interrupted do
                    if fib.Interrupted && fib.Mask = 0 then
                        interrupted <- true
                    else
                        do! Async.Sleep 5

                return
                    if interrupted && not task.IsCompleted then
                        Failure(Cause.interrupt None)
                    else
                        Success()
            })

    /// Close every resource and mark the map closed; registered as a finalizer on
    /// the map's owning scope by `make`. (RcMap.make finalizer)
    let private closeAll (self: RcMap<'K, 'A, 'E>) : Effect<unit, 'E, unit> =
        Effect.suspend (fun () ->
            let entries =
                lock self.Gate (fun () ->
                    match self.State with
                    | Closed -> []
                    | Open map ->
                        let es = MutableHashMap.values map |> List.ofSeq
                        self.State <- Closed
                        MutableHashMap.clear map |> ignore
                        es)

            Effect.forEach entries (fun entry -> interruptTimer entry |> Effect.zipRight (closeEntry entry))
            |> Effect.map ignore)

    /// Create an `RcMap` whose resources live no longer than the map's `scope`.
    /// `capacity = None` is unbounded; `idleTimeToLive key` gives the per-key idle
    /// grace period (`Duration.zero` = release immediately at refcount 0). (RcMap.make)
    let makeWithClock
        (clock: Clock)
        (scope: Scope<'E, unit>)
        (capacity: int option)
        (idleTimeToLive: 'K -> Duration)
        (lookup: 'K -> Scope<'E, unit> -> Effect<'A, 'E, unit>)
        : Effect<RcMap<'K, 'A, 'E>, 'E, unit> =
        Effect.suspend (fun () ->
            let self =
                { Lookup = lookup
                  IdleTimeToLive = idleTimeToLive
                  Capacity = capacity
                  Clock = clock
                  Gate = System.Object()
                  State = Open(MutableHashMap.empty ()) }

            Scope.addFinalizer scope (closeAll self) |> Effect.map (fun () -> self))

    /// `makeWithClock` using the live wall-clock. The `Clock` is captured
    /// explicitly (this kernel's `RcMap` ops are `Effect<_,_,unit>` — there is no
    /// ambient `Context` to read from, unlike `Cache`); a `TestClock` is injected
    /// via `makeWithClock` to make idle-TTL eviction deterministic. (RcMap.make)
    let makeWith
        (scope: Scope<'E, unit>)
        (capacity: int option)
        (idleTimeToLive: 'K -> Duration)
        (lookup: 'K -> Scope<'E, unit> -> Effect<'A, 'E, unit>)
        : Effect<RcMap<'K, 'A, 'E>, 'E, unit> =
        makeWithClock (Clock.make ()) scope capacity idleTimeToLive lookup

    /// `makeWith` with no capacity limit and immediate release at refcount 0. (RcMap.make)
    let make
        (scope: Scope<'E, unit>)
        (lookup: 'K -> Scope<'E, unit> -> Effect<'A, 'E, unit>)
        : Effect<RcMap<'K, 'A, 'E>, 'E, unit> =
        makeWith scope None (fun _ -> Duration.zero) lookup

    /// Acquire (or retain) the resource for `key`, registering its release on
    /// `callerScope`. The resource is acquired once and shared; it is released
    /// when the last caller scope closes (subject to idle TTL). (RcMap.get)
    let get (self: RcMap<'K, 'A, 'E>) (key: 'K) (callerScope: Scope<'E, unit>) : Effect<'A, 'E, unit> =
        Effect.uninterruptible (
            Effect.suspend (fun () ->
                // Synchronous bookkeeping under the lock; returns either a terminal
                // effect (closed / over capacity) or the (entry, isNew) to continue.
                let decision =
                    lock self.Gate (fun () ->
                        match self.State with
                        | Closed -> Error interruptedEffect
                        | Open map ->
                            match MutableHashMap.get map key with
                            | Some entry ->
                                entry.RefCount <- entry.RefCount + 1
                                Ok(entry, false)
                            | None ->
                                match self.Capacity with
                                | Some cap when MutableHashMap.size map >= cap ->
                                    Error(
                                        Effect.failCause (
                                            Cause.die (
                                                box (
                                                    ExceededCapacityError(
                                                        sprintf "RcMap attempted to exceed capacity of %d" cap
                                                    )
                                                )
                                            )
                                        )
                                    )
                                | _ ->
                                    let entry =
                                        { Deferred = Deferred.makeUnsafe ()
                                          Scope = Scope.make ()
                                          IdleTimeToLive = self.IdleTimeToLive key
                                          Fiber = None
                                          ExpiresAt = 0L
                                          RefCount = 1 }

                                    MutableHashMap.set map key entry |> ignore
                                    Ok(entry, true))

                match decision with
                | Error terminal -> terminal
                | Ok(entry, isNew) ->
                    // Only the first caller runs the lookup (acquiring into the
                    // entry's scope) and completes the shared Deferred.
                    let acquire: Effect<unit, 'E, unit> =
                        if isNew then
                            Deferred.complete entry.Deferred (self.Lookup key entry.Scope)
                            |> Effect.map ignore
                        else
                            Effect.succeed ()

                    acquire
                    |> Effect.zipRight (Scope.addFinalizer callerScope (release self key entry))
                    |> Effect.zipRight (Deferred.await entry.Deferred))
        )

    /// All keys currently stored. Interrupts if the map is closed. (RcMap.keys)
    let keys (self: RcMap<'K, 'A, 'E>) : Effect<'K list, 'E, unit> =
        Effect.suspend (fun () ->
            lock self.Gate (fun () ->
                match self.State with
                | Closed -> interruptedEffect
                | Open map -> Effect.succeed (MutableHashMap.keys map |> List.ofSeq)))

    /// Whether a key is currently present. A closed map returns `false`. (RcMap.has)
    let has (self: RcMap<'K, 'A, 'E>) (key: 'K) : Effect<bool, 'E, unit> =
        Effect.sync (fun () ->
            lock self.Gate (fun () ->
                match self.State with
                | Closed -> false
                | Open map -> MutableHashMap.has map key))

    /// Remove a key from the map; if it is not in use (refcount 0) release it
    /// immediately, otherwise it is released when its last reference closes. (RcMap.invalidate)
    let invalidate (self: RcMap<'K, 'A, 'E>) (key: 'K) : Effect<unit, 'E, unit> =
        Effect.uninterruptible (
            Effect.suspend (fun () ->
                let toClose =
                    lock self.Gate (fun () ->
                        match self.State with
                        | Closed -> None
                        | Open map ->
                            match MutableHashMap.get map key with
                            | None -> None
                            | Some entry ->
                                MutableHashMap.remove map key |> ignore
                                if entry.RefCount > 0 then None else Some entry)

                match toClose with
                | None -> Effect.succeed ()
                | Some entry -> interruptTimer entry |> Effect.zipRight (closeEntry entry))
        )

    /// Reset a key's idle timer, keeping it alive for another `idleTimeToLive`.
    /// No-op for absent keys, closed maps, or zero TTL. (RcMap.touch)
    let touch (self: RcMap<'K, 'A, 'E>) (key: 'K) : Effect<unit, 'E, unit> =
        Effect.sync (fun () ->
            lock self.Gate (fun () ->
                match self.State with
                | Closed -> ()
                | Open map ->
                    match MutableHashMap.get map key with
                    | Some entry when not (Duration.isZero entry.IdleTimeToLive) ->
                        entry.ExpiresAt <-
                            self.Clock.CurrentTimeMillisUnsafe()
                            + int64 (Duration.toMillis entry.IdleTimeToLive)
                    | _ -> ()))
