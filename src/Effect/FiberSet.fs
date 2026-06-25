namespace Effect

/// `FiberSet` — a scoped, unkeyed set of fibers.
///
/// Port of repos/effect-smol/packages/effect/src/FiberSet.ts. Fibers added with
/// `add`/`run` are supervised together: closing the owning scope (or `clear`)
/// interrupts every member, and completed fibers are dropped from the set.
///
/// Decoupling / omissions (per CONVENTIONS), driven by the simplified fiber
/// kernel: the owning `Scope` is passed *explicitly* to `make` (which registers
/// a finalizer interrupting all members); interruption is cooperative; completed
/// fibers are pruned lazily on read (no `addObserver`); `join` awaits all members
/// and surfaces the first typed failure. Dropped: `runtime`/`*Promise`,
/// `propagateInterruption`, the `deferred` field, `TypeId`.
/// A supervised set of fibers.
type FiberSet<'A, 'E> =
    internal
        { Fibers: ResizeArray<Fiber<'A, 'E>>
          mutable Closed: bool
          Gate: obj }

[<RequireQualifiedAccess>]
module FiberSet =

#if FABLE_COMPILER
    // Fable shim: no backing Task; "live" = completion cell not yet set. (refactor)
    let private isLive (fiber: Fiber<'A, 'E>) : bool = Option.isNone fiber.Result.Value
#else
    let private isLive (fiber: Fiber<'A, 'E>) : bool = not fiber.Task.IsCompleted
#endif

    /// Drop completed fibers; returns the live snapshot (caller holds the lock).
    let private prune (self: FiberSet<'A, 'E>) : Fiber<'A, 'E> list =
        self.Fibers.RemoveAll(fun f -> not (isLive f)) |> ignore
        List.ofSeq self.Fibers

    let private interruptAll (fibers: Fiber<'A, 'E> list) : Effect<unit, 'E2, 'R> =
        Effect.forEach fibers Effect.interrupt |> Effect.map ignore

    /// Create an empty set outside the `Effect` context. (FiberSet.makeUnsafe)
    let makeUnsafe<'A, 'E> () : FiberSet<'A, 'E> =
        { Fibers = ResizeArray()
          Closed = false
          Gate = System.Object() }

    let private closeEffect (self: FiberSet<'A, 'E>) : Effect<unit, 'E2, 'R> =
        Effect.suspend (fun () ->
            let live =
                lock self.Gate (fun () ->
                    self.Closed <- true
                    let snapshot = List.ofSeq self.Fibers
                    self.Fibers.Clear()
                    snapshot |> List.filter isLive)

            interruptAll live)

    /// Create a scoped set; all members are interrupted when `parent` closes.
    /// (FiberSet.make)
    let make (parent: Scope<'E2, 'R>) : Effect<FiberSet<'A, 'E>, 'E2, 'R> =
        Effect.suspend (fun () ->
            let self = makeUnsafe<'A, 'E> ()
            Scope.addFinalizer parent (closeEffect self) |> Effect.map (fun () -> self))

    /// Add a fiber to the set (interrupting it if the set is closed).
    /// (FiberSet.addUnsafe)
    let addUnsafe (self: FiberSet<'A, 'E>) (fiber: Fiber<'A, 'E>) : unit =
        lock self.Gate (fun () ->
            if self.Closed then
                fiber.Runtime.Interrupted <- true
            else
                prune self |> ignore
                self.Fibers.Add fiber)

    /// Fork `effect` into the set, returning its fiber. (FiberSet.run)
    let run (self: FiberSet<'A, 'E>) (effect: Effect<'A, 'E, 'R>) : Effect<Fiber<'A, 'E>, 'E2, 'R> =
        Effect.fork effect
        |> Effect.map (fun fiber ->
            addUnsafe self fiber
            fiber)

    /// The number of live fibers. (FiberSet.size)
    let size (self: FiberSet<'A, 'E>) : Effect<int, 'E2, 'R> =
        Effect.sync (fun () -> lock self.Gate (fun () -> prune self |> List.length))

    /// Interrupt every member and empty the set. (FiberSet.clear)
    let clear (self: FiberSet<'A, 'E>) : Effect<unit, 'E2, 'R> =
        Effect.suspend (fun () ->
            let live =
                lock self.Gate (fun () ->
                    let snapshot = List.ofSeq self.Fibers |> List.filter isLive
                    self.Fibers.Clear()
                    snapshot)

            interruptAll live)

    /// Wait for every current member to complete. (FiberSet.awaitEmpty)
    let awaitEmpty (self: FiberSet<'A, 'E>) : Effect<unit, 'E2, 'R> =
        Effect.suspend (fun () ->
            let live = lock self.Gate (fun () -> prune self)
            Effect.forEach live Effect.await |> Effect.map ignore)

    /// Wait for all members and surface the first typed failure. (FiberSet.join)
    let join (self: FiberSet<'A, 'E>) : Effect<unit, 'E, 'R> =
        Effect.suspend (fun () ->
            // Await all members (including just-completed ones, which may carry a
            // failure to surface); do not prune first.
            let all = lock self.Gate (fun () -> List.ofSeq self.Fibers)

            Effect.forEach all Effect.await
            |> Effect.flatMap (fun exits ->
                let firstError =
                    exits
                    |> List.tryPick (fun exit ->
                        match exit with
                        | Failure cause -> Cause.failures cause |> List.tryHead
                        | Success _ -> None)

                match firstError with
                | Some e -> Effect.fail e
                | None -> Effect.succeed ()))
