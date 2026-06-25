namespace Effect

/// `FiberHandle` — a scoped container holding at most one fiber.
///
/// Port of repos/effect-smol/packages/effect/src/FiberHandle.ts. Installing a
/// new fiber (`run`/`set`) interrupts the one it replaces (unless
/// `onlyIfMissing`); closing the owning scope interrupts the current fiber.
///
/// Decoupling / omissions (per CONVENTIONS), driven by the simplified fiber
/// kernel:
///   * The owning `Scope` is passed *explicitly* to `make` (the port's `Scope`
///     is not an environment requirement). `make` registers a scope finalizer
///     that interrupts the current fiber and closes the handle.
///   * Interruption is *cooperative* (honored at the port's interruption points,
///     e.g. `Effect.sleep`); `setUnsafe` flips a replaced fiber's interrupt flag
///     synchronously, while the effectful `run`/`clear` additionally await the
///     replaced/cleared fiber so callers observe its finalizers.
///   * A completed fiber is treated as absent (`getUnsafe`), standing in for
///     upstream's `addObserver` auto-removal (the kernel has no observers).
///   * `join` waits for the current fiber and surfaces its first typed failure.
///   * Dropped: `makeRuntime`/`runtime`/`*Promise` (need `runForkWith`/the JS
///     runtime), `propagateInterruption`, the `deferred` field, `TypeId`.
/// A handle managing at most one fiber.
type FiberHandle<'A, 'E> =
    internal
        { mutable Current: Fiber<'A, 'E> option
          mutable Closed: bool
          Gate: obj }

[<RequireQualifiedAccess>]
module FiberHandle =

    /// Set a fiber's interrupt flag without awaiting it.
    let private signal (fiber: Fiber<'A, 'E>) : unit = fiber.Runtime.Interrupted <- true

    let private isLive (fiber: Fiber<'A, 'E>) : bool = not fiber.Task.IsCompleted

    /// Create an empty handle outside the `Effect` context. (FiberHandle.makeUnsafe)
    let makeUnsafe<'A, 'E> () : FiberHandle<'A, 'E> =
        { Current = None
          Closed = false
          Gate = System.Object() }

    /// Interrupt the current fiber and close the handle (the scope finalizer).
    let private closeEffect (self: FiberHandle<'A, 'E>) : Effect<unit, 'E2, 'R> =
        Effect.suspend (fun () ->
            let cur =
                lock self.Gate (fun () ->
                    self.Closed <- true
                    let c = self.Current
                    self.Current <- None
                    c)

            match cur with
            | Some f -> Effect.interrupt f
            | None -> Effect.succeed ())

    /// Create a scoped handle; its current fiber is interrupted when `parent`
    /// closes. (FiberHandle.make)
    let make (parent: Scope<'E2, 'R>) : Effect<FiberHandle<'A, 'E>, 'E2, 'R> =
        Effect.suspend (fun () ->
            let self = makeUnsafe<'A, 'E> ()
            Scope.addFinalizer parent (closeEffect self) |> Effect.map (fun () -> self))

    /// The current (live) fiber, or `None`. (FiberHandle.getUnsafe)
    let getUnsafe (self: FiberHandle<'A, 'E>) : Fiber<'A, 'E> option =
        lock self.Gate (fun () ->
            match self.Current with
            | Some f when not self.Closed && isLive f -> Some f
            | _ -> None)

    /// The current (live) fiber, or `None`. (FiberHandle.get)
    let get (self: FiberHandle<'A, 'E>) : Effect<Fiber<'A, 'E> option, 'E2, 'R> = Effect.sync (fun () -> getUnsafe self)

    /// Install `fiber`, interrupting (flag-only) the one it replaces unless
    /// `onlyIfMissing`. (FiberHandle.setUnsafe)
    let setUnsafe (self: FiberHandle<'A, 'E>) (onlyIfMissing: bool) (fiber: Fiber<'A, 'E>) : unit =
        lock self.Gate (fun () ->
            if self.Closed then
                signal fiber
            else
                match self.Current with
                | Some old when isLive old ->
                    if onlyIfMissing then
                        signal fiber
                    elif not (System.Object.ReferenceEquals(old, fiber)) then
                        signal old
                        self.Current <- Some fiber
                | _ -> self.Current <- Some fiber)

    /// Fork `effect` into the handle, interrupting (and awaiting) the previous
    /// fiber unless `onlyIfMissing`. (FiberHandle.run)
    let runWith
        (self: FiberHandle<'A, 'E>)
        (onlyIfMissing: bool)
        (effect: Effect<'A, 'E, 'R>)
        : Effect<Fiber<'A, 'E>, 'E2, 'R> =
        Effect.fork effect
        |> Effect.flatMap (fun fiber ->
            let previous =
                lock self.Gate (fun () ->
                    if self.Closed then
                        Some fiber // interrupt the just-forked fiber
                    else
                        match self.Current with
                        | Some old when isLive old ->
                            if onlyIfMissing then
                                Some fiber
                            else
                                self.Current <- Some fiber
                                Some old
                        | _ ->
                            self.Current <- Some fiber
                            None)

            match previous with
            | None -> Effect.succeed fiber
            | Some toInterrupt -> Effect.interrupt toInterrupt |> Effect.map (fun () -> fiber))

    /// Fork `effect` into the handle, replacing/interrupting the previous fiber.
    let run (self: FiberHandle<'A, 'E>) (effect: Effect<'A, 'E, 'R>) : Effect<Fiber<'A, 'E>, 'E2, 'R> =
        runWith self false effect

    /// Interrupt the current fiber and empty the handle. (FiberHandle.clear)
    let clear (self: FiberHandle<'A, 'E>) : Effect<unit, 'E2, 'R> =
        Effect.suspend (fun () ->
            let cur =
                lock self.Gate (fun () ->
                    let c = self.Current
                    self.Current <- None
                    c)

            match cur with
            | Some f -> Effect.interrupt f
            | None -> Effect.succeed ())

    /// Wait for the current fiber to complete. (FiberHandle.awaitEmpty)
    let awaitEmpty (self: FiberHandle<'A, 'E>) : Effect<unit, 'E2, 'R> =
        Effect.suspend (fun () ->
            match getUnsafe self with
            | Some f -> Effect.await f |> Effect.map ignore
            | None -> Effect.succeed ())

    /// Wait for the current fiber and surface its first typed failure.
    /// (FiberHandle.join)
    let join (self: FiberHandle<'A, 'E>) : Effect<unit, 'E, 'R> =
        Effect.suspend (fun () ->
            // Read `Current` directly (not `getUnsafe`): a just-failed fiber is
            // already completed but still carries the failure to surface.
            match lock self.Gate (fun () -> self.Current) with
            | None -> Effect.succeed ()
            | Some f ->
                Effect.await f
                |> Effect.flatMap (fun exit ->
                    match exit with
                    | Failure cause ->
                        match Cause.failures cause with
                        | e :: _ -> Effect.fail e
                        | [] -> Effect.succeed ()
                    | Success _ -> Effect.succeed ()))
