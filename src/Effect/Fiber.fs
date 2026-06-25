namespace Effect

/// Slice: wave-4 kernel 2. The public `Fiber` module over the `Fiber<'A,'E>`
/// handle threaded by the core (`Effect.fork` produces one). The single-fiber
/// operations (`await`/`join`/`interrupt`) live on `Effect` (the core owns the
/// runtime); this module re-exposes them under the `Fiber` namespace and adds the
/// collection variants + `poll`. (Port of Fiber.ts.)
///
/// Deferred (need fiber-local references): `getCurrent`/`runIn` and the rich
/// runtime fields (currentScheduler/Span/LogLevel). The JS `isFiber` runtime guard
/// is dropped per CONVENTIONS.
[<RequireQualifiedAccess>]
module Fiber =

    /// Wait for a fiber and observe its `Exit`. (Fiber.await)
    let await (self: Fiber<'A, 'E>) : Effect<Exit<'A, 'E>, 'E2, 'R> = Effect.await self

    /// Wait for a fiber and fold its failure into this effect. (Fiber.join)
    let join (self: Fiber<'A, 'E>) : Effect<'A, 'E, 'R> = Effect.join self

    /// Interrupt a fiber and wait until it has stopped. (Fiber.interrupt)
    let interrupt (self: Fiber<'A, 'E>) : Effect<unit, 'E2, 'R> = Effect.interrupt self

    /// Await every fiber, collecting their `Exit`s in order. (Fiber.awaitAll)
    let awaitAll (fibers: seq<Fiber<'A, 'E>>) : Effect<Exit<'A, 'E> list, 'E2, 'R> = Effect.forEach fibers Effect.await

    /// Join every fiber, collecting their values; the first failure
    /// short-circuits. (Fiber.joinAll)
    let joinAll (fibers: seq<Fiber<'A, 'E>>) : Effect<'A list, 'E, 'R> = Effect.forEach fibers Effect.join

    /// Interrupt every fiber, waiting for all to stop. (Fiber.interruptAll)
    let interruptAll (fibers: seq<Fiber<'A, 'E>>) : Effect<unit, 'E2, 'R> =
        Effect.forEach fibers Effect.interrupt |> Effect.map ignore

    /// Non-blocking peek: `Some exit` if the fiber has completed, else `None`.
    /// (Fiber.pollUnsafe, lifted into an effect.)
    let poll (self: Fiber<'A, 'E>) : Effect<Exit<'A, 'E> option, 'E2, 'R> =
        Effect.sync (fun () ->
            if self.Task.IsCompleted then
                Some self.Task.Result
            else
                None)
