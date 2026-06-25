module FiberSpec

open Effect
open Effect.Vitest

let private same actual expected = toBe (actual = expected) true

describe "Fiber" (fun () ->
    itEffect "fork then join returns the fiber's value" (fun () ->
        effect {
            let! fiber = Effect.fork (Effect.succeed 42)
            let! value = Fiber.join fiber
            toBe value 42
        })

    itEffect "fork then await observes the fiber's Exit" (fun () ->
        effect {
            let! fiber = Effect.fork (Effect.fail "boom")
            let! exit = Fiber.await fiber
            same exit (Exit.fail "boom")
        })

    itEffect "interrupt stops a sleeping fiber and runs its finalizer" (fun () ->
        let log = ResizeArray<string>()

        effect {
            let! fiber =
                Effect.fork (Effect.ensuring (Effect.sync (fun () -> log.Add "released")) (Effect.sleep 5000))

            do! Effect.sleep 50
            do! Fiber.interrupt fiber
            let! childExit = Fiber.await fiber
            toBe (Exit.isFailure childExit) true
            toBe (System.String.Join(",", log)) "released"
        })

    itEffect "uninterruptible region completes and then honors interruption" (fun () ->
        let log = ResizeArray<string>()

        effect {
            let! entered = Deferred.make ()
            let! release = Deferred.make ()

            let masked: Effect<unit, string, Context> =
                Effect.uninterruptible (
                    effect {
                        do! Deferred.succeed entered () |> Effect.map ignore
                        do! Deferred.await release
                        do! Effect.sync (fun () -> log.Add "done")
                    }
                )

            let! fiber = Effect.fork masked
            do! Deferred.await entered
            do! Fiber.interruptFork fiber
            do! Deferred.succeed release () |> Effect.map ignore
            let! childExit = Fiber.await fiber
            toBe (System.String.Join(",", log)) "done"
            toBe (Exit.isFailure childExit) true
        }))
