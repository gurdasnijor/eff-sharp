module LatchSpec

open Effect
open Effect.Vitest

describe "Latch" (fun () ->
    test "isOpen open release and close reflect synchronous state" (fun () ->
        let latch = Latch.makeUnsafe false
        toBe (Latch.isOpen latch) false
        toEqual (Effect.runSync () (Latch.``open`` latch)) (Exit.succeed true)
        toBe (Latch.isOpen latch) true
        toEqual (Effect.runSync () (Latch.``open`` latch)) (Exit.succeed false)
        toEqual (Effect.runSync () (Latch.release latch)) (Exit.succeed false)
        toEqual (Effect.runSync () (Latch.close latch)) (Exit.succeed true)
        toBe (Latch.isOpen latch) false
        toEqual (Effect.runSync () (Latch.close latch)) (Exit.succeed false))

    itEffect "await on an open latch completes immediately" (fun () ->
        let latch = Latch.makeUnsafe true
        Latch.await latch)

    itEffect "open releases a suspended waiter" (fun () ->
        effect {
            let latch = Latch.makeUnsafe false
            let! waiter = Effect.fork (Latch.await latch)
            let! before = Fiber.poll waiter
            toBe (Option.isNone before) true

            let! changed = Latch.``open`` latch
            toBe changed true

            let! exit = Fiber.await waiter
            toEqual exit (Exit.succeed ())
        })

    itEffect "release wakes current waiters and keeps the latch closed" (fun () ->
        effect {
            let latch = Latch.makeUnsafe false
            let! waiter = Effect.fork (Latch.await latch)
            let! before = Fiber.poll waiter
            toBe (Option.isNone before) true

            let! released = Latch.release latch
            toBe released true
            let! exit = Fiber.await waiter
            toEqual exit (Exit.succeed ())
            toBe (Latch.isOpen latch) false

            let! waiter2 = Effect.fork (Latch.await latch)
            let! after = Fiber.poll waiter2
            toBe (Option.isNone after) true
            Latch.openUnsafe latch |> ignore
            let! exit2 = Fiber.await waiter2
            toEqual exit2 (Exit.succeed ())
        })

    itEffect "whenOpen runs immediately when open and gates while closed" (fun () ->
        effect {
            let openLatch = Latch.makeUnsafe true
            let! immediate = Latch.whenOpen openLatch (Effect.succeed 42)
            toBe immediate 42

            let closedLatch = Latch.makeUnsafe false
            let! waiter = Effect.fork (Latch.whenOpen closedLatch (Effect.succeed 7))
            let! before = Fiber.poll waiter
            toBe (Option.isNone before) true

            Latch.openUnsafe closedLatch |> ignore
            let! exit = Fiber.await waiter
            toEqual exit (Exit.succeed 7)
        }))
