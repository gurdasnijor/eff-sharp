module PartitionedSemaphoreSpec

open Effect
open Effect.Vitest

let private same actual expected = toBe (actual = expected) true

describe "PartitionedSemaphore" (fun () ->
    itEffect "module-level combinators delegate to the instance api" (fun () ->
        effect {
            let! sem = PartitionedSemaphore.make 2
            let capacity = PartitionedSemaphore.capacity sem
            let! a0 = PartitionedSemaphore.available sem
            do! PartitionedSemaphore.take sem "a" 1
            let! a1 = PartitionedSemaphore.available sem
            let! value = PartitionedSemaphore.withPermit sem "a" (Effect.succeed 1)
            let! released = PartitionedSemaphore.release sem 1
            let! value2 = PartitionedSemaphore.withPermits sem "b" 2 (Effect.succeed 2)
            let! avail = PartitionedSemaphore.withPermitsIfAvailable sem 1 (Effect.succeed "ok")
            toBe capacity 2
            toBe a0 2
            toBe a1 1
            toBe value 1
            toBe released 2
            toBe value2 2
            same avail (Some "ok")
        })

    itEffect "zero permits run immediately" (fun () ->
        let executed = ref false

        effect {
            let! sem = PartitionedSemaphore.make 1
            do! PartitionedSemaphore.withPermits sem "a" 0 (Effect.sync (fun () -> executed.Value <- true))
            let! available = PartitionedSemaphore.available sem
            toBe executed.Value true
            toBe available 1
        })

    itEffect "withPermitsIfAvailable does not block or run when unavailable" (fun () ->
        let executed = ref false

        effect {
            let! sem = PartitionedSemaphore.make 1
            do! PartitionedSemaphore.take sem "a" 1

            let! result =
                PartitionedSemaphore.withPermitsIfAvailable
                    sem
                    1
                    (Effect.sync (fun () ->
                        executed.Value <- true
                        "ok"))

            same result None
            toBe executed.Value false
        })

    itEffect "release routes a permit to a suspended waiter in another partition" (fun () ->
        effect {
            let! sem = PartitionedSemaphore.make 1
            do! PartitionedSemaphore.take sem "a" 1
            let! fiber = Effect.fork (PartitionedSemaphore.withPermit sem "b" (Effect.succeed 42))
            do! Effect.sleep 50
            let! _ = PartitionedSemaphore.release sem 1
            let! exit = Fiber.await fiber
            same exit (Exit.succeed 42)
        }))
