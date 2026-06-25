module SemaphoreSpec

open Effect
open Effect.Vitest

let private same actual expected = toBe (actual = expected) true

describe "Semaphore" (fun () ->
    itEffect "module-level combinators delegate to the instance api" (fun () ->
        effect {
            let! sem = Semaphore.make 1
            let! taken = Semaphore.take sem 1
            let! released = Semaphore.release sem 1
            do! Semaphore.resize sem 2
            let! value = Semaphore.withPermit sem (Effect.succeed 1)
            let! value2 = Semaphore.withPermits sem 2 (Effect.succeed 2)
            let! avail = Semaphore.withPermitsIfAvailable sem 1 (Effect.succeed "ok")
            let! releasedAll = Semaphore.releaseAll sem
            toBe taken 1
            toBe released 1
            toBe value 1
            toBe value2 2
            same avail (Some "ok")
            toBe releasedAll 2
        })

    itEffect "withPermits bounds concurrency" (fun () ->
        let counter = ref 0
        let maxSeen = ref 0
        let total = ref 0
        let gate = obj ()

        effect {
            let! sem = Semaphore.make 2

            let task () =
                Semaphore.withPermit
                    sem
                    (effect {
                        do!
                            Effect.sync (fun () ->
                                lock gate (fun () ->
                                    total.Value <- total.Value + 1
                                    counter.Value <- counter.Value + 1
                                    maxSeen.Value <- max maxSeen.Value counter.Value))

                        do! Effect.sleep 60
                        do! Effect.sync (fun () -> lock gate (fun () -> counter.Value <- counter.Value - 1))
                    })

            let! f1 = Effect.fork (task ())
            let! f2 = Effect.fork (task ())
            let! f3 = Effect.fork (task ())
            let! f4 = Effect.fork (task ())
            let! _ = Fiber.await f1
            let! _ = Fiber.await f2
            let! _ = Fiber.await f3
            let! _ = Fiber.await f4
            toBe (maxSeen.Value <= 2) true
            toBe counter.Value 0
            toBe total.Value 4
        })

    itEffect "take blocks until a permit is released" (fun () ->
        let log = ResizeArray<string>()
        let gate = obj ()

        effect {
            let! sem = Semaphore.make 1
            let! _ = Semaphore.take sem 1

            let! fiber =
                Effect.fork (Semaphore.withPermit sem (Effect.sync (fun () -> lock gate (fun () -> log.Add "B"))))

            do! Effect.sleep 60
            let blockedEmpty = lock gate (fun () -> log.Count = 0)
            let! _ = Semaphore.release sem 1
            let! _ = Fiber.await fiber
            toBe blockedEmpty true
            same (List.ofSeq log) [ "B" ]
        })

    itEffect "withPermitsIfAvailable does not run when unavailable" (fun () ->
        let ran = ref false

        effect {
            let! sem = Semaphore.make 1
            let! _ = Semaphore.take sem 1

            let! result =
                Semaphore.withPermitsIfAvailable
                    sem
                    1
                    (Effect.sync (fun () ->
                        ran.Value <- true
                        "x"))

            same result None
            toBe ran.Value false
        })

    itEffect "interruption releases the held permits" (fun () ->
        let ran = ref false

        effect {
            let! sem = Semaphore.make 1
            let! fiber = Effect.fork (Semaphore.withPermit sem (Effect.sleep 10000))
            do! Effect.sleep 60
            do! Fiber.interrupt fiber
            do! Semaphore.withPermit sem (Effect.sync (fun () -> ran.Value <- true))
            toBe ran.Value true
        }))
