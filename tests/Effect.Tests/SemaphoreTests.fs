module Effect.Tests.SemaphoreTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/Semaphore.test.ts.
//
// Most of the upstream file actually exercises `PartitionedSemaphore` (ported in
// wave 4) via `TestClock`, and one case needs a custom `Scheduler`; neither is
// available in this runtime. The directly-portable `Semaphore` case is
// "module-level combinators". The remaining cases here exercise the same
// contract — concurrency bounding, FIFO blocking/release, immediate-availability,
// and permit release on interruption — with the merged `Effect.fork` + `sleep`.

let private run (eff: Effect<'A, string, unit>) : 'A =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure c -> failwithf "unexpected failure: %s" (Cause.render c)

[<Fact>]
let ``module-level combinators delegate to the instance api`` () =
    let eff: Effect<int * int * int * int * string option * int * int * string option * int, string, unit> =
        effect {
            let! sem = Semaphore.make 1
            let! taken = Semaphore.take sem 1
            let! released = Semaphore.release sem 1
            do! Semaphore.resize sem 2
            let! value = Semaphore.withPermit sem (Effect.succeed 1)
            let! value2 = Semaphore.withPermits sem 2 (Effect.succeed 2)
            let! avail = Semaphore.withPermitsIfAvailable sem 1 (Effect.succeed "ok")
            let! piped = Effect.succeed 3 |> Semaphore.withPermit sem
            let! piped2 = Effect.succeed 4 |> Semaphore.withPermits sem 1
            let! pipedAvail = Effect.succeed "pipe" |> Semaphore.withPermitsIfAvailable sem 1
            let! releasedAll = Semaphore.releaseAll sem
            return (taken, released, value, value2, avail, piped, piped2, pipedAvail, releasedAll)
        }

    let taken, released, value, value2, avail, piped, piped2, pipedAvail, releasedAll = run eff
    Assert.Equal(1, taken)
    Assert.Equal(1, released)
    Assert.Equal(1, value)
    Assert.Equal(2, value2)
    Assert.Equal(Some "ok", avail)
    Assert.Equal(3, piped)
    Assert.Equal(4, piped2)
    Assert.Equal(Some "pipe", pipedAvail)
    Assert.Equal(2, releasedAll)

[<Fact>]
let ``withPermits bounds concurrency`` () =
    let counter = ref 0
    let maxSeen = ref 0
    let total = ref 0
    let l = obj ()

    let eff: Effect<int * int * int, string, unit> =
        effect {
            let! sem = Semaphore.make 2

            let task () =
                Semaphore.withPermit
                    sem
                    (effect {
                        do!
                            Effect.sync (fun () ->
                                lock l (fun () ->
                                    total.Value <- total.Value + 1
                                    counter.Value <- counter.Value + 1
                                    maxSeen.Value <- max maxSeen.Value counter.Value))

                        do! Effect.sleep 60
                        do! Effect.sync (fun () -> lock l (fun () -> counter.Value <- counter.Value - 1))
                    })

            let! f1 = Effect.fork (task ())
            let! f2 = Effect.fork (task ())
            let! f3 = Effect.fork (task ())
            let! f4 = Effect.fork (task ())
            let! _ = Effect.await f1
            let! _ = Effect.await f2
            let! _ = Effect.await f3
            let! _ = Effect.await f4
            return (maxSeen.Value, counter.Value, total.Value)
        }

    let maxSeen, counter, total = run eff
    Assert.True(maxSeen <= 2, sprintf "max concurrency was %d, expected <= 2" maxSeen)
    Assert.Equal(0, counter) // every permit released
    Assert.Equal(4, total) // every task ran

[<Fact>]
let ``take blocks until a permit is released`` () =
    let log = System.Collections.Generic.List<string>()
    let ll = obj ()

    let eff: Effect<bool * string list, string, unit> =
        effect {
            let! sem = Semaphore.make 1
            let! _ = Semaphore.take sem 1 // hold the only permit
            let! fiber = Effect.fork (Semaphore.withPermit sem (Effect.sync (fun () -> lock ll (fun () -> log.Add "B"))))
            do! Effect.sleep 60
            let blockedEmpty = lock ll (fun () -> log.Count = 0)
            let! _ = Semaphore.release sem 1
            let! _ = Effect.await fiber
            return (blockedEmpty, List.ofSeq log)
        }

    let blockedEmpty, entries = run eff
    Assert.True(blockedEmpty) // stayed blocked while the permit was held
    Assert.Equal<string list>([ "B" ], entries)

[<Fact>]
let ``withPermitsIfAvailable does not run when unavailable`` () =
    let ran = ref false

    let eff: Effect<string option, string, unit> =
        effect {
            let! sem = Semaphore.make 1
            let! _ = Semaphore.take sem 1 // exhaust permits

            return!
                Semaphore.withPermitsIfAvailable
                    sem
                    1
                    (Effect.sync (fun () ->
                        ran.Value <- true
                        "x"))
        }

    Assert.Equal(None, run eff)
    Assert.False(ran.Value)

[<Fact>]
let ``interruption releases the held permits`` () =
    let ran = ref false

    let eff: Effect<bool, string, unit> =
        effect {
            let! sem = Semaphore.make 1
            let! fiber = Effect.fork (Semaphore.withPermit sem (Effect.sleep 10000))
            do! Effect.sleep 60
            do! Effect.interrupt fiber
            // The permit was released by withPermit's finalizer on interrupt.
            do! Semaphore.withPermit sem (Effect.sync (fun () -> ran.Value <- true))
            return ran.Value
        }

    Assert.True(run eff)
