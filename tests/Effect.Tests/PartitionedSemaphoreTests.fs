module Effect.Tests.PartitionedSemaphoreTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/PartitionedSemaphore.test.ts.
// The upstream `Effect.gen` cases become `effect { }`; one extra fork-based case
// exercises the suspend-then-release path the upstream unit tests don't cover.

let private run (eff: Effect<'A, string, unit>) : 'A =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure c -> failwithf "unexpected failure: %s" (Cause.render c)

[<Fact>]
let ``module-level combinators delegate to the instance api`` () =
    let eff: Effect<int * int * int * int * int * int * string option, string, unit> =
        effect {
            let! sem = PartitionedSemaphore.make 2
            let cap = PartitionedSemaphore.capacity sem
            let! a0 = PartitionedSemaphore.available sem
            do! PartitionedSemaphore.take sem "a" 1
            let! a1 = PartitionedSemaphore.available sem
            let! v = PartitionedSemaphore.withPermit sem "a" (Effect.succeed 1)
            let! released = PartitionedSemaphore.release sem 1
            let! v2 = PartitionedSemaphore.withPermits sem "b" 2 (Effect.succeed 2)
            let! avail = PartitionedSemaphore.withPermitsIfAvailable sem 1 (Effect.succeed "ok")
            return (cap, a0, a1, v, released, v2, avail)
        }

    let cap, a0, a1, v, released, v2, avail = run eff
    Assert.Equal(2, cap)
    Assert.Equal(2, a0)
    Assert.Equal(1, a1)
    Assert.Equal(1, v)
    Assert.Equal(2, released)
    Assert.Equal(2, v2)
    Assert.Equal(Some "ok", avail)

[<Fact>]
let ``zero permits run immediately`` () =
    let executed = ref false

    let eff: Effect<int, string, unit> =
        effect {
            let! sem = PartitionedSemaphore.make 1
            do! PartitionedSemaphore.withPermits sem "a" 0 (Effect.sync (fun () -> executed.Value <- true))
            return! PartitionedSemaphore.available sem
        }

    let avail = run eff
    Assert.True(executed.Value)
    Assert.Equal(1, avail)

[<Fact>]
let ``withPermitsIfAvailable does not block or run when unavailable`` () =
    let executed = ref false

    let eff: Effect<string option, string, unit> =
        effect {
            let! sem = PartitionedSemaphore.make 1
            do! PartitionedSemaphore.take sem "a" 1

            return!
                PartitionedSemaphore.withPermitsIfAvailable
                    sem
                    1
                    (Effect.sync (fun () ->
                        executed.Value <- true
                        "ok"))
        }

    Assert.Equal(None, run eff)
    Assert.False(executed.Value)

[<Fact>]
let ``release routes a permit to a suspended waiter in another partition`` () =
    let eff: Effect<Exit<int, string>, string, unit> =
        effect {
            let! sem = PartitionedSemaphore.make 1
            do! PartitionedSemaphore.take sem "a" 1 // pool now empty
            let! fiber = Effect.fork (PartitionedSemaphore.withPermit sem "b" (Effect.succeed 42))
            do! Effect.sleep 50 // let "b" suspend waiting for a permit
            let! _ = PartitionedSemaphore.release sem 1 // routed round-robin to "b"
            return! Effect.await fiber
        }

    Assert.Equal<Exit<int, string>>(Success 42, run eff)
