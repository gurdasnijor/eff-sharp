module Effect.Tests.PubSubTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/PubSub.test.ts.
//
// Upstream uses `it.effect` + `Effect.forkChild({ startImmediately })` + `Fiber`.
// This port forks concurrent publishers/subscribers with the merged `Effect.fork`
// and synchronises sequential cases with `Latch`, exactly as the kernel exposes.
// The `replay` describe-block and `Stream.fromPubSub` test are not ported — the
// replay buffer is a documented omission of the F# PubSub port.

/// Run an effect to a value, failing the test on an unexpected failure.
let private run (eff: Effect<'A, string, unit>) : 'A =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure c -> failwithf "unexpected failure: %s" (Cause.render c)

let private hasInterrupt (exit: Exit<'A, 'E>) : bool =
    match exit with
    | Failure c ->
        c.Reasons
        |> List.exists (function
            | Reason.Interrupt _ -> true
            | _ -> false)
    | Success _ -> false

// --- publishAll across capacities (BoundedPubSub) ---

let private publishAllCase (capacity: int) =
    let eff: Effect<int list * int list, string, unit> =
        effect {
            let! pubsub = PubSub.bounded capacity

            return!
                Scope.scoped (fun scope ->
                    effect {
                        let! sub1 = PubSub.subscribe scope pubsub
                        let! sub2 = PubSub.subscribe scope pubsub
                        let! _ = PubSub.publishAll pubsub [ 1; 2 ]
                        let! t1 = PubSub.takeAll sub1
                        let! t2 = PubSub.takeAll sub2
                        return (t1, t2)
                    })
        }

    let t1, t2 = run eff
    Assert.Equal<int list>([ 1; 2 ], t1)
    Assert.Equal<int list>([ 1; 2 ], t2)

[<Fact>]
let ``publishAll - capacity 2`` () = publishAllCase 2

[<Fact>]
let ``publishAll - capacity 3`` () = publishAllCase 3

[<Fact>]
let ``publishAll - capacity 4`` () = publishAllCase 4

// --- sequential publishers and subscribers (gated by a Latch) ---

[<Fact>]
let ``sequential one publisher one subscriber`` () =
    let values = [ 0..9 ]

    let eff: Effect<Exit<int list, string>, string, unit> =
        effect {
            let! latch = Latch.make false
            let! pubsub = PubSub.bounded 10

            return!
                Scope.scoped (fun scope ->
                    effect {
                        let! sub = PubSub.subscribe scope pubsub

                        let! fiber =
                            Effect.fork (
                                Latch.await latch
                                |> Effect.flatMap (fun () -> Effect.forEach values (fun _ -> PubSub.take sub))
                            )

                        let! _ = PubSub.publishAll pubsub values
                        let! _ = Latch.``open`` latch
                        return! Effect.await fiber
                    })
        }

    Assert.Equal<Exit<int list, string>>(Success values, run eff)

[<Fact>]
let ``sequential one publisher two subscribers`` () =
    let values = [ 0..9 ]

    let taker (latch: Latch) sub =
        Latch.await latch
        |> Effect.flatMap (fun () -> Effect.forEach values (fun _ -> PubSub.take sub))

    let eff: Effect<Exit<int list, string> * Exit<int list, string>, string, unit> =
        effect {
            let! latch = Latch.make false
            let! pubsub = PubSub.bounded 10

            return!
                Scope.scoped (fun scope ->
                    effect {
                        let! sub1 = PubSub.subscribe scope pubsub
                        let! sub2 = PubSub.subscribe scope pubsub
                        let! f1 = Effect.fork (taker latch sub1)
                        let! f2 = Effect.fork (taker latch sub2)
                        let! _ = PubSub.publishAll pubsub values
                        let! _ = Latch.``open`` latch
                        let! r1 = Effect.await f1
                        let! r2 = Effect.await f2
                        return (r1, r2)
                    })
        }

    let r1, r2 = run eff
    Assert.Equal<Exit<int list, string>>(Success values, r1)
    Assert.Equal<Exit<int list, string>>(Success values, r2)

// --- concurrent publishers and subscribers, one strategy each ---

let private concurrentOneToOne
    (makePubSub: Effect<PubSub<int>, string, unit>)
    (publishEff: PubSub<int> -> int list -> Effect<bool, string, unit>)
    =
    let values = [ 0..63 ]

    let eff: Effect<Exit<int list, string>, string, unit> =
        effect {
            let! pubsub = makePubSub

            return!
                Scope.scoped (fun scope ->
                    effect {
                        let! sub = PubSub.subscribe scope pubsub
                        let! subscriber = Effect.fork (Effect.forEach values (fun _ -> PubSub.take sub))
                        let! _ = Effect.fork (publishEff pubsub values)
                        return! Effect.await subscriber
                    })
        }

    Assert.Equal<Exit<int list, string>>(Success values, run eff)

[<Fact>]
let ``backpressured concurrent - one to one`` () =
    concurrentOneToOne (PubSub.bounded 64) (fun ps vs -> PubSub.publishAll ps vs)

[<Fact>]
let ``dropping concurrent - one to one`` () =
    concurrentOneToOne (PubSub.dropping 64) (fun ps vs ->
        Effect.forEach vs (fun n -> PubSub.publish ps n) |> Effect.map (fun _ -> true))

[<Fact>]
let ``sliding concurrent - one to one`` () =
    concurrentOneToOne (PubSub.sliding 64) (fun ps vs ->
        Effect.forEach vs (fun n -> PubSub.publish ps n) |> Effect.map (fun _ -> true))

[<Fact>]
let ``unbounded concurrent - one to one`` () =
    concurrentOneToOne (PubSub.unbounded ()) (fun ps vs -> PubSub.publishAll ps vs)

// --- null values flow through ---

[<Fact>]
let ``null values`` () =
    let messages: string list = [ "1"; null ]

    let eff: Effect<string list, string, unit> =
        effect {
            let! pubsub = PubSub.unbounded ()

            return!
                Scope.scoped (fun scope ->
                    effect {
                        let! sub = PubSub.subscribe scope pubsub
                        let! _ = PubSub.publishAll pubsub messages
                        return! PubSub.takeAll sub
                    })
        }

    Assert.Equal<string list>(messages, run eff)

// --- size accounting ---

[<Fact>]
let ``publish does not increase size while no subscribers`` () =
    let eff: Effect<int, string, unit> =
        effect {
            let! pubsub = PubSub.dropping 2
            let! _ = PubSub.publish pubsub 1
            let! _ = PubSub.publish pubsub 2
            return PubSub.sizeUnsafe pubsub
        }

    Assert.Equal(0, run eff)

// --- shutdown ---

[<Fact>]
let ``shutdown interrupts suspended take`` () =
    let eff: Effect<Exit<int, string>, string, unit> =
        effect {
            let! pubsub = PubSub.unbounded ()

            return!
                Scope.scoped (fun scope ->
                    effect {
                        let! sub = PubSub.subscribe scope pubsub
                        let! fiber = Effect.fork (PubSub.take sub)
                        do! Effect.sleep 50
                        let! _ = PubSub.shutdown pubsub
                        return! Effect.await fiber
                    })
        }

    Assert.True(hasInterrupt (run eff))

[<Fact>]
let ``shutdown interrupts suspended takeAll`` () =
    let eff: Effect<Exit<int list, string>, string, unit> =
        effect {
            let! pubsub = PubSub.unbounded ()

            return!
                Scope.scoped (fun scope ->
                    effect {
                        let! sub = PubSub.subscribe scope pubsub
                        let! fiber = Effect.fork (PubSub.takeAll sub)
                        do! Effect.sleep 50
                        let! _ = PubSub.shutdown pubsub
                        return! Effect.await fiber
                    })
        }

    Assert.True(hasInterrupt (run eff))

[<Fact>]
let ``publish returns false after shutdown`` () =
    let eff: Effect<bool, string, unit> =
        effect {
            let! pubsub = PubSub.unbounded ()
            do! PubSub.shutdown pubsub
            return! PubSub.publish pubsub 1
        }

    Assert.False(run eff)

[<Fact>]
let ``publishAll returns false after shutdown`` () =
    let eff: Effect<bool, string, unit> =
        effect {
            let! pubsub = PubSub.unbounded ()
            do! PubSub.shutdown pubsub
            return! PubSub.publishAll pubsub [ 1; 2; 3 ]
        }

    Assert.False(run eff)
