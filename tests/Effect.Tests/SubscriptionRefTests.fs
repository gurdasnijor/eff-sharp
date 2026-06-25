module Effect.Tests.SubscriptionRefTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/SubscriptionRef.test.ts,
// adapted to this port (fork + a Deferred barrier instead of forkScoped + Latch;
// `isSubscriptionRef` omitted as a JS-runtime guard).

let private run (eff: Effect<'a, 'e, unit>) : 'a =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure cause -> failwithf "unexpected failure: %s" (Cause.render cause)

[<Fact>]
let ``get returns the initial value; set updates it`` () =
    let before, after =
        run (
            effect {
                let! ref = SubscriptionRef.make 0
                let! a = SubscriptionRef.get ref
                do! SubscriptionRef.set ref 42
                let! b = SubscriptionRef.get ref
                return (a, b)
            }
        )

    Assert.Equal(0, before)
    Assert.Equal(42, after)

[<Fact>]
let ``update / getAndSet / modify`` () =
    let updated, previous, modified, current =
        run (
            effect {
                let! ref = SubscriptionRef.make 10
                do! SubscriptionRef.update ref (fun n -> n + 1) // 11
                let! u = SubscriptionRef.get ref
                let! p = SubscriptionRef.getAndSet ref 20 // returns 11, sets 20
                let! m = SubscriptionRef.modify ref (fun n -> (sprintf "was %d" n, n * 2)) // "was 20", sets 40
                let! c = SubscriptionRef.get ref
                return (u, p, m, c)
            }
        )

    Assert.Equal(11, updated)
    Assert.Equal(11, previous)
    Assert.Equal("was 20", modified)
    Assert.Equal(40, current)

[<Fact>]
let ``changes streams the current value and subsequent updates`` () =
    let result =
        run (
            effect {
                let! ref = SubscriptionRef.make 0
                let! ready = Deferred.make ()

                let collector =
                    SubscriptionRef.changes ref
                    |> Stream.tap (fun _ -> Deferred.succeed ready () |> Effect.map ignore)
                    |> Stream.take 3
                    |> Stream.runCollect

                let! fiber = Effect.fork collector
                do! Deferred.await ready // subscription is live (current value emitted)
                do! SubscriptionRef.set ref 1
                do! SubscriptionRef.set ref 2
                return! Fiber.join fiber
            }
        )

    Assert.Equal<int list>([ 0; 1; 2 ], result)

[<Fact>]
let ``a late subscriber sees the value current at subscription time`` () =
    let result =
        run (
            effect {
                let! ref = SubscriptionRef.make 0
                do! SubscriptionRef.set ref 1
                do! SubscriptionRef.set ref 2 // updates before anyone subscribes are not buffered

                let! ready = Deferred.make ()

                let collector =
                    SubscriptionRef.changes ref
                    |> Stream.tap (fun _ -> Deferred.succeed ready () |> Effect.map ignore)
                    |> Stream.take 2
                    |> Stream.runCollect

                let! fiber = Effect.fork collector
                do! Deferred.await ready // current value (2) emitted
                do! SubscriptionRef.set ref 3
                return! Fiber.join fiber
            }
        )

    Assert.Equal<int list>([ 2; 3 ], result)
