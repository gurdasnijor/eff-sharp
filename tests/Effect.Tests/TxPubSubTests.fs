module Effect.Tests.TxPubSubTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/TxPubSub.test.ts (subset):
// subscribe/publish broadcast, publish-after-subscription semantics, strategies,
// shutdown, and unsubscribe.

let private run (eff: Effect<'a, 'e, unit>) : 'a =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure cause -> failwithf "unexpected failure: %s" (Cause.render cause)

[<Fact>]
let ``subscribe then publish delivers to the subscriber`` () =
    let hub: TxPubSub<string> = run (TxPubSub.unbounded ())
    let sub = run (TxPubSub.subscribe hub)
    Assert.True(run (TxPubSub.publish hub "hello"))
    Assert.Equal("hello", run (TxQueue.take sub))

[<Fact>]
let ``publish with no subscribers is a no-op returning true`` () =
    let hub: TxPubSub<string> = run (TxPubSub.unbounded ())
    Assert.True(run (TxPubSub.publish hub "no one listening"))

[<Fact>]
let ``broadcast reaches every current subscriber`` () =
    let hub: TxPubSub<int> = run (TxPubSub.unbounded ())
    let sub1 = run (TxPubSub.subscribe hub)
    let sub2 = run (TxPubSub.subscribe hub)
    Assert.True(run (TxPubSub.publish hub 7))
    Assert.Equal(7, run (TxQueue.take sub1))
    Assert.Equal(7, run (TxQueue.take sub2))

[<Fact>]
let ``a subscriber only receives messages published after it subscribed`` () =
    let hub: TxPubSub<string> = run (TxPubSub.unbounded ())
    let sub1 = run (TxPubSub.subscribe hub)
    run (TxPubSub.publish hub "A") |> ignore
    let sub2 = run (TxPubSub.subscribe hub)
    run (TxPubSub.publish hub "B") |> ignore

    Assert.Equal("A", run (TxQueue.take sub1))
    Assert.Equal("B", run (TxQueue.take sub1))
    Assert.Equal("B", run (TxQueue.take sub2)) // sub2 never sees "A"
    Assert.Equal(0, run (TxQueue.size sub2))

[<Fact>]
let ``publishAll delivers every message in order`` () =
    let hub: TxPubSub<int> = run (TxPubSub.unbounded ())
    let sub = run (TxPubSub.subscribe hub)
    Assert.True(run (TxPubSub.publishAll hub [ 1; 2; 3 ]))
    Assert.Equal<int[]>([| 1; 2; 3 |], run (TxQueue.takeAll sub))

[<Fact>]
let ``dropping hub drops messages for a full subscriber`` () =
    let hub: TxPubSub<int> = run (TxPubSub.dropping 2)
    let sub = run (TxPubSub.subscribe hub)
    Assert.True(run (TxPubSub.publish hub 1))
    Assert.True(run (TxPubSub.publish hub 2))
    Assert.False(run (TxPubSub.publish hub 3)) // dropped for the full subscriber
    Assert.Equal(1, run (TxQueue.take sub))
    Assert.Equal(2, run (TxQueue.take sub))

[<Fact>]
let ``shutdown stops publishing and marks the hub shut down`` () =
    let hub: TxPubSub<int> = run (TxPubSub.unbounded ())
    let sub = run (TxPubSub.subscribe hub)
    run (TxPubSub.shutdown hub)
    Assert.True(run (TxPubSub.isShutdown hub))
    Assert.False(run (TxPubSub.publish hub 1)) // no longer accepts
    Assert.True(run (TxQueue.isDone sub)) // subscriber queues are shut down too

[<Fact>]
let ``unsubscribe removes the subscriber and shuts its queue down`` () =
    let hub: TxPubSub<int> = run (TxPubSub.unbounded ())
    let sub = run (TxPubSub.subscribe hub)
    run (TxPubSub.unsubscribe hub sub)
    Assert.True(run (TxQueue.isDone sub))
    // with no subscribers, publish is a no-op success
    Assert.True(run (TxPubSub.publish hub 42))
