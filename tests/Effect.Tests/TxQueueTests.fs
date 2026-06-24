module Effect.Tests.TxQueueTests

open System.Threading
open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/TxQueue.test.ts (subset):
// strategies (bounded/unbounded/dropping/sliding), offer/take/poll/takeAll,
// size/isEmpty/isFull, failure propagation, and the two STM blocking guarantees
// (full bounded offer retries; empty take retries) exercised with real threads.

let private run (eff: Effect<'a, 'e, unit>) : 'a =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure cause -> failwithf "unexpected failure: %s" (Cause.render cause)

[<Fact>]
let ``bounded: offer then take`` () =
    let q: TxQueue<int, string> = run (TxQueue.bounded 10)
    Assert.True(run (TxQueue.offer q 42))
    Assert.Equal(1, run (TxQueue.size q))
    Assert.Equal(42, run (TxQueue.take q))
    Assert.True(run (TxQueue.isEmpty q))

[<Fact>]
let ``unbounded: offerAll then takeAll preserves order`` () =
    let q: TxQueue<int, string> = run (TxQueue.unbounded ())
    let rejected = run (TxQueue.offerAll q [ 1; 2; 3; 4; 5 ])
    Assert.Empty(rejected)
    Assert.Equal<int[]>([| 1; 2; 3; 4; 5 |], run (TxQueue.takeAll q))

[<Fact>]
let ``poll returns None when empty, Some after offer`` () =
    let q: TxQueue<int, string> = run (TxQueue.unbounded ())
    Assert.Equal(None, run (TxQueue.poll q))
    run (TxQueue.offer q 7) |> ignore
    Assert.Equal(Some 7, run (TxQueue.poll q))

[<Fact>]
let ``isFull reflects capacity for bounded; never for unbounded`` () =
    let q: TxQueue<int, string> = run (TxQueue.bounded 2)
    Assert.False(run (TxQueue.isFull q))
    run (TxQueue.offerAll q [ 1; 2 ]) |> ignore
    Assert.True(run (TxQueue.isFull q))

    let u: TxQueue<int, string> = run (TxQueue.unbounded ())
    run (TxQueue.offerAll u [ 1; 2; 3 ]) |> ignore
    Assert.False(run (TxQueue.isFull u))

[<Fact>]
let ``dropping: offers beyond capacity are rejected`` () =
    let q: TxQueue<int, string> = run (TxQueue.dropping 2)
    Assert.True(run (TxQueue.offer q 1))
    Assert.True(run (TxQueue.offer q 2))
    Assert.False(run (TxQueue.offer q 3)) // dropped
    Assert.Equal(2, run (TxQueue.size q))
    Assert.Equal(1, run (TxQueue.take q))
    Assert.Equal(2, run (TxQueue.take q))

[<Fact>]
let ``sliding: offers beyond capacity evict the oldest`` () =
    let q: TxQueue<int, string> = run (TxQueue.sliding 2)
    run (TxQueue.offer q 1) |> ignore
    run (TxQueue.offer q 2) |> ignore
    Assert.True(run (TxQueue.offer q 3)) // evicts 1
    Assert.Equal(2, run (TxQueue.take q))
    Assert.Equal(3, run (TxQueue.take q))

[<Fact>]
let ``fail: take fails with the queue error`` () =
    let q: TxQueue<int, string> = run (TxQueue.bounded 10)
    Assert.True(run (TxQueue.fail q "boom"))

    match Effect.runSync () (TxQueue.take q) with
    | Failure cause -> Assert.Equal<string list>([ "boom" ], Cause.failures cause)
    | Success v -> Assert.Fail(sprintf "expected failure, got %d" v)

[<Fact>]
let ``interrupt marks the queue done`` () =
    let q: TxQueue<int, string> = run (TxQueue.bounded 10)
    Assert.True(run (TxQueue.isOpen q))
    Assert.True(run (TxQueue.interrupt q))
    Assert.True(run (TxQueue.isDone q))
    Assert.True(run (TxQueue.isShutdown q))

[<Fact>]
let ``failCause with buffered items enters Closing, drains, then Done`` () =
    let q: TxQueue<int, string> = run (TxQueue.bounded 10)
    run (TxQueue.offerAll q [ 1; 2 ]) |> ignore
    Assert.True(run (TxQueue.failCause q (Cause.fail "stop")))
    Assert.True(run (TxQueue.isClosing q))
    // drain the buffered items, last take flips Closing -> Done
    Assert.Equal(1, run (TxQueue.take q))
    Assert.Equal(2, run (TxQueue.take q))
    Assert.True(run (TxQueue.isDone q))

[<Fact>]
let ``empty take blocks until another thread offers`` () =
    let q: TxQueue<int, string> = run (TxQueue.bounded 10)
    let observed = TxRef.makeUnsafe -1

    let consumer =
        Thread(ThreadStart(fun () ->
            let v = run (TxQueue.take q)
            run (TxRef.atomically (TxRef.set observed v))))

    consumer.Start()
    Thread.Sleep 250
    let before = TxRef.getUnsafe observed // still blocked on empty take
    run (TxQueue.offer q 99) |> ignore
    let joined = consumer.Join 5000

    Assert.Equal(-1, before)
    Assert.True(joined)
    Assert.Equal(99, TxRef.getUnsafe observed)

[<Fact>]
let ``full bounded offer blocks until another thread takes`` () =
    let q: TxQueue<int, string> = run (TxQueue.bounded 2)
    run (TxQueue.offerAll q [ 1; 2 ]) |> ignore // now full
    let produced = TxRef.makeUnsafe false

    let producer =
        Thread(ThreadStart(fun () ->
            run (TxQueue.offer q 3) |> ignore // blocks until space
            run (TxRef.atomically (TxRef.set produced true))))

    producer.Start()
    Thread.Sleep 250
    let beforeTake = TxRef.getUnsafe produced // producer still blocked (queue full)
    Assert.Equal(1, run (TxQueue.take q)) // free a slot
    let joined = producer.Join 5000

    Assert.False(beforeTake)
    Assert.True(joined)
    Assert.True(TxRef.getUnsafe produced)
    // remaining items are 2 then the newly-accepted 3
    Assert.Equal(2, run (TxQueue.take q))
    Assert.Equal(3, run (TxQueue.take q))
