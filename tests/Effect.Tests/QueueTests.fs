module Effect.Tests.QueueTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/Queue.test.ts.
//
// The upstream suite drives cases through Effect.gen + it.effect and uses
// helpers this port lacks (forkChild / yieldNow / pollUnsafe / Cause.Done()).
// Here each case is an `effect { }` program run with `Effect.runSync`, using the
// merged `Effect.fork` + `Effect.await` for concurrent producers/consumers.
// The queue error channel is `obj`; end-of-input is the `QueueDone` marker
// (`Queue.isDone`), and typed failures are boxed.

let private run (eff: Effect<'A, obj, unit>) : 'A =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure c -> failwithf "unexpected failure: %s" (Cause.render c)

let private causeIsDone (c: Cause<obj>) : bool =
    c.Reasons
    |> List.exists (function
        | Reason.Fail e -> Queue.isDone e
        | _ -> false)

let private causeHasInterrupt (c: Cause<obj>) : bool =
    c.Reasons
    |> List.exists (function
        | Reason.Interrupt _ -> true
        | _ -> false)

let private firstError (c: Cause<obj>) : obj option =
    c.Reasons
    |> List.tryPick (function
        | Reason.Fail e -> Some e
        | _ -> None)

// ----------------------------------------------------------------------------

[<Fact>]
let ``offer then take`` () =
    let v =
        run (
            effect {
                let! q = Queue.bounded 10
                let! _ = Queue.offer q 42
                return! Queue.take q
            }
        )

    Assert.Equal(42, v)

[<Fact>]
let ``poll returns Option for non-blocking take`` () =
    let empty, some, getSome, empty2 =
        run (
            effect {
                let! q = Queue.bounded 10
                let! a = Queue.poll q
                let! _ = Queue.offer q 42
                let! b = Queue.poll q
                let! c = Queue.poll q
                return (a, Option.isSome b, b, c)
            }
        )

    Assert.Equal(None, empty)
    Assert.True(some)
    Assert.Equal(Some 42, getSome)
    Assert.Equal(None, empty2)

[<Fact>]
let ``peek views item without removing it`` () =
    let p1, s1, p2, taken, s2 =
        run (
            effect {
                let! q = Queue.bounded 10
                let! _ = Queue.offer q 42
                let! a = Queue.peek q
                let! n1 = Queue.size q
                let! b = Queue.peek q
                let! t = Queue.take q
                let! n2 = Queue.size q
                return (a, n1, b, t, n2)
            }
        )

    Assert.Equal(42, p1)
    Assert.Equal(1, s1)
    Assert.Equal(42, p2)
    Assert.Equal(42, taken)
    Assert.Equal(0, s2)

[<Fact>]
let ``offer dropping`` () =
    let remaining, offered, taken =
        run (
            effect {
                let! q = Queue.make 2 Dropping
                let! r = Queue.offerAll q [ 1; 2; 3; 4 ]
                let! o = Queue.offer q 5
                let! t = Queue.takeAll q
                return (r, o, t)
            }
        )

    Assert.Equal<int list>([ 3; 4 ], remaining)
    Assert.False(offered)
    Assert.Equal<int list>([ 1; 2 ], taken)

[<Fact>]
let ``offer sliding`` () =
    let remaining, offered, taken =
        run (
            effect {
                let! q = Queue.make 2 Sliding
                let! r = Queue.offerAll q [ 1; 2; 3; 4 ]
                let! o = Queue.offer q 5
                let! t = Queue.takeAll q
                return (r, o, t)
            }
        )

    Assert.Equal<int list>([], remaining)
    Assert.True(offered)
    Assert.Equal<int list>([ 4; 5 ], taken)

[<Fact>]
let ``takeN`` () =
    let a, b =
        run (
            effect {
                let! q = Queue.unbounded ()
                let! _ = Queue.offerAll q [ 1; 2; 3; 4 ]
                let! a = Queue.takeN q 2
                let! b = Queue.takeN q 2
                return (a, b)
            }
        )

    Assert.Equal<int list>([ 1; 2 ], a)
    Assert.Equal<int list>([ 3; 4 ], b)

[<Fact>]
let ``size`` () =
    let s0, s2 =
        run (
            effect {
                let! q = Queue.unbounded ()
                let! a = Queue.size q
                let! _ = Queue.offerAll q [ 1; 2 ]
                let! b = Queue.size q
                return (a, b)
            }
        )

    Assert.Equal(0, s0)
    Assert.Equal(2, s2)

[<Fact>]
let ``end completes takes after draining`` () =
    let exit =
        Effect.runSync
            ()
            (effect {
                let! q = Queue.bounded 2
                let! _ = Queue.offer q 1
                let! _ = Queue.offer q 2
                let! _ = Queue.endQueue q
                let! a = Queue.take q
                let! b = Queue.take q
                // third take must fail with the Done marker
                return! Queue.take q |> Effect.map (fun c -> (a, b, c))
            })

    match exit with
    | Failure c -> Assert.True(causeIsDone c)
    | Success _ -> Assert.True(false, "expected a Done failure on the drained queue")

[<Fact>]
let ``end then await succeeds and offer is rejected`` () =
    let awaited, offered =
        run (
            effect {
                let! q = Queue.bounded 2
                let! _ = Queue.endQueue q
                let! _ = Queue.await q
                let! o = Queue.offer q 10
                return ((), o)
            }
        )

    Assert.Equal((), awaited)
    Assert.False(offered)

[<Fact>]
let ``collect does not duplicate messages`` () =
    let result =
        run (
            effect {
                let! q = Queue.unbounded ()
                let! _ = Queue.offer q 0
                let! _ = Queue.endQueue q
                return! Queue.collect q
            }
        )

    Assert.Equal<int list>([ 0 ], result)

[<Fact>]
let ``fail drains buffered values before failing takers`` () =
    let exit =
        Effect.runSync
            ()
            (effect {
                let! q = Queue.unbounded ()
                let! _ = Queue.offerAll q [ 1; 2; 3; 4; 5 ]
                let! _ = Queue.fail q (box "boom")
                let! drained = Queue.takeAll q
                // next takeAll fails with the boxed error
                return! Queue.takeAll q |> Effect.map (fun _ -> drained)
            })

    match exit with
    | Success drained -> Assert.True(false, sprintf "expected failure, drained %A" drained)
    | Failure c ->
        Assert.False(causeIsDone c)
        Assert.Equal(Some(box "boom"), firstError c)

[<Fact>]
let ``fail drains the buffered values first`` () =
    let drained =
        run (
            effect {
                let! q = Queue.unbounded ()
                let! _ = Queue.offerAll q [ 1; 2; 3; 4; 5 ]
                let! _ = Queue.fail q (box "boom")
                return! Queue.takeAll q
            }
        )

    Assert.Equal<int list>([ 1; 2; 3; 4; 5 ], drained)

[<Fact>]
let ``interrupt allows draining then fails`` () =
    let interrupted, offered, drained, exit =
        run (
            effect {
                let! q = Queue.bounded 10
                let! _ = Queue.offerAll q [ 1; 2; 3; 4; 5 ]
                let! i = Queue.interrupt q
                let! o = Queue.offer q 6
                let! a = Queue.take q
                let! b = Queue.take q
                let! c = Queue.take q
                let! d = Queue.take q
                let! e = Queue.take q
                let! ex = Effect.fork (Queue.take q) |> Effect.flatMap Effect.await
                return (i, o, [ a; b; c; d; e ], ex)
            }
        )

    Assert.True(interrupted)
    Assert.False(offered)
    Assert.Equal<int list>([ 1; 2; 3; 4; 5 ], drained)

    match exit with
    | Failure c -> Assert.True(causeHasInterrupt c)
    | Success _ -> Assert.True(false, "drained queue should fail with interrupt")

[<Fact>]
let ``shutdown drops buffered values and fails`` () =
    let down, offered, exit =
        run (
            effect {
                let! q = Queue.bounded 10
                let! _ = Queue.offerAll q [ 1; 2; 3 ]
                let! s = Queue.shutdown q
                let! o = Queue.offer q 10
                let! ex = Effect.fork (Queue.takeAll q) |> Effect.flatMap Effect.await
                return (s, o, ex)
            }
        )

    Assert.True(down)
    Assert.False(offered)

    match exit with
    | Failure c -> Assert.True(causeHasInterrupt c)
    | Success _ -> Assert.True(false, "shutdown queue should fail takers with interrupt")

[<Fact>]
let ``bounded offerAll waits until capacity is released`` () =
    let a, b, leftover =
        run (
            effect {
                let! q = Queue.bounded 2
                let! fib = Effect.fork (Queue.offerAll q [ 1; 2; 3; 4 ])
                let! a = Queue.takeAll q
                let! b = Queue.takeAll q
                let! leftover = Effect.await fib
                return (a, b, leftover)
            }
        )

    Assert.Equal<int list>([ 1; 2 ], a)
    Assert.Equal<int list>([ 3; 4 ], b)
    Assert.Equal<Exit<int list, obj>>(Success [], leftover)

[<Fact>]
let ``bounded 0 capacity rendezvous`` () =
    let first, second =
        run (
            effect {
                let! q = Queue.bounded 0
                let! _ = Effect.fork (Queue.offer q 1)
                let! first = Queue.take q
                let! takeFib = Effect.fork (Queue.take q)
                let! _ = Queue.offer q 2
                let! second = Effect.join takeFib
                return (first, second)
            }
        )

    Assert.Equal(1, first)
    Assert.Equal(2, second)

[<Fact>]
let ``await waits for items then completes`` () =
    let pendingBefore, drained, completed =
        run (
            effect {
                let! q = Queue.unbounded ()
                let! awaitFib = Effect.fork (Queue.await q)
                let! _ = Queue.offer q 1
                let! _ = Queue.endQueue q
                // await must still be pending while the value is buffered
                let! pending = Fiber.poll awaitFib
                let! items = Queue.takeAll q
                // draining empties the queue, so await can now complete
                let! ex = Effect.await awaitFib
                return (Option.isNone pending, items, ex)
            }
        )

    Assert.True(pendingBefore)
    Assert.Equal<int list>([ 1 ], drained)
    Assert.Equal<Exit<unit, obj>>(Success(), completed)

[<Fact>]
let ``concurrent producers and consumer`` () =
    let total =
        run (
            effect {
                let! q = Queue.bounded 4
                let! _ = Effect.fork (Queue.offerAll q [ 1; 2; 3; 4; 5 ])
                let! _ = Effect.fork (Queue.offerAll q [ 6; 7; 8; 9; 10 ])
                let! taken = Queue.takeN q 10
                return List.sum taken
            }
        )

    Assert.Equal(55, total)
