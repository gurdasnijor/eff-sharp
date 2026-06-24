module Effect.Tests.TxDeferredTests

open System.Threading
open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/TxDeferred.test.ts (subset).

let private run (eff: Effect<'a, 'e, unit>) : 'a =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure cause -> failwithf "unexpected failure: %s" (Cause.render cause)

[<Fact>]
let ``poll is None before completion, Some after`` () =
    let d: TxDeferred<int, string> = run (TxDeferred.make ())
    Assert.Equal(None, run (TxRef.atomically (TxDeferred.poll d)))
    run (TxRef.atomically (TxDeferred.succeed d 42) |> Effect.map ignore)
    Assert.Equal(Some(Ok 42), run (TxRef.atomically (TxDeferred.poll d)))

[<Fact>]
let ``succeed completes once; the second write is a no-op`` () =
    let d: TxDeferred<int, string> = run (TxDeferred.make ())
    let first = run (TxRef.atomically (TxDeferred.succeed d 1))
    let second = run (TxRef.atomically (TxDeferred.succeed d 2))
    Assert.True(first)
    Assert.False(second)
    Assert.Equal(1, run (TxDeferred.await d))

[<Fact>]
let ``await surfaces a typed failure`` () =
    let d: TxDeferred<int, string> = run (TxDeferred.make ())
    run (TxRef.atomically (TxDeferred.fail d "boom") |> Effect.map ignore)

    match Effect.runSync () (TxDeferred.await d) with
    | Failure cause -> Assert.Equal<string list>([ "boom" ], Cause.failures cause)
    | Success v -> Assert.Fail(sprintf "expected failure, got %d" v)

[<Fact>]
let ``await blocks until another thread completes the deferred`` () =
    let d: TxDeferred<int, string> = run (TxDeferred.make ())
    let observed = TxRef.makeUnsafe -1

    let waiter =
        Thread(ThreadStart(fun () ->
            let v = run (TxDeferred.await d)
            run (TxRef.atomically (TxRef.set observed v))))

    waiter.Start()
    Thread.Sleep 250
    let before = TxRef.getUnsafe observed // still blocked
    run (TxRef.atomically (TxDeferred.succeed d 7) |> Effect.map ignore)
    let joined = waiter.Join 5000

    Assert.Equal(-1, before)
    Assert.True(joined)
    Assert.Equal(7, TxRef.getUnsafe observed)
