module Effect.Tests.LatchTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/Latch.test.ts.
//
// Upstream forks a waiter fiber and inspects it with `pollUnsafe`. This wave does
// not own `Fiber`, so a waiter is forked onto the .NET threadpool via
// `Effect.runExit |> Async.StartAsTask` and inspected with `Task.IsCompleted` /
// `Task.Wait(timeout)`. A still-suspended waiter reliably fails to complete
// within the wait window; a released one completes promptly (the gate is already
// signalled), so the timing assertions are not racy.

/// Run a `bool`-returning latch effect (phantom error channel pinned to string).
let private runBool (eff: Effect<bool, string, unit>) : bool =
    match Effect.runSync () eff with
    | Success b -> b
    | Failure c -> failwithf "unexpected failure: %s" (Cause.render c)

/// Fork an effect onto the threadpool, returning its in-flight `Task<Exit>`.
let private fork (eff: Effect<'A, string, unit>) =
    Effect.runExit () eff |> Async.StartAsTask

[<Fact>]
let ``isOpen reflects the state of the latch`` () =
    let latch = Latch.makeUnsafe false
    Assert.False(Latch.isOpen latch)
    Assert.True(runBool (Latch.``open`` latch))
    Assert.True(Latch.isOpen latch)

[<Fact>]
let ``await on an open latch completes immediately`` () =
    let latch = Latch.makeUnsafe true
    let awaitEff: Effect<unit, string, unit> = Latch.await latch
    Assert.Equal<Exit<unit, string>>(Exit.succeed (), Effect.runSync () awaitEff)

[<Fact>]
let ``open releases a suspended waiter`` () =
    let latch = Latch.makeUnsafe false
    let awaitEff: Effect<unit, string, unit> = Latch.await latch
    let waiter = fork awaitEff

    // Closed latch: the waiter stays suspended.
    Assert.False(waiter.Wait 100)

    Assert.True(runBool (Latch.``open`` latch))
    Assert.True(waiter.Wait 1000)
    Assert.Equal<Exit<unit, string>>(Exit.succeed (), waiter.Result)

[<Fact>]
let ``release wakes current waiters and keeps the latch closed`` () =
    let latch = Latch.makeUnsafe false
    let awaitEff: Effect<unit, string, unit> = Latch.await latch
    let waiter = fork awaitEff

    Assert.False(waiter.Wait 100)

    // release wakes the current waiter but leaves the latch closed.
    Assert.True(runBool (Latch.release latch))
    Assert.True(waiter.Wait 1000)
    Assert.False(Latch.isOpen latch)

    // A fresh waiter suspends again because the latch is still closed.
    let waiter2 = fork awaitEff
    Assert.False(waiter2.Wait 100)

    // Clean up so the forked async cannot outlive the test.
    Latch.openUnsafe latch |> ignore
    waiter2.Wait 1000 |> ignore

[<Fact>]
let ``release on an open latch is a no-op returning false`` () =
    let latch = Latch.makeUnsafe true
    Assert.False(runBool (Latch.release latch))
    Assert.True(Latch.isOpen latch)

[<Fact>]
let ``open is idempotent`` () =
    let latch = Latch.makeUnsafe false
    Assert.True(runBool (Latch.``open`` latch))
    Assert.False(runBool (Latch.``open`` latch))

[<Fact>]
let ``close re-arms an open latch`` () =
    let latch = Latch.makeUnsafe true
    Assert.True(Latch.isOpen latch)
    Assert.True(runBool (Latch.close latch))
    Assert.False(Latch.isOpen latch)
    Assert.False(runBool (Latch.close latch))

[<Fact>]
let ``whenOpen runs the effect immediately on an open latch`` () =
    let latch = Latch.makeUnsafe true
    let eff: Effect<int, string, unit> = Latch.whenOpen latch (Effect.succeed 42)
    Assert.Equal<Exit<int, string>>(Exit.succeed 42, Effect.runSync () eff)

[<Fact>]
let ``whenOpen gates the effect behind a closed latch`` () =
    let latch = Latch.makeUnsafe false
    let eff: Effect<int, string, unit> = Latch.whenOpen latch (Effect.succeed 42)
    let waiter = fork eff

    Assert.False(waiter.Wait 100)

    Latch.openUnsafe latch |> ignore
    Assert.True(waiter.Wait 1000)
    Assert.Equal<Exit<int, string>>(Exit.succeed 42, waiter.Result)
