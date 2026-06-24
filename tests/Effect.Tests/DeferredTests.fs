module Effect.Tests.DeferredTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/Deferred.test.ts.
//
// The upstream suite drives each case through `Effect.gen` and inspects results
// with `Effect.exit`. This port allocates the `Deferred` with `makeUnsafe` and
// runs each operation with `Effect.runSync`, which already yields an `Exit`
// (never raising for a typed failure) — the direct analogue of `Effect.exit`.

/// Run a `bool`-returning completion effect (the error channel is phantom — these
/// effects never fail — so it is pinned to `string` here).
let private runBool (eff: Effect<bool, string, unit>) : bool =
    match Effect.runSync () eff with
    | Success b -> b
    | Failure c -> failwithf "unexpected failure: %s" (Cause.render c)

// ----------------------------------------------------------------------------
// success
// ----------------------------------------------------------------------------

[<Fact>]
let ``succeed - should propagate the value`` () =
    let deferred = Deferred.makeUnsafe<int, string> ()
    Assert.True(runBool (Deferred.succeed deferred 1))
    Assert.False(runBool (Deferred.succeed deferred 1))
    Assert.Equal<Exit<int, string>>(Exit.succeed 1, Effect.runSync () (Deferred.await deferred))

[<Fact>]
let ``complete - should memoize the result of the provided effect`` () =
    let value = ref 0

    let complete: Effect<int, string, unit> =
        Effect.sync (fun () ->
            value.Value <- value.Value + 1
            value.Value)

    let deferred = Deferred.makeUnsafe<int, string> ()
    Assert.True(runBool (Deferred.complete deferred complete))
    Assert.False(runBool (Deferred.complete deferred complete))
    Assert.Equal(1, value.Value)
    Assert.Equal<Exit<int, string>>(Exit.succeed 1, Effect.runSync () (Deferred.await deferred))
    Assert.Equal<Exit<int, string>>(Exit.succeed 1, Effect.runSync () (Deferred.await deferred))
    Assert.Equal(1, value.Value)

[<Fact>]
let ``completeWith - should not memoize the provided effect`` () =
    let value = ref 0

    let complete: Effect<int, string, unit> =
        Effect.sync (fun () ->
            value.Value <- value.Value + 1
            value.Value)

    let complete2: Effect<int, string, unit> =
        Effect.sync (fun () ->
            value.Value <- value.Value + 10
            value.Value)

    let deferred = Deferred.makeUnsafe<int, string> ()
    Assert.True(runBool (Deferred.completeWith deferred complete))
    Assert.False(runBool (Deferred.completeWith deferred complete2))
    Assert.Equal(0, value.Value)
    Assert.Equal<Exit<int, string>>(Exit.succeed 1, Effect.runSync () (Deferred.await deferred))
    Assert.Equal<Exit<int, string>>(Exit.succeed 2, Effect.runSync () (Deferred.await deferred))
    Assert.Equal(2, value.Value)

// ----------------------------------------------------------------------------
// failure
// ----------------------------------------------------------------------------

[<Fact>]
let ``fail - should propagate the failure`` () =
    let deferred = Deferred.makeUnsafe<int, string> ()
    Assert.True(runBool (Deferred.fail deferred "boom"))
    Assert.False(runBool (Deferred.succeed deferred 1))
    Assert.Equal<Exit<int, string>>(Exit.fail "boom", Effect.runSync () (Deferred.await deferred))

[<Fact>]
let ``complete - should memoize the failure of the provided effect`` () =
    let deferred = Deferred.makeUnsafe<int, string> ()
    Assert.True(runBool (Deferred.complete deferred (Effect.fail "boom")))
    Assert.False(runBool (Deferred.complete deferred (Effect.fail "boom2")))
    Assert.Equal<Exit<int, string>>(Exit.fail "boom", Effect.runSync () (Deferred.await deferred))
    Assert.Equal<Exit<int, string>>(Exit.fail "boom", Effect.runSync () (Deferred.await deferred))

[<Fact>]
let ``completeWith - should memoize the provided effect`` () =
    // The stored effect is re-run per awaiter; `failSync` here increments an
    // index so the two awaits observe distinct failures (boom, then boom2).
    let i = ref 0
    let failures = [| "boom"; "boom2" |]

    let complete: Effect<int, string, unit> =
        Effect.sync (fun () ->
            let v = failures.[i.Value]
            i.Value <- i.Value + 1
            v)
        |> Effect.flatMap Effect.fail

    let complete2: Effect<int, string, unit> = Effect.fail "boom3"

    let deferred = Deferred.makeUnsafe<int, string> ()
    Assert.True(runBool (Deferred.completeWith deferred complete))
    Assert.False(runBool (Deferred.completeWith deferred complete2))
    Assert.Equal<Exit<int, string>>(Exit.fail "boom", Effect.runSync () (Deferred.await deferred))
    Assert.Equal<Exit<int, string>>(Exit.fail "boom2", Effect.runSync () (Deferred.await deferred))

// ----------------------------------------------------------------------------
// interruption
// ----------------------------------------------------------------------------

[<Fact>]
let ``interrupt - should propagate the interruption`` () =
    let deferred = Deferred.makeUnsafe<int, string> ()
    Assert.True(runBool (Deferred.interruptWith deferred -1))
    Assert.False(runBool (Deferred.interrupt deferred))
    Assert.Equal<Exit<int, string>>(Exit.failCause (Cause.interrupt (Some -1)), Effect.runSync () (Deferred.await deferred))

// ----------------------------------------------------------------------------
// polling
// ----------------------------------------------------------------------------

[<Fact>]
let ``poll - should return None when the deferred is incomplete`` () =
    let deferred = Deferred.makeUnsafe<int, string> ()
    let poll: Effect<Effect<int, string, unit> option, string, unit> = Deferred.poll deferred
    Assert.Equal<Exit<Effect<int, string, unit> option, string>>(Exit.succeed None, Effect.runSync () poll)

[<Fact>]
let ``poll - should return Some when the deferred was completed`` () =
    let deferred = Deferred.makeUnsafe<int, string> ()
    runBool (Deferred.succeed deferred 1) |> ignore
    let poll: Effect<Effect<int, string, unit> option, string, unit> = Deferred.poll deferred

    match Effect.runSync () poll with
    | Success(Some inner) -> Assert.Equal<Exit<int, string>>(Exit.succeed 1, Effect.runSync () inner)
    | other -> Assert.Fail(sprintf "expected Some, got %A" other)

[<Fact>]
let ``poll - should return Some when the deferred was failed`` () =
    let deferred = Deferred.makeUnsafe<int, string> ()
    runBool (Deferred.fail deferred "boom") |> ignore
    let poll: Effect<Effect<int, string, unit> option, string, unit> = Deferred.poll deferred

    match Effect.runSync () poll with
    | Success(Some inner) -> Assert.Equal<Exit<int, string>>(Exit.fail "boom", Effect.runSync () inner)
    | other -> Assert.Fail(sprintf "expected Some, got %A" other)

[<Fact>]
let ``poll - should return Some when the deferred was interrupted`` () =
    let deferred = Deferred.makeUnsafe<int, string> ()
    runBool (Deferred.interruptWith deferred -1) |> ignore
    let poll: Effect<Effect<int, string, unit> option, string, unit> = Deferred.poll deferred

    match Effect.runSync () poll with
    | Success(Some inner) ->
        Assert.Equal<Exit<int, string>>(Exit.failCause (Cause.interrupt (Some -1)), Effect.runSync () inner)
    | other -> Assert.Fail(sprintf "expected Some, got %A" other)

// ----------------------------------------------------------------------------
// done
// ----------------------------------------------------------------------------

[<Fact>]
let ``isDone - should return true when succeeded`` () =
    let deferred = Deferred.makeUnsafe<int, string> ()
    runBool (Deferred.succeed deferred 1) |> ignore
    Assert.True(runBool (Deferred.isDone deferred))

[<Fact>]
let ``isDone - should return true when failed`` () =
    let deferred = Deferred.makeUnsafe<int, string> ()
    runBool (Deferred.fail deferred "boom") |> ignore
    Assert.True(runBool (Deferred.isDone deferred))
