module Effect.Tests.SynchronizedRefTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/SynchronizedRef.test.ts.
// `it.effect` cases become xUnit Facts; effects run via `Effect.runSync ()`.
//
// Skipped: "getAndUpdateSomeEffect - interrupt parent fiber and update" — it
// exercises Deferred/Fiber.forkChild/Fiber.interrupt, none of which are ported
// yet (out of scope for w2-ref). The serialization semantics it leans on are
// covered by the effectful Facts below.

let private current = "value"
let private update = "new value"
let private failure = "failure"

type State =
    | Active
    | Changed
    | Closed

let private isActive = function
    | Active -> true
    | _ -> false

let private isChanged = function
    | Changed -> true
    | _ -> false

let private isClosed = function
    | Closed -> true
    | _ -> false

/// Run an effect that cannot fail (env = unit), returning its success value.
let private run (eff: Effect<'A, 'E, unit>) : 'A =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure cause -> failwithf "unexpected failure: %s" (Cause.render cause)

[<Fact>]
let ``get`` () =
    let result = run (SynchronizedRef.make current |> Effect.flatMap SynchronizedRef.get)
    Assert.Equal(current, result)

[<Fact>]
let ``getAndUpdateEffect - happy path`` () =
    let ref = run (SynchronizedRef.make current)
    let result1 = run (SynchronizedRef.getAndUpdateEffect ref (fun _ -> Effect.succeed update))
    let result2 = run (SynchronizedRef.get ref)
    Assert.Equal(current, result1)
    Assert.Equal(update, result2)

[<Fact>]
let ``getAndUpdateEffect - with failure`` () =
    let ref = run (SynchronizedRef.make current)
    let result = Effect.runSync () (SynchronizedRef.getAndUpdateEffect ref (fun _ -> Effect.fail failure))
    Assert.Equal<Exit<string, string>>(Exit.fail failure, result)

[<Fact>]
let ``getAndUpdateSomeEffect - happy path`` () =
    let ref = run (SynchronizedRef.make Active)

    let result1 =
        run (SynchronizedRef.getAndUpdateSomeEffect ref (fun s ->
            if isClosed s then Effect.succeed (Some Changed) else Effect.succeed None))

    let result2 = run (SynchronizedRef.get ref)
    Assert.Equal(Active, result1)
    Assert.Equal(Active, result2)

[<Fact>]
let ``getAndUpdateSomeEffect - twice`` () =
    let ref = run (SynchronizedRef.make Active)

    let result1 =
        run (SynchronizedRef.getAndUpdateSomeEffect ref (fun s ->
            if isActive s then Effect.succeed (Some Changed) else Effect.succeed None))

    let result2 =
        run (SynchronizedRef.getAndUpdateSomeEffect ref (fun s ->
            if isClosed s then Effect.succeed (Some Active)
            elif isChanged s then Effect.succeed (Some Closed)
            else Effect.succeed None))

    let result3 = run (SynchronizedRef.get ref)
    Assert.Equal(Active, result1)
    Assert.Equal(Changed, result2)
    Assert.Equal(Closed, result3)

[<Fact>]
let ``getAndUpdateSomeEffect - with failure`` () =
    let ref = run (SynchronizedRef.make Active)

    let result =
        Effect.runSync
            ()
            (SynchronizedRef.getAndUpdateSomeEffect ref (fun s ->
                if isActive s then Effect.fail failure else Effect.succeed None))

    Assert.Equal<Exit<State, string>>(Exit.fail failure, result)

// --- additional coverage of the other effectful + pure mutations ---

[<Fact>]
let ``updateAndGetEffect stores and returns the new value`` () =
    let ref = run (SynchronizedRef.make Active)
    let result = run (SynchronizedRef.updateAndGetEffect ref (fun _ -> Effect.succeed Closed))
    Assert.Equal(Closed, result)
    Assert.Equal(Closed, run (SynchronizedRef.get ref))

[<Fact>]
let ``modifyEffect returns a result while storing the new value`` () =
    let ref = run (SynchronizedRef.make current)
    let result1 = run (SynchronizedRef.modifyEffect ref (fun _ -> Effect.succeed ("hello", update)))
    let result2 = run (SynchronizedRef.get ref)
    Assert.Equal("hello", result1)
    Assert.Equal(update, result2)

[<Fact>]
let ``updateSomeAndGetEffect returns current when no transition matches`` () =
    let ref = run (SynchronizedRef.make Active)

    let result =
        run (SynchronizedRef.updateSomeAndGetEffect ref (fun s ->
            if isClosed s then Effect.succeed (Some Changed) else Effect.succeed None))

    Assert.Equal(Active, result)

[<Fact>]
let ``set then get observes the new value`` () =
    let ref = run (SynchronizedRef.make current)
    run (SynchronizedRef.set ref update)
    Assert.Equal(update, run (SynchronizedRef.get ref))

[<Fact>]
let ``update applies a pure transition under the permit`` () =
    let ref = run (SynchronizedRef.make Active)
    run (SynchronizedRef.update ref (fun _ -> Changed))
    Assert.Equal(Changed, run (SynchronizedRef.get ref))

[<Fact>]
let ``modifySome applies matching transitions`` () =
    let ref = run (SynchronizedRef.make Active)

    let result =
        run (SynchronizedRef.modifySome ref (fun s ->
            if isActive s then ("changed", Some Changed) else ("state does not change", None)))

    Assert.Equal("changed", result)
    Assert.Equal(Changed, run (SynchronizedRef.get ref))
