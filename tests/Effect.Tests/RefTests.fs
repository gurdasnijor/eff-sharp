module Effect.Tests.RefTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/Ref.test.ts.
// `it.effect` cases become xUnit Facts; effects run via `Effect.runSync ()`.

let private current = "value"
let private update = "new value"

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
let ``get returns the current value`` () =
    let result = run (Ref.get (Ref.makeUnsafe current))
    Assert.Equal(current, result)

[<Fact>]
let ``getAndSet returns the previous value and stores the replacement`` () =
    let ref = run (Ref.make current)
    let result1 = run (Ref.getAndSet ref update)
    Assert.Equal(current, result1)
    Assert.Equal(update, run (Ref.get ref))

[<Fact>]
let ``getAndUpdate returns the previous value and stores the computed value`` () =
    let ref = run (Ref.make current)
    let result1 = run (Ref.getAndUpdate ref (fun _ -> update))
    Assert.Equal(current, result1)
    Assert.Equal(update, run (Ref.get ref))

[<Fact>]
let ``getAndUpdateSome leaves state unchanged when no transition matches`` () =
    let ref = run (Ref.make Active)
    let result1 = run (Ref.getAndUpdateSome ref (fun s -> if isClosed s then Some Changed else None))
    let result2 = run (Ref.get ref)
    Assert.Equal(Active, result1)
    Assert.Equal(Active, result2)

[<Fact>]
let ``getAndUpdateSome returns each previous state while applying matching transitions`` () =
    let ref = run (Ref.make Active)
    let result1 = run (Ref.getAndUpdateSome ref (fun s -> if isActive s then Some Changed else None))

    let result2 =
        run (
            Ref.getAndUpdateSome ref (fun s ->
                if isActive s then Some Changed
                elif isChanged s then Some Closed
                else None)
        )

    let result3 = run (Ref.get ref)
    Assert.Equal(Active, result1)
    Assert.Equal(Changed, result2)
    Assert.Equal(Closed, result3)

[<Fact>]
let ``set stores the provided value`` () =
    let ref = run (Ref.make current)
    run (Ref.set ref update)
    Assert.Equal(update, run (Ref.get ref))

[<Fact>]
let ``update stores the computed value`` () =
    let ref = run (Ref.make current)
    run (Ref.update ref (fun _ -> update))
    Assert.Equal(update, run (Ref.get ref))

[<Fact>]
let ``updateAndGet stores and returns the computed value`` () =
    let ref = run (Ref.make current)
    let result = run (Ref.updateAndGet ref (fun _ -> update))
    Assert.Equal(update, result)

[<Fact>]
let ``updateSome leaves state unchanged when no transition matches`` () =
    let ref = run (Ref.make Active)
    run (Ref.updateSome ref (fun s -> if isClosed s then Some Changed else None))
    Assert.Equal(Active, run (Ref.get ref))

[<Fact>]
let ``updateSome applies matching transitions to the stored state`` () =
    let ref = run (Ref.make Active)
    run (Ref.updateSome ref (fun s -> if isActive s then Some Changed else None))
    let result1 = run (Ref.get ref)

    run (
        Ref.updateSome ref (fun s ->
            if isActive s then Some Changed
            elif isChanged s then Some Closed
            else None)
    )

    let result2 = run (Ref.get ref)
    Assert.Equal(Changed, result1)
    Assert.Equal(Closed, result2)

[<Fact>]
let ``updateSomeAndGet returns the current state when no transition matches`` () =
    let ref = run (Ref.make Active)
    let result = run (Ref.updateSomeAndGet ref (fun s -> if isClosed s then Some Changed else None))
    Assert.Equal(Active, result)

[<Fact>]
let ``updateSomeAndGet - twice`` () =
    let ref = run (Ref.make Active)
    let result1 = run (Ref.updateSomeAndGet ref (fun s -> if isActive s then Some Changed else None))

    let result2 =
        run (
            Ref.updateSomeAndGet ref (fun s ->
                if isActive s then Some Changed
                elif isChanged s then Some Closed
                else None)
        )

    Assert.Equal(Changed, result1)
    Assert.Equal(Closed, result2)

[<Fact>]
let ``modify returns a result while storing the new value`` () =
    let ref = run (Ref.make current)
    let result1 = run (Ref.modify ref (fun _ -> ("hello", update)))
    let result2 = run (Ref.get ref)
    Assert.Equal("hello", result1)
    Assert.Equal(update, result2)

[<Fact>]
let ``modifySome - once`` () =
    let ref = run (Ref.make Active)

    let result =
        run (
            Ref.modifySome ref (fun s ->
                if isClosed s then ("active", Some Active) else ("state does not change", None))
        )

    Assert.Equal("state does not change", result)

[<Fact>]
let ``modifySome returns a result while applying matching transitions`` () =
    let ref = run (Ref.make Active)

    let result1 =
        run (
            Ref.modifySome ref (fun s ->
                if isActive s then ("changed", Some Changed) else ("state does not change", None))
        )

    let result2 =
        run (
            Ref.modifySome ref (fun s ->
                if isActive s then ("changed", Some Changed)
                elif isChanged s then ("closed", Some Closed)
                else ("state does not change", None))
        )

    Assert.Equal("changed", result1)
    Assert.Equal("closed", result2)
