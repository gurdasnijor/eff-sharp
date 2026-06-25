module Effect.Tests.RcRefTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/RcRef.test.ts ("deallocation"),
// adapted to the port's explicit-`Scope` model (see RcRef.fs): `Scope.provide(s)`
// becomes passing the borrower scope to `get`. The `idleTimeToLive` case is
// skipped upstream (commented out) and not ported.

let private voidExit: Exit<obj, obj> = Success(box ())

let private run (eff: Effect<'A, 'E, unit>) : 'A =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure c -> failwithf "unexpected failure: %s" (Cause.render c)

[<Fact>]
let ``deallocation`` () =
    let acquired = ref 0
    let released = ref 0

    let acquire (scope: Scope<string, unit>) : Effect<string, string, unit> =
        Scope.acquireRelease
            scope
            (Effect.sync (fun () ->
                incr acquired
                "foo"))
            (fun _ _ -> Effect.sync (fun () -> incr released))

    let refScope = Scope.make<string, unit> ()
    let rcRef = run (RcRef.make refScope acquire)

    Assert.Equal(0, acquired.Value)

    // First borrow inside a scope that closes immediately -> acquire then release.
    let v = run (Scope.scoped (fun s -> RcRef.get rcRef s))
    Assert.Equal("foo", v)
    Assert.Equal(1, acquired.Value)
    Assert.Equal(1, released.Value)

    // Two concurrent borrowers share one acquisition.
    let scopeA = Scope.make<string, unit> ()
    let scopeB = Scope.make<string, unit> ()
    run (RcRef.get rcRef scopeA) |> ignore
    run (RcRef.get rcRef scopeB) |> ignore
    Assert.Equal(2, acquired.Value)
    Assert.Equal(1, released.Value)

    // Closing one borrower keeps the resource (count still > 0).
    run (Scope.close scopeB voidExit)
    Assert.Equal(2, acquired.Value)
    Assert.Equal(1, released.Value)

    // Closing the last borrower releases the resource.
    run (Scope.close scopeA voidExit)
    Assert.Equal(2, acquired.Value)
    Assert.Equal(2, released.Value)

    // A fresh borrow re-acquires.
    let scopeC = Scope.make<string, unit> ()
    run (RcRef.get rcRef scopeC) |> ignore
    Assert.Equal(3, acquired.Value)
    Assert.Equal(2, released.Value)

    // Closing the owner scope releases the current resource.
    run (Scope.close refScope voidExit)
    Assert.Equal(3, acquired.Value)
    Assert.Equal(3, released.Value)

    // get after the RcRef is closed fails (interrupted).
    match Effect.runSync () (Scope.scoped (fun s -> RcRef.get rcRef s)) with
    | Failure c ->
        let hasInterrupt =
            c.Reasons
            |> List.exists (function
                | Reason.Interrupt _ -> true
                | _ -> false)

        Assert.True(hasInterrupt, "expected an interrupt cause")
    | Success _ -> Assert.Fail "expected an interrupted failure"
