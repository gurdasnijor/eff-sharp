module Effect.Tests.TxSemaphoreTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/TxSemaphore.test.ts.

let private run (eff: Effect<'a, 'e, unit>) : 'a =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure cause -> failwithf "unexpected failure: %s" (Cause.render cause)

/// Assert an effect completes with a defect (Die).
let private assertDies (eff: Effect<'a, 'e, unit>) =
    match Effect.runSync () eff with
    | Failure cause -> Assert.False(List.isEmpty (Cause.defects cause), "expected a defect")
    | Success _ -> Assert.Fail "expected a defect"

// --- constructors ---

[<Fact>]
let ``make creates semaphore with specified permits`` () =
    let sem = run (TxSemaphore.make 5)
    Assert.Equal(5, run (TxSemaphore.available sem))
    Assert.Equal(5, TxSemaphore.capacity sem)

[<Fact>]
let ``make with zero permits creates empty semaphore`` () =
    let sem = run (TxSemaphore.make 0)
    Assert.Equal(0, run (TxSemaphore.available sem))
    Assert.Equal(0, TxSemaphore.capacity sem)

[<Fact>]
let ``make with negative permits causes defect`` () = assertDies (TxSemaphore.make -1)

// --- basic operations ---

[<Fact>]
let ``acquire and release work correctly`` () =
    let sem = run (TxSemaphore.make 3)
    run (TxSemaphore.acquire sem)
    Assert.Equal(2, run (TxSemaphore.available sem))
    run (TxSemaphore.release sem)
    Assert.Equal(3, run (TxSemaphore.available sem))

[<Fact>]
let ``acquireN and releaseN work correctly`` () =
    let sem = run (TxSemaphore.make 5)
    run (TxSemaphore.acquireN sem 3)
    Assert.Equal(2, run (TxSemaphore.available sem))
    run (TxSemaphore.releaseN sem 2)
    Assert.Equal(4, run (TxSemaphore.available sem))

[<Fact>]
let ``tryAcquire succeeds when permits available`` () =
    let sem = run (TxSemaphore.make 2)
    Assert.True(run (TxSemaphore.tryAcquire sem))
    Assert.Equal(1, run (TxSemaphore.available sem))

[<Fact>]
let ``tryAcquire fails when no permits available`` () =
    let sem = run (TxSemaphore.make 1)
    run (TxSemaphore.acquire sem)
    Assert.False(run (TxSemaphore.tryAcquire sem))
    Assert.Equal(0, run (TxSemaphore.available sem))

[<Fact>]
let ``tryAcquireN succeeds when enough permits available`` () =
    let sem = run (TxSemaphore.make 5)
    Assert.True(run (TxSemaphore.tryAcquireN sem 3))
    Assert.Equal(2, run (TxSemaphore.available sem))

[<Fact>]
let ``tryAcquireN fails when not enough permits available`` () =
    let sem = run (TxSemaphore.make 2)
    Assert.False(run (TxSemaphore.tryAcquireN sem 3))
    Assert.Equal(2, run (TxSemaphore.available sem))

// --- edge cases ---

[<Fact>]
let ``acquireN with zero permits causes defect`` () =
    let sem = run (TxSemaphore.make 5)
    assertDies (TxSemaphore.acquireN sem 0)

[<Fact>]
let ``acquireN with negative permits causes defect`` () =
    let sem = run (TxSemaphore.make 5)
    assertDies (TxSemaphore.acquireN sem -1)

[<Fact>]
let ``releaseN with zero permits causes defect`` () =
    let sem = run (TxSemaphore.make 5)
    assertDies (TxSemaphore.releaseN sem 0)

[<Fact>]
let ``release does not exceed capacity`` () =
    let sem = run (TxSemaphore.make 3)
    run (TxSemaphore.release sem)
    run (TxSemaphore.release sem)
    Assert.Equal(3, run (TxSemaphore.available sem))

[<Fact>]
let ``releaseN does not exceed capacity`` () =
    let sem = run (TxSemaphore.make 5)
    run (TxSemaphore.releaseN sem 10)
    Assert.Equal(5, run (TxSemaphore.available sem))

// --- transactional behavior ---

[<Fact>]
let ``net effect of acquire acquire release is minus one`` () =
    let sem = run (TxSemaphore.make 5)
    run (TxSemaphore.acquire sem)
    run (TxSemaphore.acquire sem)
    run (TxSemaphore.release sem)
    Assert.Equal(4, run (TxSemaphore.available sem))

// --- scoped / wrapped usage ---

[<Fact>]
let ``withPermit acquires for the effect and releases after`` () =
    let sem = run (TxSemaphore.make 2)
    let inside = run (TxSemaphore.withPermit sem (TxSemaphore.available sem))
    Assert.Equal(1, inside)
    Assert.Equal(2, run (TxSemaphore.available sem))

[<Fact>]
let ``withPermits acquires multiple and releases after`` () =
    let sem = run (TxSemaphore.make 5)
    let inside = run (TxSemaphore.withPermits sem 3 (TxSemaphore.available sem))
    Assert.Equal(2, inside)
    Assert.Equal(5, run (TxSemaphore.available sem))

[<Fact>]
let ``withPermit releases even when the effect fails`` () =
    let sem = run (TxSemaphore.make 2)

    match Effect.runSync () (TxSemaphore.withPermit sem (Effect.fail "boom")) with
    | Failure cause -> Assert.Equal<string list>([ "boom" ], Cause.failures cause)
    | Success _ -> Assert.Fail "expected failure"

    Assert.Equal(2, run (TxSemaphore.available sem))

[<Fact>]
let ``withPermitScoped releases the permit when the scope closes`` () =
    let sem = run (TxSemaphore.make 3)

    let inside =
        run (
            Scope.scoped (fun scope ->
                effect {
                    do! TxSemaphore.withPermitScoped sem scope
                    return! TxSemaphore.available sem
                })
        )

    Assert.Equal(2, inside)
    Assert.Equal(3, run (TxSemaphore.available sem))
