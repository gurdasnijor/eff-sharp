module Effect.Tests.ResourceTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/Resource.test.ts, adapted to
// the port's explicit-`Scope` model (Resource.manual/auto take the parent scope).
// Upstream drives `auto` deterministically with TestClock; this port has no
// TestClock, so the schedule-driven tests advance *real* time with `Effect.sleep`
// and a generous margin. The `isResource` guard is omitted (JS-runtime guard).

let private run (eff: Effect<'A, 'E, unit>) : 'A =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure c -> failwithf "unexpected failure: %s" (Cause.render c)

[<Fact>]
let ``manual refresh updates the cached value`` () =
    let r1, r2 =
        run (
            Scope.scoped (fun parent ->
                effect {
                    let! ref = Ref.make 0
                    let! resource = Resource.manual parent (Ref.get ref)
                    let! result1 = Resource.get resource
                    do! Ref.set ref 1
                    do! Resource.refresh resource
                    let! result2 = Resource.get resource
                    return (result1, result2)
                })
        )

    Assert.Equal(0, r1)
    Assert.Equal(1, r2)

[<Fact>]
let ``manual: get surfaces a failed acquisition`` () =
    let outcome =
        Effect.runSync
            ()
            (Scope.scoped (fun parent ->
                effect {
                    let! resource = Resource.manual parent (Effect.fail "boom": Effect<int, string, unit>)
                    return! Resource.get resource
                }))

    match outcome with
    | Failure cause -> Assert.Equal<string list>([ "boom" ], Cause.failures cause)
    | Success v -> Assert.Fail(sprintf "expected failure, got %d" v)

[<Fact>]
let ``auto refreshes the cached value on the schedule`` () =
    let r1, r2 =
        run (
            Scope.scoped (fun parent ->
                effect {
                    let! ref = Ref.make 0
                    let! resource = Resource.auto parent (Ref.get ref) (Schedule.spaced (Duration.millis 4.0))
                    let! result1 = Resource.get resource
                    do! Ref.set ref 1
                    do! Effect.sleep 80 // let the background fiber refresh several times
                    let! result2 = Resource.get resource
                    return (result1, result2)
                })
        )

    Assert.Equal(0, r1)
    Assert.Equal(1, r2)

[<Fact>]
let ``auto: a failed refresh does not affect the cached value`` () =
    let r1, r2 =
        run (
            Scope.scoped (fun parent ->
                effect {
                    let! ref = Ref.make (Effect.succeed 0: Effect<int, string, unit>)
                    let acquire = Ref.get ref |> Effect.flatMap (fun eff -> eff)
                    let! resource = Resource.auto parent acquire (Schedule.spaced (Duration.millis 4.0))
                    let! result1 = Resource.get resource
                    do! Ref.set ref (Effect.fail "Uh oh!")
                    do! Effect.sleep 80 // background refreshes now fail and are ignored
                    let! result2 = Resource.get resource
                    return (result1, result2)
                })
        )

    Assert.Equal(0, r1)
    Assert.Equal(0, r2) // previous successful value is retained
