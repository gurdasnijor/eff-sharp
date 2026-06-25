module ResourceSpec

open Effect
open Effect.Vitest

let private run (eff: Effect<'A, 'E, unit>) : 'A =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure cause -> failwithf "unexpected failure: %s" (Cause.render cause)

describe "Resource" (fun () ->
    test "manual refresh updates the cached value" (fun () ->
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

        toBe r1 0
        toBe r2 1)

    test "manual get surfaces a failed acquisition" (fun () ->
        let outcome =
            Effect.runSync
                ()
                (Scope.scoped (fun parent ->
                    effect {
                        let! resource = Resource.manual parent (Effect.fail "boom": Effect<int, string, unit>)
                        return! Resource.get resource
                    }))

        match outcome with
        | Failure cause -> toBe (Cause.failures cause = [ "boom" ]) true
        | Success value -> failwithf "expected failure, got %d" value))
