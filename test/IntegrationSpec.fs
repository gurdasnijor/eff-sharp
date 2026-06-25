module IntegrationSpec

open Effect
open Effect.Vitest

type private Multiplier = { Factor: int }

describe "Integration" (fun () ->
    test "async core + Context + Ref + effect CE compose end-to-end" (fun () ->
        let tag = Tag.make<Multiplier> "multiplier"

        let program: Effect<int, string, Context> =
            effect {
                let! cfg = Effect.service tag
                let! counter = Ref.make 0

                for i in 1..5 do
                    do! Ref.update counter (fun n -> n + i)

                let! sum = Ref.get counter
                return sum * cfg.Factor
            }

        let provided = Effect.provideContext (Context.make tag { Factor = 3 }) program
        toBe (Effect.runSync () provided = Exit.succeed 45) true)

    test "Scope runs finalizers on success" (fun () ->
        let log = System.Collections.Generic.List<string>()

        let program: Effect<int, string, unit> =
            Scope.scoped (fun scope ->
                effect {
                    do! Scope.addFinalizer scope (Effect.sync (fun () -> log.Add "released"))
                    return 42
                })

        toBe (Effect.runSync () program = Exit.succeed 42) true
        toBe (System.String.Join(",", log)) "released")

    test "Scope runs finalizers when the body fails" (fun () ->
        let log = System.Collections.Generic.List<string>()

        let program: Effect<int, string, unit> =
            Scope.scoped (fun scope ->
                effect {
                    do! Scope.addFinalizer scope (Effect.sync (fun () -> log.Add "released"))
                    let! value = Effect.fail "boom"
                    return value
                })

        toBe (Effect.runSync () program = Exit.fail "boom") true
        toBe (System.String.Join(",", log)) "released")

    test "Deferred.make allocates completes and awaits inside a typed effect block" (fun () ->
        let program: Effect<int, string, unit> =
            effect {
                let! deferred = Deferred.make ()
                let! _ = Deferred.succeed deferred 99
                let! value = Deferred.await deferred
                return value
            }

        toBe (Effect.runSync () program = Exit.succeed 99) true))
