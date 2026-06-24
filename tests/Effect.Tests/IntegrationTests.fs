module Effect.Tests.IntegrationTests

open Xunit
open Effect

// Cross-module composition tests — exercise the async core + Context (DI) + Ref +
// Deferred + Scope (finalizers) + the `effect { }` CE together, which no per-module
// unit test covers. This is the "does the car drive" check.

type Multiplier = { Factor: int }

[<Fact>]
let ``async core + Context + Ref + Deferred + effect CE compose end-to-end`` () =
    let tag = Tag.make<Multiplier> "multiplier"

    // NOTE: Deferred is intentionally exercised separately below, not in this CE —
    // `Deferred.make<'A,'E>`'s explicit type params suppress generalization of the
    // effect's error/env channels, so it can't be `let!`-bound in a typed
    // `effect { }` block (defaults to obj). Tracked as a health finding.
    let program: Effect<int, string, Context> =
        effect {
            let! cfg = Effect.service tag // dependency injection
            let! counter = Ref.make 0 // mutable state in the effect
            for i in 1..5 do
                do! Ref.update counter (fun n -> n + i) // CE `for` loop
            let! sum = Ref.get counter // 1+2+3+4+5 = 15
            return sum * cfg.Factor
        }

    let provided = Effect.provideContext (Context.make tag { Factor = 3 }) program
    Assert.Equal<Exit<int, string>>(Exit.succeed 45, Effect.runSync () provided)

[<Fact>]
let ``Scope runs finalizers on success`` () =
    let log = System.Collections.Generic.List<string>()

    let prog: Effect<int, string, unit> =
        Scope.scoped (fun scope ->
            effect {
                do! Scope.addFinalizer scope (Effect.sync (fun () -> log.Add "released"))
                return 42
            })

    let exit = Effect.runSync () prog
    Assert.Equal<Exit<int, string>>(Exit.succeed 42, exit)
    Assert.Equal<string>("released", System.String.Join(",", log))

[<Fact>]
let ``Scope runs finalizers even when the body fails (resource safety)`` () =
    let log = System.Collections.Generic.List<string>()

    let prog: Effect<int, string, unit> =
        Scope.scoped (fun scope ->
            effect {
                do! Scope.addFinalizer scope (Effect.sync (fun () -> log.Add "released"))
                let! v = Effect.fail "boom"
                return v
            })

    let exit = Effect.runSync () prog
    Assert.Equal<Exit<int, string>>(Exit.fail "boom", exit)
    // finalizer must still have run despite the typed failure
    Assert.Equal<string>("released", System.String.Join(",", log))

[<Fact>]
let ``Deferred.make allocates, completes and awaits inside a typed effect block`` () =
    // Regression for the P0 fix: `Deferred.make ()` is now `let!`-bindable in a
    // typed effect{} block (its explicit type params were dropped so the effect's
    // error/env channels generalize). The Deferred types are inferred from use.
    let prog: Effect<int, string, unit> =
        effect {
            let! d = Deferred.make ()
            let! _ = Deferred.succeed d 99
            let! v = Deferred.await d
            return v
        }

    Assert.Equal<Exit<int, string>>(Exit.succeed 99, Effect.runSync () prog)
