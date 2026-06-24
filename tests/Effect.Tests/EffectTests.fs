module Effect.Tests.EffectTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/Effect.test.ts (slice 1 subset).
// Async/fiber-heavy cases land as their slices do; see PORTING.md.

type DivError = DivByZero
type Config = { Factor: int }

let private safeDiv a b : Effect<int, DivError, 'R> =
    if b = 0 then Effect.fail DivByZero else Effect.succeed (a / b)

[<Fact>]
let ``succeed yields the value`` () =
    Assert.Equal<Result<int, DivError>>(Ok 42, Effect.run () (Effect.succeed 42))

[<Fact>]
let ``fail yields the typed error`` () =
    Assert.Equal<Result<int, DivError>>(Error DivByZero, Effect.run () (Effect.fail DivByZero))

[<Fact>]
let ``map transforms success`` () =
    Assert.Equal<Result<int, DivError>>(Ok 42, Effect.run () (Effect.succeed 21 |> Effect.map ((*) 2)))

[<Fact>]
let ``flatMap sequences on success`` () =
    let e = Effect.succeed 84 |> Effect.flatMap (fun x -> safeDiv x 2)
    Assert.Equal<Result<int, DivError>>(Ok 42, Effect.run () e)

[<Fact>]
let ``flatMap short-circuits on error`` () =
    let e = Effect.succeed 84 |> Effect.flatMap (fun x -> safeDiv x 0)
    Assert.Equal<Result<int, DivError>>(Error DivByZero, Effect.run () e)

[<Fact>]
let ``catchAll recovers from a typed error`` () =
    let e = Effect.fail DivByZero |> Effect.catchAll (fun _ -> Effect.succeed -1)
    Assert.Equal<Result<int, DivError>>(Ok -1, Effect.run () e)

[<Fact>]
let ``environment injects dependencies`` () =
    let program : Effect<int, DivError, Config> =
        Effect.environmentWith (fun c -> c.Factor) |> Effect.map ((*) 10)
    Assert.Equal<Result<int, DivError>>(Ok 50, Effect.run { Factor = 5 } program)

[<Fact>]
let ``effect builder threads value and environment`` () =
    let ce : Effect<int, DivError, Config> =
        effect {
            let! cfg = Effect.environment<Config, DivError>
            let! d = safeDiv 100 cfg.Factor
            return d + 1
        }
    Assert.Equal<Result<int, DivError>>(Ok 26, Effect.run { Factor = 4 } ce)

[<Fact>]
let ``effect builder propagates typed failure`` () =
    let ce : Effect<int, DivError, Config> =
        effect {
            let! cfg = Effect.environment<Config, DivError>
            let! d = safeDiv 100 (cfg.Factor - cfg.Factor)
            return d
        }
    Assert.Equal<Result<int, DivError>>(Error DivByZero, Effect.run { Factor = 4 } ce)
