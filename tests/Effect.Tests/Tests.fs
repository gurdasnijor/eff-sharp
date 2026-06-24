module Tests

open Effect

let mutable failures = 0
let check name (cond: bool) =
    if cond then printfn "  ok   %s" name
    else failures <- failures + 1; printfn "  FAIL %s" name

// Domain error + environment for the DI test
type DivError = DivByZero
type Config = { Factor: int }

[<EntryPoint>]
let main _ =
    printfn "Effect core (slice 1)"

    // succeed
    check "succeed" (Effect.run () (Effect.succeed 42) = Ok 42)

    // fail (typed error channel)
    check "fail" (Effect.run () (Effect.fail DivByZero) = Error DivByZero)

    // map
    check "map" (Effect.run () (Effect.succeed 21 |> Effect.map ((*) 2)) = Ok 42)

    // flatMap short-circuits on error
    let safeDiv a b : Effect<int, DivError, 'R> =
        if b = 0 then Effect.fail DivByZero else Effect.succeed (a / b)
    check "flatMap ok"  (Effect.run () (Effect.succeed 84 |> Effect.flatMap (fun x -> safeDiv x 2)) = Ok 42)
    check "flatMap err" (Effect.run () (Effect.succeed 84 |> Effect.flatMap (fun x -> safeDiv x 0)) = Error DivByZero)

    // catchAll recovers
    check "catchAll" (Effect.run () (Effect.fail DivByZero |> Effect.catchAll (fun _ -> Effect.succeed -1)) = Ok -1)

    // environment / DI
    let program : Effect<int, DivError, Config> =
        Effect.environmentWith (fun c -> c.Factor) |> Effect.map ((*) 10)
    check "environment DI" (Effect.run { Factor = 5 } program = Ok 50)

    // effect { } computation expression
    let ce : Effect<int, DivError, Config> =
        effect {
            let! cfg = Effect.environment<Config, DivError>
            let! d = safeDiv 100 cfg.Factor
            return d + 1
        }
    check "effect { } builder" (Effect.run { Factor = 4 } ce = Ok 26)

    // CE propagates typed failure
    let ceFail : Effect<int, DivError, Config> =
        effect {
            let! cfg = Effect.environment<Config, DivError>
            let! d = safeDiv 100 (cfg.Factor - cfg.Factor)
            return d
        }
    check "effect { } error propagation" (Effect.run { Factor = 4 } ceFail = Error DivByZero)

    printfn "%s" (if failures = 0 then "ALL PASS" else sprintf "%d FAILURE(S)" failures)
    failures
