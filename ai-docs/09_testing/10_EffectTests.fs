/// @title Testing effects
///
/// Effects are values, so testing one is just: run it and inspect the `Exit`. No
/// special harness is required — drop these patterns inside an `[<Fact>]` (xUnit)
/// and assert with `Assert.*`. Here we use a tiny `check` helper so the docs
/// project stays framework-free.
///
/// Mirrors upstream `09_testing/10_effect-tests`. The "control time with
/// TestClock" idea becomes: model time as a *service* and inject a fixed clock —
/// deterministic by construction.
module EffSharp.AiDocs.EffectTests

open Effect
open EffSharp.AiDocs.Shared

// Run an effect and assert its success value (the most common shape).
let private expectSuccess (label: string) (expected: 'A) (eff: Effect<'A, 'E, unit>) =
    match Effect.runSync () eff with
    | Success actual -> check label (actual = expected)
    | Failure cause -> check (sprintf "%s (got failure %s)" label (Cause.render cause)) false

// Assert that an effect fails in the typed channel.
let private expectFailure (label: string) (eff: Effect<'A, 'E, unit>) =
    match Effect.runSync () eff with
    | Failure _ -> check label true
    | Success _ -> check label false

// --- a tiny clock service so time-dependent logic is testable ---------------
type Clock =
    { Millis: Effect<int64, string, Context> }

let clockTag: Tag<Clock> = Tag.make<Clock> "test/Clock"

/// Production logic that reads the wall clock through the service.
let elapsedSince (start: int64) : Effect<int64, string, Context> =
    effect {
        let! clock = Effect.service clockTag
        let! now = clock.Millis
        return now - start
    }

let demo () =
    banner "09 · Testing — run + assert on Exit"

    // 1. Plain success assertion.
    expectSuccess "map doubles the value" 84 (Effect.succeed 42 |> Effect.map (fun x -> x * 2))

    // 2. Typed-failure assertion.
    expectFailure "fail stays in the error channel" (Effect.fail "boom": Effect<int, string, unit>)

    // 3. Parameterized cases — just iterate (the analogue of `it.effect.each`).
    [ " Ada ", "ada"; " Lin ", "lin"; " Nia ", "nia" ]
    |> List.iter (fun (input, expected) ->
        let eff = Effect.sync (fun () -> input.Trim().ToLowerInvariant())
        expectSuccess (sprintf "normalize %s" (input.Trim())) expected eff)

    // 4. Deterministic time — inject a FIXED clock (the TestClock analogue).
    let fixedClock = { Millis = Effect.succeed 1000L }
    let deterministic = elapsedSince 250L |> Effect.provideService clockTag fixedClock
    expectSuccess "elapsed is deterministic with a fixed clock" 750L deterministic
