/// @title Creating and composing effects (effect { } vs combinators)
///
/// An `Effect<'A, 'E, 'R>` is a *description* of a computation that, when run,
/// either succeeds with an `'A`, fails with a typed error `'E`, or needs an
/// environment `'R`. Nothing happens until you run it (see `05_Running`).
///
/// This is the eff-sharp answer to upstream `01_effect/01_basics`. Where Effect
/// (TypeScript) only has `Effect.gen(function* () { ... })`, eff-sharp gives you a
/// real F# computation expression: `effect { let! x = ...; return ... }`. It
/// desugars to the very same `flatMap`/`map` calls, so you can mix the two freely.
module EffSharp.AiDocs.Basics

open Effect
open EffSharp.AiDocs.Shared

// ---------------------------------------------------------------------------
// Creating effects — the constructors (mirrors 10_creating-effects.ts)
// ---------------------------------------------------------------------------

/// `succeed` lifts a pure value. The error/env channels stay polymorphic.
let pureValue: Effect<int, string, unit> = Effect.succeed 42

/// `fail` puts a *typed* error in the error channel — not an exception. The
/// compiler now tracks that this computation can fail with a `string`.
let boom: Effect<int, string, unit> = Effect.fail "nope"

/// `sync` defers a side-effecting thunk; exceptions become *defects* (the `Die`
/// channel) rather than typed failures.
let now: Effect<System.DateTime, string, unit> =
    Effect.sync (fun () -> System.DateTime.UtcNow)

// ---------------------------------------------------------------------------
// Composing with combinators (the lingua franca — works in any language)
// ---------------------------------------------------------------------------

/// Classic pipe style: `map`/`flatMap` threaded with `|>`. This is the closest
/// to Effect's TS authoring, and it reads fine for short chains.
let withCombinators: Effect<int, string, unit> =
    Effect.succeed 10
    |> Effect.map (fun x -> x + 1)
    |> Effect.flatMap (fun x -> Effect.succeed (x * 2))

// ---------------------------------------------------------------------------
// Composing with the `effect { }` computation expression  ⭐ F# edge
// ---------------------------------------------------------------------------

/// The same computation as `withCombinators`, but as do-notation. `let!` is
/// `flatMap`; the final `return` is `succeed`. For multi-step logic this reads
/// top-to-bottom like ordinary code — no nested callbacks, no `function*`.
let withCE: Effect<int, string, unit> =
    effect {
        let! a = Effect.succeed 10
        let b = a + 1 // ordinary `let` for pure steps — no lifting needed
        let! c = Effect.succeed (b * 2)
        return c
    }

/// `effect { }` is a *superset* of `Effect.gen`: it also supports native F#
/// control flow that lowers to the right combinators.
///   - `for x in xs do` -> `Effect.forEach`
///   - `if/then`, `match` -> ordinary branching that yields effects
let sumFirstN (n: int) : Effect<int, string, unit> =
    effect {
        let! acc = Ref.make 0 // a tiny mutable cell (see 02_Services for services)

        // `for` in the CE sequences an effect per element, short-circuiting on error.
        for i in 1..n do
            do! Ref.update acc (fun s -> s + i)

        return! Ref.get acc
    }

/// `match` inside `effect { }` is just F# pattern matching producing effects —
/// no special `Effect.if`/`Effect.match` combinator needed.
let classify (x: int) : Effect<string, string, unit> =
    effect {
        match x with
        | 0 -> return "zero"
        | n when n < 0 -> return "negative"
        | _ -> return "positive"
    }

let demo () =
    banner "01 · Basics — effect { } vs combinators"
    printfn "  pureValue        = %d" (run pureValue)
    printfn "  withCombinators  = %d" (run withCombinators)
    printfn "  withCE           = %d" (run withCE)
    printfn "  sumFirstN 5      = %d" (run (sumFirstN 5))
    printfn "  classify -3      = %s" (run (classify -3))
