/// @title Shared demo helpers
///
/// Tiny utilities every topic file uses to *run* its examples and print results.
/// Kept separate so the topic files stay focused on the API being taught.
module EffSharp.AiDocs.Demo

open Effect

/// Print a labelled section header so `dotnet run` output is readable.
let section (title: string) : unit =
    printfn "\n──────────── %s ────────────" title

/// Run an effect to a value, rendering any failure `Cause` into an exception.
///
/// `Effect.runSync` returns an `Exit<'A,'E>` — a DU we destructure with a NATIVE
/// `match` instead of an `Exit.match*` combinator (CONVENTIONS §2: lower Effect's
/// matchers onto F# pattern matching). `Success`/`Failure` are the two cases.
let run (env: 'R) (eff: Effect<'A, 'E, 'R>) : 'A =
    match Effect.runSync env eff with
    | Success value -> value
    | Failure cause -> failwithf "example effect failed: %s" (Cause.render cause)

/// Run an effect whose environment is `unit` (no services required).
let runUnit (eff: Effect<'A, 'E, unit>) : 'A = run () eff
