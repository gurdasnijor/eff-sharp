/// @title Running effects (Exit, sync, async)
///
/// Effects are inert descriptions. A *runner* at the edge of your program turns
/// one into a real result. eff-sharp offers:
///   - `Effect.runSync env eff : Exit<'A,'E>`   — run now, get the full outcome
///   - `Effect.runExit env eff : Async<Exit>`   — same, but async
///   - `Effect.runAsync env eff : Async<'A>`    — async, *throws* on failure
///
/// Mirrors upstream `01_effect/05_running`. The `Exit`/`Cause` returned by the
/// non-throwing runners are plain DUs, so you inspect them with `match` — the
/// idiomatic alternative to `Exit.match`.
module EffSharp.AiDocs.Running

open Effect
open EffSharp.AiDocs.Shared

let private succeeds: Effect<int, string, unit> =
    Effect.succeed 21 |> Effect.map (fun x -> x * 2)

let private fails: Effect<int, string, unit> = Effect.fail "boom"

let demo () =
    banner "05 · Running — Exit / sync / async"

    // `runSync` hands back an `Exit`; pattern-match both arms — total, no try/catch.
    match Effect.runSync () succeeds with
    | Success value -> printfn "  runSync success => %d" value
    | Failure cause -> printfn "  runSync failure => %s" (Cause.render cause)

    match Effect.runSync () fails with
    | Success value -> printfn "  unexpected %d" value
    | Failure cause ->
        // A typed failure shows up as a single `Reason.Fail`.
        match Cause.failures cause with
        | [ msg ] -> printfn "  runSync failure => typed error %A" msg
        | _ -> printfn "  runSync failure => %s" (Cause.render cause)

    // `runAsync` is for the async edge; it RAISES on failure (use when you want
    // exceptions to propagate to a host, e.g. ASP.NET). Here we only run the
    // happy path.
    let value = Effect.runAsync () succeeds |> Async.RunSynchronously
    printfn "  runAsync success => %d" value
