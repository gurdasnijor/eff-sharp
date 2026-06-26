module MatchSpec

open Effect
open Effect.Vitest

describe "Match" (fun () ->
    test "first runs cases in order and falls back" (fun () ->
        let classify =
            Match.first
                [ Match.``when`` (Match.literal "claude") (fun _ -> "npx claude")
                  Match.``when`` (Match.literal "codex") (fun _ -> "npx codex") ]
                (fun unknown -> "unknown:" + unknown)

        toBe (classify "codex") "npx codex"
        toBe (classify "other") "unknown:other")

    test "dynamic predicates classify boxed primitives" (fun () ->
        toBe (Match.string (box "x")) true
        toBe (Match.number (box 1.0)) true
        toBe (Match.boolean (box true)) true
        toBe (Match.string (box 1)) false))
