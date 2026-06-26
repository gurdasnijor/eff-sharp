module OptionSpec

open Effect
open Effect.Vitest

describe "Option compatibility helpers" (fun () ->
    test "some/none and predicates use native F# option" (fun () ->
        let value = Option.some 42
        toBe (Option.isSome value) true
        toBe (Option.isNone value) false
        toBe (Option.isNone (Option.none<int> ())) true)

    test "match folds both cases" (fun () ->
        let someValue = Option.``match`` (fun () -> "none") (fun n -> sprintf "some:%d" n) (Some 3)
        let noneValue = Option.``match`` (fun () -> "none") (fun n -> sprintf "some:%d" n) None

        toBe someValue "some:3"
        toBe noneValue "none")

    test "nullable helpers map null to None" (fun () ->
        let missing: string = null
        toBe (Option.isNone (Option.fromNullable missing)) true
        toBe (Option.fromNullishOr "x") (Some "x")))
