module ExitSpec

open Effect
open Effect.Vitest

describe "Exit" (fun () ->
    test "render renders Success and Failure variants" (fun () ->
        toBe (Exit.render (Exit.succeed 1)) "Success(1)"
        toBe (Exit.render (Exit.fail "error")) "Failure(Cause([Fail(\"error\")]))"
        toBe (Exit.render (Exit.die (box "error"))) "Failure(Cause([Die(\"error\")]))"
        toBe (Exit.render (Exit.interrupt (Some 1))) "Failure(Cause([Interrupt(1)]))"
        toBe (Exit.render (Exit.interrupt None)) "Failure(Cause([Interrupt(undefined)]))")

    test "succeed is a success, fail is a failure" (fun () ->
        toBe (Exit.isSuccess (Exit.succeed 1)) true
        toBe (Exit.isFailure (Exit.succeed 1)) false
        toBe (Exit.isFailure (Exit.fail "boom")) true
        toBe (Exit.isSuccess (Exit.fail "boom")) false)

    test "match folds both branches" (fun () ->
        let label = Exit.matchExit (fun _ -> "failed") (sprintf "ok:%d") (Exit.succeed 7)
        toBe label "ok:7"
        let label2 = Exit.matchExit (fun _ -> "failed") (sprintf "ok:%d") (Exit.fail "x")
        toBe label2 "failed"))

