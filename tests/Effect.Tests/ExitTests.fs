module Effect.Tests.ExitTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/Exit.test.ts

[<Fact>]
let ``toString renders Success and Failure variants`` () =
    Assert.Equal("Success(1)", Exit.render (Exit.succeed 1))
    Assert.Equal("Failure(Cause([Fail(\"error\")]))", Exit.render (Exit.fail "error"))
    Assert.Equal("Failure(Cause([Die(\"error\")]))", Exit.render (Exit.die (box "error")))
    Assert.Equal("Failure(Cause([Interrupt(1)]))", Exit.render (Exit.interrupt (Some 1)))
    Assert.Equal("Failure(Cause([Interrupt(undefined)]))", Exit.render (Exit.interrupt None))

[<Fact>]
let ``succeed is a success, fail is a failure`` () =
    Assert.True(Exit.isSuccess (Exit.succeed 1))
    Assert.False(Exit.isFailure (Exit.succeed 1))
    Assert.True(Exit.isFailure (Exit.fail "boom"))
    Assert.False(Exit.isSuccess (Exit.fail "boom"))

[<Fact>]
let ``match folds both branches`` () =
    let label = Exit.matchExit (fun _ -> "failed") (sprintf "ok:%d") (Exit.succeed 7)
    Assert.Equal("ok:7", label)
    let label2 = Exit.matchExit (fun _ -> "failed") (sprintf "ok:%d") (Exit.fail "x")
    Assert.Equal("failed", label2)
