module Effect.Tests.ExitTests

open Xunit

// Port target: repos/effect-smol/packages/effect/test/Exit.test.ts
// Pending until slice 2 (Cause/Exit) lands. Kept as a red->green marker.

[<Fact(Skip = "slice 2: Exit not yet implemented")>]
let ``toString renders Success and Failure variants`` () =
    // Expected upstream behaviour:
    //   Exit.succeed(1).toString()    === "Success(1)"
    //   Exit.fail("error").toString() === "Failure(Cause([Fail(\"error\")]))"
    //   Exit.die("error").toString()  === "Failure(Cause([Die(\"error\")]))"
    //   Exit.interrupt(1).toString()  === "Failure(Cause([Interrupt(1)]))"
    Assert.True(false)
