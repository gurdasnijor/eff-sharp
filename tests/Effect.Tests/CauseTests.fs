module Effect.Tests.CauseTests

open Xunit

// Port target: repos/effect-smol/packages/effect/test/Cause.test.ts
// Pending until slice 2 (Cause/Exit) lands. Kept as a red->green marker.

[<Fact(Skip = "slice 2: Cause not yet implemented")>]
let ``models typed failures and defects`` () =
    Assert.True(false)
