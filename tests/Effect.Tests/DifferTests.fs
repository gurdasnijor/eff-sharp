module Effect.Tests.DifferTests

open Xunit
open Effect

// No upstream test file exists (repos/effect-smol/.../test/Differ.test.ts is
// absent); Differ.ts ships only the model interface. These smoke tests exercise
// the ported record/constructor with a simple additive integer differ where a
// patch is the delta between values.

let private IntDiffer : Differ<int, int> =
    Differ.make 0 (fun oldV newV -> newV - oldV) (+) (+)

[<Fact>]
let ``empty patch is a no-op`` () =
    Assert.Equal(5, IntDiffer.patch 5 IntDiffer.empty)

[<Fact>]
let ``diff then patch round-trips`` () =
    let p = IntDiffer.diff 3 10
    Assert.Equal(7, p)
    Assert.Equal(10, IntDiffer.patch 3 p)

[<Fact>]
let ``combine composes patches in order`` () =
    let p1 = IntDiffer.diff 0 2
    let p2 = IntDiffer.diff 0 3
    let combined = IntDiffer.combine p1 p2
    Assert.Equal(5, combined)
    Assert.Equal(15, IntDiffer.patch 10 combined)
