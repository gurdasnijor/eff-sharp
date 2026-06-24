module Effect.Tests.CauseTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/Cause.test.ts
// (the structural subset; JS runtime guards isCause/isReason are omitted by
// design — see Cause.fs).

[<Fact>]
let ``TypeId constants match upstream`` () =
    Assert.Equal("~effect/Cause", Cause.TypeId)
    Assert.Equal("~effect/Cause/Reason", Cause.ReasonTypeId)

[<Fact>]
let ``empty has no reasons`` () =
    Assert.Empty(Cause.empty<string>.Reasons)

[<Fact>]
let ``fail creates a single Fail reason preserving the error`` () =
    let cause = Cause.fail "error"
    Assert.Equal(1, List.length cause.Reasons)
    Assert.True(Cause.isFailReason cause.Reasons.[0])
    Assert.Equal<string list>([ "error" ], Cause.failures cause)

[<Fact>]
let ``die creates a single Die reason preserving the defect`` () =
    let cause = Cause.die (box "defect")
    Assert.Equal(1, List.length cause.Reasons)
    Assert.True(Cause.isDieReason cause.Reasons.[0])
    Assert.Equal<obj list>([ box "defect" ], Cause.defects cause)

[<Fact>]
let ``interrupt creates a single Interrupt reason`` () =
    let cause = Cause.interrupt (Some 1)
    Assert.Equal(1, List.length cause.Reasons)
    Assert.True(Cause.isInterruptReason cause.Reasons.[0])

[<Fact>]
let ``reason guards narrow exactly one variant`` () =
    let fail = (Cause.fail "error").Reasons.[0]
    let die = (Cause.die (box "defect")).Reasons.[0]
    let intr = (Cause.interrupt (Some 1)).Reasons.[0]
    // Fail
    Assert.True(Cause.isFailReason fail)
    Assert.False(Cause.isDieReason fail)
    Assert.False(Cause.isInterruptReason fail)
    // Die
    Assert.True(Cause.isDieReason die)
    Assert.False(Cause.isFailReason die)
    Assert.False(Cause.isInterruptReason die)
    // Interrupt
    Assert.True(Cause.isInterruptReason intr)
    Assert.False(Cause.isFailReason intr)
    Assert.False(Cause.isDieReason intr)

[<Fact>]
let ``makeReason helpers build the matching variant`` () =
    Assert.True(Cause.isFailReason (Cause.makeFailReason "error"))
    Assert.True(Cause.isDieReason (Cause.makeDieReason (box "defect")))
    Assert.True(Cause.isInterruptReason (Cause.makeInterruptReason (Some 1)))

[<Fact>]
let ``render matches Effect's toString`` () =
    Assert.Equal("Cause([Fail(\"error\")])", Cause.render (Cause.fail "error"))
    Assert.Equal("Cause([Die(\"defect\")])", Cause.render (Cause.die (box "defect")))
    Assert.Equal("Cause([Interrupt(1)])", Cause.render (Cause.interrupt (Some 1)))
    Assert.Equal("Cause([Interrupt(undefined)])", Cause.render (Cause.interrupt None))
    Assert.Equal("Cause([])", Cause.render Cause.empty<string>)
