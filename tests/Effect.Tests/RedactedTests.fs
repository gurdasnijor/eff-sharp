module Effect.Tests.RedactedTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/Redacted.test.ts
// Equal/Hash assertions use F#'s built-in structural equality (the Redacted type
// overrides Equals/GetHashCode), per the leaf6 decoupling rule.

[<Fact>]
let ``constructor wraps a structurally-equal value`` () =
    let a = Redacted.make (List.ofSeq "redacted")
    let b = Redacted.make (List.ofSeq "redacted")
    Assert.True((a = b))

[<Fact>]
let ``value recovers the protected value`` () =
    let r = Redacted.make (List.ofSeq "redacted")
    Assert.Equal<char list>(List.ofSeq "redacted", Redacted.value r)

[<Fact>]
let ``pipe supports extracting the protected value`` () =
    let value = {| asd = 123 |}
    let r = Redacted.make value
    let extracted = r |> Redacted.value
    Assert.Equal(value, extracted)

[<Fact>]
let ``toString returns a redacted placeholder`` () =
    let r = Redacted.make "redacted"
    Assert.Equal("<redacted>", string r)

[<Fact>]
let ``toJSON serializes the redacted placeholder`` () =
    let r = Redacted.make "redacted"
    Assert.Equal("<redacted>", Redacted.toJSON r)

[<Fact>]
let ``label appears in redacted output and wipe errors`` () =
    let r = Redacted.makeWith "MY_LABEL" "redacted"
    Assert.Equal(Some "MY_LABEL", r.Label)
    Assert.Equal("<redacted:MY_LABEL>", r.ToString())
    Assert.Equal("<redacted:MY_LABEL>", Redacted.toJSON r)
    Assert.True(Redacted.wipeUnsafe r)
    let ex = Assert.Throws<exn>(fun () -> Redacted.value r |> ignore)
    Assert.Contains("Unable to get redacted value with label: \"MY_LABEL\"", ex.Message)

[<Fact>]
let ``wipeUnsafe removes access to the protected value`` () =
    let r = Redacted.make "redacted"
    Assert.True(Redacted.wipeUnsafe r)
    let ex = Assert.Throws<exn>(fun () -> Redacted.value r |> ignore)
    Assert.Contains("Unable to get redacted value", ex.Message)

[<Fact>]
let ``equality compares the protected values`` () =
    Assert.True((Redacted.make 1 = Redacted.make 1))
    Assert.False((Redacted.make 1 = Redacted.make 2))

[<Fact>]
let ``hash is derived from the protected value`` () =
    Assert.Equal((Redacted.make 1).GetHashCode(), (Redacted.make 1).GetHashCode())
    Assert.NotEqual((Redacted.make 1).GetHashCode(), (Redacted.make 2).GetHashCode())

[<Fact>]
let ``makeEquivalence compares through an equivalence without exposing values`` () =
    let eq = Redacted.makeEquivalence (=)
    Assert.False(eq (Redacted.make "1234567890") (Redacted.make "1-34567890"))
    Assert.True(eq (Redacted.make "1234567890") (Redacted.make "1234567890"))
