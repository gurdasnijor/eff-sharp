module Effect.Tests.RedactableTests

open Xunit
open Effect

// No upstream Redactable.test.ts exists; these cover the protocol surface
// exercised via repos/effect-smol/packages/effect/test/Formatter.test.ts and the
// Redactable.ts doc examples (masking an API key).

type private ApiKey(raw: string) =
    interface IRedactable with
        member _.Redact() = box (raw.Substring(0, 4) + "...")

[<Fact>]
let ``isRedactable narrows IRedactable values`` () =
    Assert.True(Redactable.isRedactable (box (ApiKey "1234567890")))
    Assert.False(Redactable.isRedactable (box 5))
    Assert.False(Redactable.isRedactable (box "not redactable"))

[<Fact>]
let ``redact returns the redacted representation`` () =
    Assert.Equal<obj>(box "1234...", Redactable.redact (box (ApiKey "1234567890")))

[<Fact>]
let ``redact leaves non-redactable values unchanged`` () =
    Assert.Equal<obj>(box 5, Redactable.redact (box 5))

[<Fact>]
let ``getRedacted reads from a known IRedactable`` () =
    let key = ApiKey "abcdefgh"
    Assert.Equal<obj>(box "abcd...", Redactable.getRedacted (key :> IRedactable))
