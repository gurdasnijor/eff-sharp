module Effect.Tests.JsonPointerTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/JsonPointer.test.ts

[<Fact>]
let ``escapeToken escapes tilde to ~0`` () =
    Assert.Equal("a~0b", JsonPointer.escapeToken "a~b")
    Assert.Equal("~0", JsonPointer.escapeToken "~")
    Assert.Equal("~0~0", JsonPointer.escapeToken "~~")

[<Fact>]
let ``escapeToken escapes slash to ~1`` () =
    Assert.Equal("a~1b", JsonPointer.escapeToken "a/b")
    Assert.Equal("~1", JsonPointer.escapeToken "/")
    Assert.Equal("~1~1", JsonPointer.escapeToken "//")

[<Fact>]
let ``escapeToken escapes both tilde and slash`` () =
    Assert.Equal("a~0b~1c", JsonPointer.escapeToken "a~b/c")
    Assert.Equal("path~1to~0key", JsonPointer.escapeToken "path/to~key")
    Assert.Equal("~0a~1b~0c~1d~0", JsonPointer.escapeToken "~a/b~c/d~")

[<Fact>]
let ``escapeToken replaces ~ before / (order of operations)`` () =
    Assert.Equal("a~0~1b", JsonPointer.escapeToken "a~/b")
    Assert.Equal("a~1~0b", JsonPointer.escapeToken "a/~b")
    Assert.Equal("~0~1", JsonPointer.escapeToken "~/")
    Assert.Equal("~1~0", JsonPointer.escapeToken "/~")

[<Fact>]
let ``escapeToken handles confusable sequences`` () =
    Assert.Equal("~001", JsonPointer.escapeToken "~01")
    Assert.Equal("~010", JsonPointer.escapeToken "~10")
    Assert.Equal("~000", JsonPointer.escapeToken "~00")
    Assert.Equal("~011", JsonPointer.escapeToken "~11")

[<Fact>]
let ``escapeToken edge cases and passthrough`` () =
    Assert.Equal("", JsonPointer.escapeToken "")
    Assert.Equal("abc", JsonPointer.escapeToken "abc")
    Assert.Equal("héllo", JsonPointer.escapeToken "héllo")
    Assert.Equal("世界", JsonPointer.escapeToken "世界")
    Assert.Equal("a.b", JsonPointer.escapeToken "a.b")

[<Fact>]
let ``unescapeToken unescapes ~0 to tilde`` () =
    Assert.Equal("a~b", JsonPointer.unescapeToken "a~0b")
    Assert.Equal("~", JsonPointer.unescapeToken "~0")
    Assert.Equal("~~", JsonPointer.unescapeToken "~0~0")

[<Fact>]
let ``unescapeToken unescapes ~1 to slash`` () =
    Assert.Equal("a/b", JsonPointer.unescapeToken "a~1b")
    Assert.Equal("/", JsonPointer.unescapeToken "~1")
    Assert.Equal("//", JsonPointer.unescapeToken "~1~1")

[<Fact>]
let ``unescapeToken replaces ~1 before ~0 (order of operations)`` () =
    Assert.Equal("a/b~c", JsonPointer.unescapeToken "a~1b~0c")
    Assert.Equal("~1", JsonPointer.unescapeToken "~01")
    Assert.Equal("/0", JsonPointer.unescapeToken "~10")
    Assert.Equal("~0", JsonPointer.unescapeToken "~00")
    Assert.Equal("/1", JsonPointer.unescapeToken "~11")

[<Fact>]
let ``unescapeToken edge cases and passthrough`` () =
    Assert.Equal("", JsonPointer.unescapeToken "")
    Assert.Equal("abc", JsonPointer.unescapeToken "abc")
    Assert.Equal("héllo~world/test", JsonPointer.unescapeToken "héllo~0world~1test")

[<Fact>]
let ``round-trip unescape(escape(token)) = token`` () =
    let cases =
        [ "abc"; "a~b"; "a/b"; "a~b/c"; "path/to~key"; "name/alias"; "~"; "/"; "~~"; "//"; "~/"; "/~"
          ""; "~01"; "~10"; "~00"; "~11"; "a~01b"; "a~10b"; "héllo"; "世界"; "🚀"; "héllo~world/test"
          "世界/🌍~key"; "a~b/c~d/e"; "~a/b~c/d~" ]
    for token in cases do
        let escaped = JsonPointer.escapeToken token
        Assert.Equal(token, JsonPointer.unescapeToken escaped)
