module Effect.Tests.FormatterTests

open System
open System.Text.RegularExpressions
open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/Formatter.test.ts.
// JS-only inputs (undefined, symbol, function, null-prototype object, FormData,
// typed arrays, Schema.Class/ErrorClass, Context.Service) have no F# analogue and
// are not ported; .NET equivalents are substituted where they exist (anonymous
// record = JS object literal, named record = class instance, F# array/list = JS
// array, F# Map/Set = JS Map/Set, BigInteger = bigint, Exception = Error).

/// A value carrying its redacted form (test/Formatter.test.ts `SensitiveData`).
type private SensitiveData(secret: string) =
    interface IRedactable with
        member _.Redact() = box {| secret = "[REDACTED]" |}

type private CustomToString() =
    override _.ToString() = "custom"

let private data = SensitiveData("my-secret-key")

[<Fact>]
let ``null`` () = Assert.Equal("null", Formatter.format null)

[<Fact>]
let ``string`` () = Assert.Equal("\"a\"", Formatter.format (box "a"))

[<Fact>]
let ``number`` () = Assert.Equal("123", Formatter.format (box 123))

[<Fact>]
let ``float`` () = Assert.Equal("1.5", Formatter.format (box 1.5))

[<Fact>]
let ``boolean`` () = Assert.Equal("true", Formatter.format (box true))

[<Fact>]
let ``bigint`` () = Assert.Equal("123n", Formatter.format (box (System.Numerics.BigInteger 123)))

[<Fact>]
let ``custom toString method`` () = Assert.Equal("custom", Formatter.format (box (CustomToString())))

[<Fact>]
let ``array`` () =
    let arr : obj[] = [| box 1; box 2; box (System.Numerics.BigInteger 3) |]
    Assert.Equal("[1,2,3n]", Formatter.format (box arr))

[<Fact>]
let ``F# list`` () = Assert.Equal("[1,2,3]", Formatter.format (box [ 1; 2; 3 ]))

[<Fact>]
let ``circular array`` () =
    let arr : obj[] = Array.zeroCreate 2
    arr.[0] <- box 1
    arr.[1] <- box arr
    Assert.Equal("[1,[Circular]]", Formatter.format (box arr))

[<Fact>]
let ``object (anonymous record)`` () =
    Assert.Equal("{\"a\":1}", Formatter.format (box {| a = 1 |}))
    Assert.Equal("{\"a\":1,\"b\":2}", Formatter.format (box {| a = 1; b = 2 |}))
    let b : obj[] = [| box 1; box 2; box (System.Numerics.BigInteger 3) |]
    Assert.Equal("{\"a\":1,\"b\":[1,2,3n]}", Formatter.format (box {| a = 1; b = b |}))

type private Foo = { a: int }

[<Fact>]
let ``named record wraps with the type name`` () =
    Assert.Equal("Foo({\"a\":1})", Formatter.format (box { a = 1 }))

[<Fact>]
let ``RegExp`` () = Assert.Equal("/a/", Formatter.format (box (Regex "a")))

[<Fact>]
let ``Set`` () = Assert.Equal("Set([1,2,3])", Formatter.format (box (Set [ 1; 2; 3 ])))

[<Fact>]
let ``Map`` () =
    Assert.Equal("Map([[\"a\",1],[\"b\",2]])", Formatter.format (box (Map [ ("a", 1); ("b", 2) ])))

[<Fact>]
let ``Date`` () =
    let d = DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)
    Assert.Equal("1970-01-01T00:00:00.000Z", Formatter.format (box d))

[<Fact>]
let ``Redacted`` () = Assert.Equal("<redacted>", Formatter.format (box (Redacted.make "a")))

[<Fact>]
let ``Error`` () =
    Assert.Equal("Exception: a", Formatter.format (box (Exception "a")))
    // Upstream a JS Error cause is any value; .NET's InnerException is itself an
    // exception, so the cause renders as a formatted exception.
    Assert.Equal("Exception: a (cause: Exception: b)",
                 Formatter.format (box (Exception("a", Exception "b"))))

[<Fact>]
let ``Option (discriminated union)`` () =
    Assert.Equal("Some(1)", Formatter.format (box (Some 1)))
    // Divergence: F# `None` is a null reference at runtime, so it renders as
    // `null` rather than upstream's `none()`.
    Assert.Equal("null", Formatter.format (box (None: int option)))

[<Fact>]
let ``whitespace object`` () =
    Assert.Equal("{\"a\":1}", Formatter.formatWith 2 (box {| a = 1 |}))
    Assert.Equal("{\n  \"a\": 1,\n  \"b\": 2\n}", Formatter.formatWith 2 (box {| a = 1; b = 2 |}))
    Assert.Equal("{\n  \"a\": 1,\n  \"b\": [\n    1,\n    2,\n    3n\n  ]\n}",
                 Formatter.formatWith 2 (box {| a = 1; b = ([| box 1; box 2; box (System.Numerics.BigInteger 3) |]) |}))

[<Fact>]
let ``should redact sensitive data`` () =
    Assert.Equal("{\"secret\":\"[REDACTED]\"}", Formatter.format (box data))
    Assert.Equal("{\"a\":{\"secret\":\"[REDACTED]\"}}", Formatter.format (box {| a = data |}))

// --- formatJson ---------------------------------------------------------

/// A mutable object so a genuine reference cycle can be constructed. Public so
/// its properties are visible to cross-assembly reflection in `formatJson`.
type JsonCircular() =
    member val a = 1 with get, set
    member val self : obj = null with get, set

[<Fact>]
let ``formatJson omits circular references`` () =
    let obj = JsonCircular()
    obj.self <- box obj
    Assert.Equal("{\"a\":1}", Formatter.formatJson (box obj))

[<Fact>]
let ``formatJson preserves repeated non-circular references`` () =
    let shared = {| a = 1 |}
    Assert.Equal("{\"left\":{\"a\":1},\"right\":{\"a\":1}}",
                 Formatter.formatJson (box {| left = shared; right = shared |}))

[<Fact>]
let ``formatJson redacts sensitive data`` () =
    Assert.Equal("{\"secret\":\"[REDACTED]\"}", Formatter.formatJson (box data))
    Assert.Equal("{\"a\":{\"secret\":\"[REDACTED]\"}}", Formatter.formatJson (box {| a = data |}))

[<Fact>]
let ``formatPath renders bracket notation`` () =
    Assert.Equal("[\"a\"][\"b\"]", Formatter.formatPath [ "a"; "b" ])
