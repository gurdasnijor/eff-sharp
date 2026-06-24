module Effect.Tests.EncodingTests

open Xunit
open Effect

// Ported from the documented examples in
// repos/effect-smol/packages/effect/src/Encoding.ts (no upstream Encoding.test.ts
// exists). Cases come from the `@example` blocks plus the documented decode
// failure conditions.

let private bytes (xs: int list) : byte[] = xs |> List.map byte |> List.toArray

// --- Base64 ---

[<Fact>]
let ``encodeBase64 of string and bytes`` () =
    Assert.Equal("aGVsbG8=", Encoding.encodeBase64String "hello")
    Assert.Equal("SGVsbG8=", Encoding.encodeBase64 (bytes [ 72; 101; 108; 108; 111 ]))

[<Fact>]
let ``decodeBase64 round-trips`` () =
    match Encoding.decodeBase64 "SGVsbG8=" with
    | Ok b -> Assert.Equal<byte[]>(bytes [ 72; 101; 108; 108; 111 ], b)
    | Error e -> failwithf "expected success, got %A" e

[<Fact>]
let ``decodeBase64String decodes to text`` () =
    Assert.Equal(Ok "hello", Encoding.decodeBase64String "aGVsbG8=")

[<Fact>]
let ``decodeBase64 rejects bad length`` () =
    match Encoding.decodeBase64 "abc" with
    | Error e -> Assert.Contains("multiple of 4", e.Message)
    | Ok _ -> failwith "expected failure"

[<Fact>]
let ``decodeBase64 rejects misplaced padding`` () =
    match Encoding.decodeBase64 "a=bc" with
    | Error e -> Assert.Contains("not at the end", e.Message)
    | Ok _ -> failwith "expected failure"

// --- Base64Url ---

[<Fact>]
let ``encodeBase64Url is url-safe and unpadded`` () =
    Assert.Equal("aGVsbG8_", Encoding.encodeBase64UrlString "hello?")
    Assert.Equal("SGVsbG8_", Encoding.encodeBase64Url (bytes [ 72; 101; 108; 108; 111; 63 ]))

[<Fact>]
let ``decodeBase64Url accepts unpadded input`` () =
    match Encoding.decodeBase64Url "SGVsbG8_" with
    | Ok b -> Assert.Equal<byte[]>(bytes [ 72; 101; 108; 108; 111; 63 ], b)
    | Error e -> failwithf "expected success, got %A" e

[<Fact>]
let ``decodeBase64UrlString decodes to text`` () =
    Assert.Equal(Ok "hello?", Encoding.decodeBase64UrlString "aGVsbG8_")

[<Fact>]
let ``decodeBase64Url rejects invalid length`` () =
    match Encoding.decodeBase64Url "abcde" with
    | Error e -> Assert.Contains("multiple of 4", e.Message)
    | Ok _ -> failwith "expected failure"

// --- Hex ---

[<Fact>]
let ``encodeHex of string and bytes`` () =
    Assert.Equal("68656c6c6f", Encoding.encodeHexString "hello")
    Assert.Equal("48656c6c6f", Encoding.encodeHex (bytes [ 72; 101; 108; 108; 111 ]))

[<Fact>]
let ``decodeHex round-trips`` () =
    match Encoding.decodeHex "48656c6c6f" with
    | Ok b -> Assert.Equal<byte[]>(bytes [ 72; 101; 108; 108; 111 ], b)
    | Error e -> failwithf "expected success, got %A" e

[<Fact>]
let ``decodeHexString decodes to text`` () =
    Assert.Equal(Ok "hello", Encoding.decodeHexString "68656c6c6f")

[<Fact>]
let ``decodeHex rejects odd length`` () =
    match Encoding.decodeHex "abc" with
    | Error e -> Assert.Contains("multiple of 2", e.Message)
    | Ok _ -> failwith "expected failure"

[<Fact>]
let ``decodeHex rejects invalid characters`` () =
    match Encoding.decodeHex "zz" with
    | Error e -> Assert.Contains("Invalid input", e.Message)
    | Ok _ -> failwith "expected failure"

// --- round trips ---

[<Fact>]
let ``base64 round-trip over text`` () =
    let s = "The quick brown fox 🦊"
    Assert.Equal(Ok s, Encoding.decodeBase64String (Encoding.encodeBase64String s))
    Assert.Equal(Ok s, Encoding.decodeBase64UrlString (Encoding.encodeBase64UrlString s))
    Assert.Equal(Ok s, Encoding.decodeHexString (Encoding.encodeHexString s))
