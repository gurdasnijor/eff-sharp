module EncodingSpec

open Effect
open Effect.Vitest

let private bytes (xs: int list) : byte[] =
    xs |> List.map byte |> List.toArray

let private same actual expected =
    toBe (actual = expected) true

let private expectErrorContains substring result =
    match result with
    | Error e -> toContain e.Message substring
    | Ok _ -> failwith "expected decode failure"

describe "Encoding" (fun () ->
    test "base64 encodes strings and bytes" (fun () ->
        toBe (Encoding.encodeBase64String "hello") "aGVsbG8="
        toBe (Encoding.encodeBase64 (bytes [ 72; 101; 108; 108; 111 ])) "SGVsbG8=")

    test "base64 decodes and validates" (fun () ->
        match Encoding.decodeBase64 "SGVsbG8=" with
        | Ok b -> same b (bytes [ 72; 101; 108; 108; 111 ])
        | Error e -> failwithf "expected success, got %A" e

        same (Encoding.decodeBase64String "aGVsbG8=") (Ok "hello")
        expectErrorContains "multiple of 4" (Encoding.decodeBase64 "abc")
        expectErrorContains "not at the end" (Encoding.decodeBase64 "a=bc"))

    test "base64url encodes unpadded url-safe strings and bytes" (fun () ->
        toBe (Encoding.encodeBase64UrlString "hello?") "aGVsbG8_"
        toBe (Encoding.encodeBase64Url (bytes [ 72; 101; 108; 108; 111; 63 ])) "SGVsbG8_")

    test "base64url decodes and validates" (fun () ->
        match Encoding.decodeBase64Url "SGVsbG8_" with
        | Ok b -> same b (bytes [ 72; 101; 108; 108; 111; 63 ])
        | Error e -> failwithf "expected success, got %A" e

        same (Encoding.decodeBase64UrlString "aGVsbG8_") (Ok "hello?")
        expectErrorContains "multiple of 4" (Encoding.decodeBase64Url "abcde"))

    test "hex encodes strings and bytes" (fun () ->
        toBe (Encoding.encodeHexString "hello") "68656c6c6f"
        toBe (Encoding.encodeHex (bytes [ 72; 101; 108; 108; 111 ])) "48656c6c6f")

    test "hex decodes and validates" (fun () ->
        match Encoding.decodeHex "48656c6c6f" with
        | Ok b -> same b (bytes [ 72; 101; 108; 108; 111 ])
        | Error e -> failwithf "expected success, got %A" e

        same (Encoding.decodeHexString "68656c6c6f") (Ok "hello")
        expectErrorContains "multiple of 2" (Encoding.decodeHex "abc")
        expectErrorContains "Invalid input" (Encoding.decodeHex "zz"))

    test "text round-trips across all encodings" (fun () ->
        let s = "The quick brown fox \U0001F98A"
        same (Encoding.decodeBase64String (Encoding.encodeBase64String s)) (Ok s)
        same (Encoding.decodeBase64UrlString (Encoding.encodeBase64UrlString s)) (Ok s)
        same (Encoding.decodeHexString (Encoding.encodeHexString s)) (Ok s)))
