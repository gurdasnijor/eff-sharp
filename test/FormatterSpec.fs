module FormatterSpec

open Effect
open Effect.Vitest

type private SensitiveData(secret: string) =
    member _.Raw = secret

    interface IRedactable with
        member _.Redact() = box {| secret = "[REDACTED]" |}

describe "Formatter" (fun () ->
    test "formatPath renders bracket notation" (fun () ->
        toBe (Formatter.formatPath [ "a"; "b" ]) "[\"a\"][\"b\"]")

    test "formatJson redacts sensitive data" (fun () ->
        let data = SensitiveData("my-secret-key")
        toBe (Formatter.formatJson (box data)) "{\"secret\":\"[REDACTED]\"}"
        toBe (Formatter.formatJson (box {| a = data |})) "{\"a\":{\"secret\":\"[REDACTED]\"}}")

    test "formatJson preserves repeated non-circular references" (fun () ->
        let shared = {| a = 1 |}
        toBe (Formatter.formatJson (box {| left = shared; right = shared |})) "{\"left\":{\"a\":1},\"right\":{\"a\":1}}")

    test "formatDate renders UTC ISO strings" (fun () ->
        let d = System.DateTime(1970, 1, 1, 0, 0, 0, System.DateTimeKind.Utc)
        toBe (Formatter.formatDate d) "1970-01-01T00:00:00.000Z"))
