module RedactableSpec

open Effect
open Effect.Vitest

type private ApiKey(raw: string) =
    interface IRedactable with
        member _.Redact() = box (raw.Substring(0, 4) + "...")

describe "Redactable" (fun () ->
    test "isRedactable narrows IRedactable values" (fun () ->
        toBe (Redactable.isRedactable (box (ApiKey "1234567890"))) true
        toBe (Redactable.isRedactable (box 5)) false
        toBe (Redactable.isRedactable (box "not redactable")) false)

    test "redact returns the redacted representation" (fun () ->
        toEqual (Redactable.redact (box (ApiKey "1234567890"))) (box "1234..."))

    test "redact leaves non-redactable values unchanged" (fun () ->
        toEqual (Redactable.redact (box 5)) (box 5))

    test "getRedacted reads from a known IRedactable" (fun () ->
        let key = ApiKey "abcdefgh"
        toEqual (Redactable.getRedacted (key :> IRedactable)) (box "abcd...")))
