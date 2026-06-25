module RedactedSpec

open Effect
open Effect.Vitest

describe "Redacted" (fun () ->
    test "constructor wraps a structurally-equal value" (fun () ->
        let a = Redacted.make (List.ofSeq "redacted")
        let b = Redacted.make (List.ofSeq "redacted")
        toBe (a = b) true)

    test "value recovers the protected value" (fun () ->
        let r = Redacted.make (List.ofSeq "redacted")
        toEqual (Redacted.value r) (List.ofSeq "redacted"))

    test "string and JSON output are redacted" (fun () ->
        let r = Redacted.make "redacted"
        toBe (string r) "<redacted>"
        toBe (Redacted.toJSON r) "<redacted>")

    test "label appears in redacted output and wipe errors" (fun () ->
        let r = Redacted.makeWith "MY_LABEL" "redacted"
        toEqual r.Label (Some "MY_LABEL")
        toBe (r.ToString()) "<redacted:MY_LABEL>"
        toBe (Redacted.toJSON r) "<redacted:MY_LABEL>"
        toBe (Redacted.wipeUnsafe r) true
        toThrow (fun () -> Redacted.value r |> ignore))

    test "wipeUnsafe removes access to the protected value" (fun () ->
        let r = Redacted.make "redacted"
        toBe (Redacted.wipeUnsafe r) true
        toThrow (fun () -> Redacted.value r |> ignore))

    test "equality and hash use the protected value" (fun () ->
        toBe (Redacted.make 1 = Redacted.make 1) true
        toBe (Redacted.make 1 = Redacted.make 2) false
        toBe ((Redacted.make 1).GetHashCode()) ((Redacted.make 1).GetHashCode())
        toBe ((Redacted.make 1).GetHashCode() <> (Redacted.make 2).GetHashCode()) true)

    test "makeEquivalence compares through an equivalence without exposing values" (fun () ->
        let eq = Redacted.makeEquivalence (=)
        toBe (eq (Redacted.make "1234567890") (Redacted.make "1-34567890")) false
        toBe (eq (Redacted.make "1234567890") (Redacted.make "1234567890")) true))

