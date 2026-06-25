/// @title Data toolkit — Encoding, Redacted, JSON diff, Schema
///
/// Beyond effects, eff-sharp ports Effect's data utilities. This tour stitches a
/// few together: `Encoding` (Base64/Hex, total — failures are `Result`),
/// `Redacted` (wrap secrets so they don't leak into logs), `JsonPatch` (compute &
/// apply a structural diff between JSON values), and `Schema` (validate untrusted
/// JSON into a typed record).
///
/// These are pure values, so they need no runner — they just compose.
module EffSharp.AiDocs.DataToolkit

open Effect
open EffSharp.AiDocs.Shared

// ---------------------------------------------------------------------------
// Encoding — decode functions return Result, never throw.
// ---------------------------------------------------------------------------

let encoding () =
    let b64 = Encoding.encodeBase64String "hello"
    let hex = Encoding.encodeHexString "hi"

    let roundTrip =
        match Encoding.decodeBase64String b64 with
        | Ok text -> text
        | Error e -> e.Message

    printfn "  base64('hello') = %s   hex('hi') = %s   decode => %s" b64 hex roundTrip

// ---------------------------------------------------------------------------
// Redacted — a secret that won't print itself, but trusted code can recover it.
// ---------------------------------------------------------------------------

let redacted () =
    let apiKey = Redacted.make "sk-live-12345"
    // `string` / logging shows the placeholder, not the secret:
    printfn "  redacted shows  = %s" (string apiKey)
    // ...but trusted code at a boundary can still read it:
    printfn "  recovered value = %s" (Redacted.value apiKey)

// ---------------------------------------------------------------------------
// JsonPatch — structural diff + apply (RFC 6902 subset), reusing the Json DU.
// ---------------------------------------------------------------------------

let jsonDiff () =
    let before = JObject(Map [ "name", JString "Ada"; "age", JNumber 36.0 ])

    let after =
        JObject(Map [ "name", JString "Ada"; "age", JNumber 37.0; "active", JBool true ])

    let patch = JsonPatch.get before after // [Add /active true; Replace /age 37]
    let applied = JsonPatch.apply patch before // == after

    printfn "  diff ops        = %d" (List.length patch)
    printfn "  apply(diff)==new = %b" (applied = after)

// ---------------------------------------------------------------------------
// Schema — validate JSON into a typed record (see also bonus/01_CEPower).
// ---------------------------------------------------------------------------

type Account = { User: string; Age: int }

let accountSchema: Schema<Account> =
    Schema.object {
        let! user = Schema.field "user" (Schema.string |> Schema.minLength 1) (fun (a: Account) -> a.User)
        and! age = Schema.field "age" (Schema.int |> Schema.between 0 150) (fun a -> a.Age)
        return { User = user; Age = age }
    }

let schema () =
    match Schema.decode accountSchema (JObject(Map [ "user", JString "ada"; "age", JNumber 36.0 ])) with
    | Ok account -> printfn "  decoded         = %A" account
    | Error e -> printfn "  decode failed   = %s" (Schema.format e)

    // accumulates ALL errors at once:
    match Schema.decode accountSchema (JObject(Map [ "user", JString ""; "age", JNumber 999.0 ])) with
    | Ok _ -> ()
    | Error e -> printfn "  rejected bad in = %s" (Schema.format e)

let demo () =
    banner "04 · Data toolkit — Encoding / Redacted / JsonPatch / Schema"
    encoding ()
    redacted ()
    jsonDiff ()
    schema ()
