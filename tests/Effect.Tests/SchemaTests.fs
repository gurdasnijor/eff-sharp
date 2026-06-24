module Effect.Tests.SchemaTests

open Xunit
open Effect

// Schema v1 (wave4) — type-first codec. Behaviour cribbed from
// repos/effect-smol/.../SCHEMA.md (Validation / Structs / Unions) and the
// agreed DESIGN.md. JSON values are the leaf6 `JsonPatch.Json` DU.

// -- fixtures ----------------------------------------------------------------

type Role =
    | Admin
    | Member
    | Guest

let roleSchema: Schema<Role> =
    Schema.literalMap [ JString "admin", Admin; JString "member", Member; JString "guest", Guest ]

type Person = { Name: string; Age: int; Role: Role }

let personSchema: Schema<Person> =
    Schema.object {
        let! name = Schema.field "name" (Schema.string |> Schema.minLength 1) (fun (p: Person) -> p.Name)
        and! age = Schema.field "age" (Schema.int |> Schema.between 0 150) (fun p -> p.Age)
        and! role = Schema.field "role" roleSchema (fun p -> p.Role)
        return { Name = name; Age = age; Role = role }
    }

type Addr = { Zip: string }
type Cust = { Name: string; Addr: Addr }

let addrSchema: Schema<Addr> =
    Schema.object {
        let! zip = Schema.field "zip" (Schema.string |> Schema.minLength 5) (fun (a: Addr) -> a.Zip)
        return { Zip = zip }
    }

let custSchema: Schema<Cust> =
    Schema.object {
        let! name = Schema.field "name" Schema.string (fun (c: Cust) -> c.Name)
        and! addr = Schema.field "addr" addrSchema (fun c -> c.Addr)
        return { Name = name; Addr = addr }
    }

let private issues (e: SchemaError) =
    match e.Issue with
    | Composite xs -> xs
    | single -> [ single ]

let private okOrFail =
    function
    | Ok v -> v
    | Error e -> failwithf "expected Ok, got %s" (Schema.format e)

// -- primitives --------------------------------------------------------------

[<Fact>]
let ``primitives decode success`` () =
    Assert.Equal(Ok "hi", Schema.decode Schema.string (JString "hi"))
    Assert.Equal(Ok 42, Schema.decode Schema.int (JNumber 42.0))
    Assert.Equal(Ok 1.5, Schema.decode Schema.float (JNumber 1.5))
    Assert.Equal(Ok true, Schema.decode Schema.bool (JBool true))

[<Fact>]
let ``primitives decode type-mismatch -> InvalidType`` () =
    match Schema.decode Schema.string (JNumber 1.0) with
    | Error { Issue = InvalidType("string", JNumber 1.0) } -> ()
    | other -> failwithf "unexpected %A" other

[<Fact>]
let ``int rejects a fractional number -> InvalidValue`` () =
    match Schema.decode Schema.int (JNumber 1.2) with
    | Error { Issue = InvalidValue("Expected an integer", _) } -> ()
    | other -> failwithf "unexpected %A" other

[<Fact>]
let ``filters: minLength / between / matches`` () =
    Assert.True(Result.isError (Schema.decode (Schema.string |> Schema.minLength 3) (JString "ab")))
    Assert.Equal(Ok "abc", Schema.decode (Schema.string |> Schema.minLength 3) (JString "abc"))
    Assert.True(Result.isError (Schema.decode (Schema.int |> Schema.between 0 10) (JNumber 99.0)))
    Assert.Equal(Ok 5, Schema.decode (Schema.int |> Schema.between 0 10) (JNumber 5.0))
    Assert.Equal(Ok "x1", Schema.decode (Schema.string |> Schema.matches "^x") (JString "x1"))
    Assert.True(Result.isError (Schema.decode (Schema.string |> Schema.matches "^x") (JString "y1")))

// -- literalMap (enum) -------------------------------------------------------

[<Fact>]
let ``literalMap decodes and encodes an enum DU`` () =
    Assert.Equal(Ok Admin, Schema.decode roleSchema (JString "admin"))
    Assert.Equal(JString "guest", Schema.encode roleSchema Guest)
    match Schema.decode roleSchema (JString "nope") with
    | Error { Issue = InvalidValue _ } -> ()
    | other -> failwithf "unexpected %A" other

// -- struct: accumulation, missing key, nested pointer -----------------------

[<Fact>]
let ``struct decodes a valid object`` () =
    let json = JObject(Map [ "name", JString "Alice"; "age", JNumber 30.0; "role", JString "member" ])
    Assert.Equal({ Name = "Alice"; Age = 30; Role = Member }, okOrFail (Schema.decode personSchema json))

[<Fact>]
let ``struct accumulates ALL field errors`` () =
    let json = JObject(Map [ "name", JString ""; "age", JNumber 200.0; "role", JString "x" ])
    match Schema.decode personSchema json with
    | Error e ->
        // one issue per bad field, each pointed at its key
        Assert.Equal(3, List.length (issues e))
        let keys =
            issues e
            |> List.choose (function Pointer([ k ], _) -> Some k | _ -> None)
            |> List.sort
        Assert.Equal<string list>([ "age"; "name"; "role" ], keys)
    | Ok _ -> failwith "expected failure"

[<Fact>]
let ``struct reports a missing required key`` () =
    let json = JObject(Map [ "name", JString "A"; "role", JString "guest" ]) // no age
    match Schema.decode personSchema json with
    | Error e -> Assert.Contains(MissingKey "age", issues e)
    | Ok _ -> failwith "expected failure"

[<Fact>]
let ``nested struct error carries a Pointer path`` () =
    let json = JObject(Map [ "name", JString "A"; "addr", JObject(Map [ "zip", JString "12" ]) ])
    match Schema.decode custSchema json with
    | Error e ->
        // path renders as ["addr"]["zip"]
        Assert.Contains("[\"addr\"][\"zip\"]", Schema.format e)
    | Ok _ -> failwith "expected failure"

[<Fact>]
let ``non-object decoded by a struct -> InvalidType object`` () =
    match Schema.decode personSchema (JString "nope") with
    | Error { Issue = InvalidType("object", _) } -> ()
    | other -> failwithf "unexpected %A" other

// -- array -------------------------------------------------------------------

[<Fact>]
let ``array decodes elements and points at the failing index`` () =
    Assert.Equal(Ok [ 1; 2; 3 ], Schema.decode (Schema.array Schema.int) (JArray [ JNumber 1.0; JNumber 2.0; JNumber 3.0 ]))
    match Schema.decode (Schema.array Schema.int) (JArray [ JNumber 1.0; JString "x"; JNumber 3.0 ]) with
    | Error e ->
        match issues e with
        | [ Pointer([ "1" ], InvalidType("number", _)) ] -> ()
        | other -> failwithf "unexpected %A" other
    | Ok _ -> failwith "expected failure"

// -- union -------------------------------------------------------------------

[<Fact>]
let ``union picks the first matching member, else AnyOf`` () =
    let u = Schema.union [ Schema.string |> Schema.minLength 3; Schema.string |> Schema.matches "^x" ]
    Assert.Equal(Ok "abcd", Schema.decode u (JString "abcd")) // member 1
    Assert.Equal(Ok "xy", Schema.decode u (JString "xy")) // member 2
    match Schema.decode u (JNumber 1.0) with
    | Error { Issue = AnyOf members } -> Assert.Equal(2, List.length members)
    | other -> failwithf "unexpected %A" other

// -- round trip --------------------------------------------------------------

[<Fact>]
let ``round-trip: decode (encode x) = Ok x`` () =
    let p = { Name = "Alice"; Age = 30; Role = Member }
    Assert.Equal(Ok p, Schema.decode personSchema (Schema.encode personSchema p))
    let c = { Name = "Bob"; Addr = { Zip = "90210" } }
    Assert.Equal(Ok c, Schema.decode custSchema (Schema.encode custSchema c))

// -- decodeExit / Cause / Exit ----------------------------------------------

[<Fact>]
let ``decodeExit success`` () =
    match Schema.decodeExit Schema.int (JNumber 7.0) with
    | Success 7 -> ()
    | other -> failwithf "unexpected %A" other

[<Fact>]
let ``decodeExit failure is Failure(Cause([Fail(SchemaError)]))`` () =
    match Schema.decodeExit Schema.int (JString "x") with
    | Failure cause ->
        match cause.Reasons with
        | [ Reason.Fail({ Issue = InvalidType("number", _) }) ] -> () // typed Fail, never Die
        | other -> failwithf "expected a single typed Fail, got %A" other
    | Success _ -> failwith "expected failure"

// -- validate / decodeEffect -------------------------------------------------

[<Fact>]
let ``validate returns unit on success and error otherwise`` () =
    Assert.Equal(Ok(), Schema.validate Schema.int (JNumber 1.0))
    Assert.True(Result.isError (Schema.validate Schema.int (JBool true)))

[<Fact>]
let ``decodeEffect lifts the result into the Effect channel`` () =
    let ok = Schema.decodeEffect Schema.int (JNumber 9.0) |> Effect.runSync ()
    Assert.Equal(Success 9, ok)
    match Schema.decodeEffect Schema.int (JString "x") |> Effect.runSync () with
    | Failure _ -> ()
    | Success _ -> failwith "expected failure"

// -- derive (reflection) -----------------------------------------------------

type Point = { X: int; Y: int }

[<Fact>]
let ``derive builds a record codec keyed by field name`` () =
    let s = Schema.derive<Point> ()
    let json = JObject(Map [ "X", JNumber 1.0; "Y", JNumber 2.0 ])
    Assert.Equal({ X = 1; Y = 2 }, okOrFail (Schema.decode s json))
    Assert.Equal(json, Schema.encode s { X = 1; Y = 2 })
    // round-trip
    Assert.Equal(Ok { X = 3; Y = 4 }, Schema.decode s (Schema.encode s { X = 3; Y = 4 }))

[<Fact>]
let ``derive accumulates record field errors`` () =
    let s = Schema.derive<Point> ()
    match Schema.decode s (JObject(Map [ "X", JString "a" ])) with // bad X, missing Y
    | Error e -> Assert.Equal(2, List.length (issues e))
    | Ok _ -> failwith "expected failure"

[<Fact>]
let ``derive maps an enum-like DU to its case name`` () =
    let s = Schema.derive<Role> ()
    Assert.Equal(Ok Admin, Schema.decode s (JString "Admin"))
    Assert.Equal(JString "Guest", Schema.encode s Guest)
