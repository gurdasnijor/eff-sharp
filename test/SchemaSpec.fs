module SchemaSpec

open Effect
open Effect.Vitest

/// Spec for `Effect.Schema`, authored once in F# and run on Node via Vitest.
///
/// Scope note: this is the Fable/Vitest harness. Reflective derivation compiles
/// to the `failwith` JS stub here, and FsCheck-based arbitrary generation is not
/// part of the Node test package.
///
/// Everything else is exercised by example. Comparisons follow the `HashSpec`
/// idiom (`toBe (expr) true`); JSON is navigated via `Map.tryFind` and only *leaf*
/// `Json` values (JString/JNumber/JBool/JNull) are compared with `=`, to avoid
/// relying on JS-level deep equality of compiled F# `Map` internals.

// ── fixtures ────────────────────────────────────────────────────────────────

[<Measure>]
type ms

type Color =
    | Red
    | Green
    | Blue

type User = { Name: string; Age: int }

type OptionalUser = { Name: string; Nickname: string option }

type Email = Email of string

type Shape =
    | Circle of float
    | Square of float

let private jobj (pairs: (string * Json) list) : Json = JObject(Map.ofList pairs)

let private decodeOk (s: Schema<'T>) (j: Json) : 'T =
    match Schema.decode s j with
    | Ok v -> v
    | Error e -> failwith ("unexpected decode failure: " + Schema.format e)

let private decodeErr (s: Schema<'T>) (j: Json) : SchemaError =
    match Schema.decode s j with
    | Ok _ -> failwith "expected decode to fail, but it succeeded"
    | Error e -> e

/// Count the *leaf* issues in the tree — the metric that distinguishes
/// accumulating decode (many) from fail-fast decode (always one).
let rec private countLeaves (issue: SchemaIssue) : int =
    match issue with
    | Pointer(_, inner) -> countLeaves inner
    | Composite issues -> issues |> List.sumBy countLeaves
    | AnyOf issues -> issues |> List.sumBy countLeaves
    | _ -> 1

let private colorSchema: Schema<Color> =
    Schema.literalMap [ JString "red", Red; JString "green", Green; JString "blue", Blue ]

let private userSchema: Schema<User> =
    Schema.object {
        let! name = Schema.field "name" Schema.string (fun (u: User) -> u.Name)
        and! age = Schema.field "age" (Schema.int |> Schema.between 0 150) (fun (u: User) -> u.Age)
        return { Name = name; Age = age }
    }

let private circleSchema: Schema<Shape> =
    Schema.object {
        let! r = Schema.field "radius" Schema.float (function
                     | Circle r -> r
                     | _ -> 0.0)

        return Circle r
    }

let private squareSchema: Schema<Shape> =
    Schema.object {
        let! s = Schema.field "side" Schema.float (function
                     | Square s -> s
                     | _ -> 0.0)

        return Square s
    }

let private shapeSchema: Schema<Shape> =
    Schema.taggedUnion
        [ "Circle", circleSchema; "Square", squareSchema ]
        (function
        | Circle _ -> "Circle"
        | Square _ -> "Square")

// ── primitives & refinements ────────────────────────────────────────────────

describe "Schema · primitives & refinements" (fun () ->
    test "primitives decode and report type mismatches" (fun () ->
        toBe (decodeOk Schema.string (JString "s") = "s") true
        toBe (decodeOk Schema.bool (JBool true) = true) true
        toBe (decodeOk Schema.float (JNumber 1.5) = 1.5) true
        toBe (decodeOk Schema.int (JNumber 4.0) = 4) true

        match (decodeErr Schema.string (JNumber 1.0)).Issue with
        | InvalidType(expected, _) -> toBe expected "string"
        | _ -> failwith "expected a type error")

    test "int rejects a fractional number" (fun () ->
        match (decodeErr Schema.int (JNumber 4.5)).Issue with
        | InvalidValue(msg, _) -> toBe (msg.Contains "integer") true
        | _ -> failwith "expected an integer error")

    test "literalMap round-trips enum-like DUs" (fun () ->
        toBe (decodeOk colorSchema (JString "green") = Green) true
        toBe (Schema.encode colorSchema Blue = JString "blue") true

        match (decodeErr colorSchema (JString "purple")).Issue with
        | InvalidValue(msg, _) -> toBe (msg.Contains "one of") true
        | _ -> failwith "expected an InvalidValue")

    test "erase preserves executable codec and AST" (fun () ->
        let erased = Schema.erase userSchema
        toBe (erased.Ast = userSchema.Ast) true

        let json = jobj [ "name", JString "Ada"; "age", JNumber 37.0 ]

        match erased.Decode json with
        | Ok value ->
            let user = unbox<User> value
            toBe user.Name "Ada"
            toBe user.Age 37
            match erased.Encode value with
            | JObject fields ->
                match Map.find "name" fields with
                | JString name -> toBe name "Ada"
                | other -> failwithf "expected encoded name string, got %A" other

                match Map.find "age" fields with
                | JNumber age -> toBe age 37.0
                | other -> failwithf "expected encoded age number, got %A" other
            | other -> failwithf "expected encoded object, got %A" other
        | Error issues -> failwithf "expected erased schema to decode, got %A" issues)

    test "minLength accepts long-enough strings and rejects short ones" (fun () ->
        let s = Schema.string |> Schema.minLength 3
        toBe (decodeOk s (JString "abcd") = "abcd") true

        match (decodeErr s (JString "ab")).Issue with
        | InvalidValue(msg, _) -> toBe (msg.Contains "at least 3") true
        | _ -> failwith "expected a refinement error")

    test "compatibility aliases preserve primitive behavior" (fun () ->
        toBe (decodeOk Schema.String (JString "s")) "s"
        toBe (decodeOk Schema.Boolean (JBool true)) true
        toBe (decodeOk Schema.Number (JNumber 1.5)) 1.5
        toBe (decodeOk Schema.Null JNull) ()
        match decodeOk Schema.Unknown (JObject Map.empty) with
        | JObject fields -> toBe (Map.isEmpty fields) true
        | other -> failwithf "expected object, got %A" other
        toBe (decodeOk Schema.BigInt (JString "9007199254740993")) 9007199254740993I)

    test "between bounds comparable values" (fun () ->
        let s = Schema.int |> Schema.between 1 10
        toBe (decodeOk s (JNumber 5.0) = 5) true
        toBe (Schema.isValid s (JNumber 0.0)) false
        toBe (Schema.isValid s (JNumber 11.0)) false)

    test "compatibility refinements cover common constraints" (fun () ->
        let bounded = Schema.String |> Schema.isMinLength 2 |> Schema.isMaxLength 4
        toBe (Schema.isValid bounded (JString "abc")) true
        toBe (Schema.isValid bounded (JString "a")) false
        toBe (Schema.isValid bounded (JString "abcde")) false

        let integral = Schema.Number |> Schema.isInt
        toBe (Schema.isValid integral (JNumber 3.0)) true
        toBe (Schema.isValid integral (JNumber 3.25)) false

        let atLeast = Schema.Int |> Schema.isGreaterThanOrEqualTo 10
        toBe (Schema.isValid atLeast (JNumber 10.0)) true
        toBe (Schema.isValid atLeast (JNumber 9.0)) false)

    test "matches enforces a regex" (fun () ->
        let s = Schema.string |> Schema.matches "^[a-z]+$"
        toBe (Schema.isValid s (JString "abc")) true
        toBe (Schema.isValid s (JString "Abc1")) false))

// ── composites ──────────────────────────────────────────────────────────────

describe "Schema · composites" (fun () ->
    test "option decodes null to None and round-trips" (fun () ->
        let s = Schema.option Schema.string
        toBe (decodeOk s JNull = None) true
        toBe (decodeOk s (JString "x") = Some "x") true
        toBe (Schema.encode s None = JNull) true
        toBe (Schema.encode s (Some "x") = JString "x") true)

    test "optionalKey decodes missing/null to None and omits None on encode" (fun () ->
        let s =
            Schema.Struct {
                let! name = Schema.field "name" Schema.String (fun value -> value.Name)
                and! nickname = Schema.optionalKey "nickname" Schema.String (fun value -> value.Nickname)
                return { Name = name; Nickname = nickname }
            }

        let missing = decodeOk s (jobj [ "name", JString "Ada" ])
        let present = decodeOk s (jobj [ "name", JString "Ada"; "nickname", JString "ace" ])

        toBe missing.Nickname None
        toBe present.Nickname (Some "ace")

        match Schema.encode s missing with
        | JObject fields -> toBe (Map.containsKey "nickname" fields) false
        | other -> failwithf "expected object, got %A" other)

    test "array tags element errors with their index" (fun () ->
        let s = Schema.array Schema.int
        toBe (decodeOk s (JArray [ JNumber 1.0; JNumber 2.0 ]) = [ 1; 2 ]) true

        match (decodeErr s (JArray [ JNumber 1.0; JString "x" ])).Issue with
        | Pointer([ i ], _) -> toBe i "1"
        | _ -> failwith "expected an indexed pointer")

    test "cause encodes and decodes full failure causes" (fun () ->
        let s = Schema.cause Schema.string
        let cause = Cause.combine (Cause.fail "boom") (Cause.interrupt (Some 42))
        let encoded = Schema.encode s cause
        let expected =
            JArray
                [ JObject(Map.ofList [ "_tag", JString "Fail"; "error", JString "boom" ])
                  JObject(Map.ofList [ "_tag", JString "Interrupt"; "fiberId", JNumber 42.0 ]) ]

        toBe (encoded = expected) true

        match Schema.decode s encoded with
        | Ok decoded ->
            toEqual (Cause.failures decoded) [ "boom" ]
            toEqual decoded.Reasons cause.Reasons
        | Error error -> failwithf "expected cause decode, got %A" error)

    test "tuple2 decodes, round-trips, and rejects wrong length" (fun () ->
        let s = Schema.tuple2 Schema.int Schema.string
        let j = JArray [ JNumber 7.0; JString "k" ]
        toBe (decodeOk s j = (7, "k")) true
        toBe (Schema.encode s (7, "k") = j) true

        match (decodeErr s (JArray [ JNumber 1.0 ])).Issue with
        | InvalidValue(msg, _) -> toBe (msg.Contains "length 2") true
        | _ -> failwith "expected a length error")

    test "object builder decodes and round-trips a record" (fun () ->
        let j = jobj [ "name", JString "Ada"; "age", JNumber 36.0 ]
        let u = decodeOk userSchema j
        toBe u.Name "Ada"
        toBe u.Age 36
        toBe (decodeOk userSchema (Schema.encode userSchema u) = u) true)

    test "union tries members in order and reports AnyOf when none match" (fun () ->
        let s = Schema.union [ Schema.literal (JString "a"); Schema.literal (JString "b") ]
        toBe (decodeOk s (JString "a") = JString "a") true

        match (decodeErr s (JString "z")).Issue with
        | AnyOf _ -> toBe true true
        | _ -> failwith "expected AnyOf")

    test "map lifts into a single-case DU (nominal branding)" (fun () ->
        let s = Schema.string |> Schema.map Email (fun (Email e) -> e)
        toBe (decodeOk s (JString "a@b.com") = Email "a@b.com") true
        toBe (Schema.encode s (Email "a@b.com") = JString "a@b.com") true)

    test "measured decodes into a unit-typed float and round-trips" (fun () ->
        let s: Schema<float<ms>> =
            Schema.float |> Schema.between 0.0 60000.0 |> Schema.measured

        toBe (decodeOk s (JNumber 250.0) = 250.0<ms>) true
        toBe (Schema.encode s 250.0<ms> = JNumber 250.0) true))

// ── tagged unions (the lossless-discriminator path) ─────────────────────────

describe "Schema · tagged unions" (fun () ->
    test "round-trips losslessly and injects _tag" (fun () ->
        let original = Circle 3.0
        let encoded = Schema.encode shapeSchema original

        match encoded with
        | JObject m -> toBe (Map.tryFind "_tag" m = Some(JString "Circle")) true
        | _ -> failwith "expected an object"

        toBe (decodeOk shapeSchema encoded = original) true
        toBe (decodeOk shapeSchema (Schema.encode shapeSchema (Square 2.0)) = Square 2.0) true)

    test "rejects a missing tag" (fun () ->
        match (decodeErr shapeSchema (jobj [ "radius", JNumber 1.0 ])).Issue with
        | MissingKey k -> toBe k "_tag"
        | _ -> failwith "expected a missing _tag")

    test "rejects an unknown tag" (fun () ->
        match (decodeErr shapeSchema (jobj [ "_tag", JString "Triangle" ])).Issue with
        | InvalidValue(msg, _) -> toBe (msg.Contains "Triangle") true
        | _ -> failwith "expected an unknown-tag error"))

// ── error accumulation & rendering ──────────────────────────────────────────

describe "Schema · error accumulation & rendering" (fun () ->
    // The defining property the port keeps (and a fail-fast decoder gives up):
    // every field is checked and ALL issues are reported, not just the first.
    test "object decode accumulates every field error" (fun () ->
        let bad = jobj [ "name", JNumber 1.0; "age", JString "old" ]
        let err = decodeErr userSchema bad
        toBe (countLeaves err.Issue) 2)

    test "two missing keys produce two issues" (fun () ->
        let err = decodeErr userSchema (jobj [])
        toBe (countLeaves err.Issue) 2)

    test "format renders path-annotated lines for each failure" (fun () ->
        let err = decodeErr userSchema (jobj [ "name", JNumber 1.0; "age", JString "old" ])
        let rendered = Schema.format err
        toBe (rendered.Contains "name") true
        toBe (rendered.Contains "age") true)

    test "format names a missing key" (fun () ->
        let err = decodeErr userSchema (jobj [ "name", JString "Bob" ])
        toBe ((Schema.format err).Contains "Missing key") true))

// ── AST-fold interpreters (the new structural-AST payoff) ───────────────────

describe "Schema · interpreters" (fun () ->
    test "equivalence compares by encoded form" (fun () ->
        let eqColor = Schema.equivalence colorSchema
        toBe (eqColor Red Red) true
        toBe (eqColor Red Blue) false

        let eqOpt = Schema.equivalence (Schema.option Schema.int)
        toBe (eqOpt (Some 1) (Some 1)) true
        toBe (eqOpt (Some 1) None) false)

    test "pretty renders primitives, options, and dispatches tagged unions" (fun () ->
        toBe (Schema.pretty Schema.string "hi") "\"hi\""
        toBe (Schema.pretty (Schema.option Schema.string) (Some "x")) "Some \"x\""
        toBe (Schema.pretty (Schema.option Schema.string) None) "None"
        toBe ((Schema.pretty shapeSchema (Circle 3.0)).StartsWith "Circle") true)

    test "jsonSchema: primitives carry a type" (fun () ->
        match Schema.jsonSchema Schema.string with
        | JObject m -> toBe (Map.tryFind "type" m = Some(JString "string")) true
        | _ -> failwith "expected an object"

        match Schema.jsonSchema Schema.int with
        | JObject m -> toBe (Map.tryFind "type" m = Some(JString "integer")) true
        | _ -> failwith "expected an object")

    test "jsonSchema: objects list properties and required keys" (fun () ->
        match Schema.jsonSchema userSchema with
        | JObject m ->
            toBe (Map.tryFind "type" m = Some(JString "object")) true

            match Map.tryFind "properties" m with
            | Some(JObject p) ->
                toBe (Map.containsKey "name" p) true
                toBe (Map.containsKey "age" p) true
            | _ -> failwith "expected properties"

            match Map.tryFind "required" m with
            | Some(JArray req) ->
                toBe (List.contains (JString "name") req) true
                toBe (List.contains (JString "age") req) true
            | _ -> failwith "expected required"
        | _ -> failwith "expected an object")

    test "jsonSchema: arrays describe items, options allow null, literals are const" (fun () ->
        match Schema.jsonSchema (Schema.array Schema.int) with
        | JObject m ->
            toBe (Map.tryFind "type" m = Some(JString "array")) true

            match Map.tryFind "items" m with
            | Some(JObject im) -> toBe (Map.tryFind "type" im = Some(JString "integer")) true
            | _ -> failwith "expected items"
        | _ -> failwith "expected an object"

        match Schema.jsonSchema (Schema.option Schema.string) with
        | JObject m ->
            match Map.tryFind "anyOf" m with
            | Some(JArray alts) ->
                toBe
                    (alts
                     |> List.exists (function
                         | JObject am -> Map.tryFind "type" am = Some(JString "null")
                         | _ -> false))
                    true
            | _ -> failwith "expected anyOf"
        | _ -> failwith "expected an object"

        match Schema.jsonSchema (Schema.literal (JString "x")) with
        | JObject m -> toBe (Map.tryFind "const" m = Some(JString "x")) true
        | _ -> failwith "expected an object")

    test "jsonSchema: tagged unions become oneOf with _tag consts" (fun () ->
        match Schema.jsonSchema shapeSchema with
        | JObject m ->
            match Map.tryFind "oneOf" m with
            | Some(JArray variants) ->
                toBe (List.length variants) 2

                toBe
                    (variants
                     |> List.forall (function
                         | JObject vm ->
                             match Map.tryFind "required" vm with
                             | Some(JArray req) -> List.contains (JString "_tag") req
                             | _ -> false
                         | _ -> false))
                    true
            | _ -> failwith "expected oneOf"
        | _ -> failwith "expected an object"))

// ── surfaces ────────────────────────────────────────────────────────────────

describe "Schema · surfaces" (fun () ->
    test "validate / isValid" (fun () ->
        toBe (Schema.isValid Schema.int (JNumber 3.0)) true
        toBe (Schema.isValid Schema.int (JString "x")) false

        match Schema.validate Schema.bool (JBool true) with
        | Ok() -> toBe true true
        | Error _ -> failwith "expected Ok")

    test "decodeExit yields Success with the value, Failure on invalid input" (fun () ->
        match Schema.decodeExit Schema.string (JString "ok") with
        | Success v -> toBe v "ok"
        | Failure _ -> failwith "expected Success"

        match Schema.decodeExit Schema.int (JString "no") with
        | Success _ -> failwith "expected Failure"
        | Failure _ -> toBe true true)

    test "Decoded active pattern matches a schema in pattern position" (fun () ->
        let strS = Schema.string

        let hit =
            match JString "hi" with
            | Schema.Decoded strS s -> s
            | _ -> "no-match"

        toBe hit "hi"

        let miss =
            match JNumber 1.0 with
            | Schema.Decoded strS s -> s
            | _ -> "no-match"

        toBe miss "no-match")

    // Integration with the Effect-aware runner: a successful decode flows through
    // the Effect channel; assertions inside `Effect.map` become defects → Failure
    // → a failed test if they throw.
    itEffect "decodeEffect lifts a successful decode into the Effect channel" (fun () ->
        Schema.decodeEffect userSchema (jobj [ "name", JString "Grace"; "age", JNumber 45.0 ])
        |> Effect.map (fun u ->
            toBe u.Name "Grace"
            toBe u.Age 45)))
