namespace Effect

/// Schema v1 — a **type-first, schema-as-codec** validation/parsing library.
///
/// Direction & rationale: `eff-sharp-research-schema/DESIGN.md`. F# has no
/// type-level computation, so a faithful port of Effect's `Schema.Type<typeof S>`
/// (≈14 type params) is rejected. Instead the data model is authored natively
/// (records / DUs / units of measure) and a `Schema<'T>` is a *reified codec
/// value* pointed at that type:
///
///   `Schema<'T> = { Decode; Encode; Ast }` over the JSON value type **reused from
///   `JsonPatch.Json`** (leaf6 — not reinvented).
///
/// Decoding accumulates a structured `SchemaIssue` tree; a validation failure is a
/// typed `Reason.Fail` carried in a `Cause` (NEVER a `Die` defect) so
/// `decodeExit` yields `Failure(Cause([Fail(SchemaError …)]))` — matching upstream
/// rendering — and `decodeEffect` lifts into the `Effect` channel.
///
/// Deferred to v2 (noted, not built): `toJsonSchema`, Myriad source-gen,
/// `Schema.diff` (via JsonPatch), recursive `suspend`, general transformations.
/// See DESIGN.md "Phased plan".

open System
open Microsoft.FSharp.Reflection

/// Lightweight reified shape of a schema (powers v2 tooling like `toJsonSchema`).
type SchemaAst =
    | AString
    | AInt
    | AFloat
    | ABool
    | ADecimal
    | ALiteral of Json
    | AArray of SchemaAst
    | ATuple of SchemaAst list
    | AObject of keys: string list
    | AUnion of SchemaAst list
    | ARefine of SchemaAst * description: string
    | ADeclare of name: string

/// The structured decode/encode error tree (port of effect's `SchemaIssue`).
/// `Pointer`/`Composite`/`AnyOf` make it a tree; leaves describe one failure.
type SchemaIssue =
    | InvalidType of expected: string * actual: Json
    | InvalidValue of message: string * actual: Json
    | MissingKey of key: string
    | UnexpectedKey of key: string
    | Forbidden of message: string
    | Pointer of path: string list * issue: SchemaIssue
    | Composite of issues: SchemaIssue list
    | AnyOf of issues: SchemaIssue list

/// The typed error carried in the `'E` channel on a failed decode.
type SchemaError = { Issue: SchemaIssue }

/// A reified, bidirectional codec for `'T`. `Decode` returns a *list* of issues so
/// composite schemas (structs/tuples/arrays) can accumulate every failure.
type Schema<'T> =
    { Decode: Json -> Result<'T, SchemaIssue list>
      Encode: 'T -> Json
      Ast: SchemaAst }

/// An applicative object-field codec used by the `Schema.object { }` builder. It
/// is bidirectional: `DecodeFields` reads the keys it owns out of a JSON object
/// (accumulating issues), `EncodeFields` projects them back out of the record.
type ObjectCodec<'R, 'A> =
    { DecodeFields: Map<string, Json> -> Result<'A, SchemaIssue list>
      EncodeFields: 'R -> (string * Json) list
      Keys: string list }

[<RequireQualifiedAccess>]
module Schema =

    open System.Text.RegularExpressions

    // -- numeric conversions, captured BEFORE the `int`/`float`/`decimal` schema
    //    values shadow the built-in operators for the rest of the module. --
    let inline private toInt (n: float) : int = int n
    let inline private toFloat (i: int) : float = float i
    let inline private toDecimal (n: float) : decimal = decimal n
    let inline private ofDecimal (d: decimal) : float = float d

    // -- helpers --------------------------------------------------------------

    /// Accumulate a list of per-element results into one, concatenating all issues.
    let private sequenceAccum (results: Result<'a, SchemaIssue list> list) : Result<'a list, SchemaIssue list> =
        let oks = ResizeArray<'a>()
        let errs = ResizeArray<SchemaIssue>()

        for r in results do
            match r with
            | Ok v -> oks.Add v
            | Error e -> errs.AddRange e

        if errs.Count = 0 then
            Ok(List.ofSeq oks)
        else
            Error(List.ofSeq errs)

    // -- primitives -----------------------------------------------------------

    /// Matches a JSON string. (Schema.String)
    let string: Schema<string> =
        { Ast = AString
          Decode =
            (function
            | JString s -> Ok s
            | j -> Error [ InvalidType("string", j) ])
          Encode = JString }

    /// Matches a JSON boolean. (Schema.Boolean)
    let bool: Schema<bool> =
        { Ast = ABool
          Decode =
            (function
            | JBool b -> Ok b
            | j -> Error [ InvalidType("boolean", j) ])
          Encode = JBool }

    /// Matches any JSON number. (Schema.Number)
    let float: Schema<float> =
        { Ast = AFloat
          Decode =
            (function
            | JNumber n -> Ok n
            | j -> Error [ InvalidType("number", j) ])
          Encode = JNumber }

    /// Matches a JSON number with no fractional part. (Schema.Number + isInt)
    let int: Schema<int> =
        { Ast = AInt
          Decode =
            (function
            | JNumber n when Double.IsInteger n -> Ok(toInt n)
            | JNumber n -> Error [ InvalidValue("Expected an integer", JNumber n) ]
            | j -> Error [ InvalidType("number", j) ])
          Encode = fun i -> JNumber(toFloat i) }

    /// Matches a JSON number, decoded as `decimal` (lossy from JSON float; v1).
    let decimal: Schema<decimal> =
        { Ast = ADecimal
          Decode =
            (function
            | JNumber n -> Ok(toDecimal n)
            | j -> Error [ InvalidType("number", j) ])
          Encode = fun d -> JNumber(ofDecimal d) }

    /// Matches exactly one JSON literal value. (Schema.Literal)
    let literal (value: Json) : Schema<Json> =
        { Ast = ALiteral value
          Decode =
            (fun j ->
                if j = value then
                    Ok j
                else
                    Error [ InvalidValue(sprintf "Expected %A" value, j) ])
          Encode = id }

    /// Maps a closed set of JSON literals to/from F# values — the idiomatic way to
    /// model enum-like DUs (round-trips losslessly). (Schema.Literals + decode)
    let literalMap (pairs: (Json * 'T) list) : Schema<'T> =
        let expected = pairs |> List.map (fst >> sprintf "%A") |> String.concat ", "

        { Ast = AUnion(pairs |> List.map (fst >> ALiteral))
          Decode =
            (fun j ->
                match pairs |> List.tryFind (fun (jv, _) -> jv = j) with
                | Some(_, v) -> Ok v
                | None -> Error [ InvalidValue(sprintf "Expected one of: %s" expected, j) ])
          Encode =
            (fun v ->
                match pairs |> List.tryFind (fun (_, vv) -> vv = v) with
                | Some(j, _) -> j
                | None -> JNull) }

    // -- filters (Schema<'T> -> Schema<'T>) -----------------------------------

    /// Refine an existing schema with a predicate; on failure reports
    /// `InvalidValue message`. (Schema.check / refinements)
    let refine (predicate: 'T -> bool) (message: string) (s: Schema<'T>) : Schema<'T> =
        { s with
            Ast = ARefine(s.Ast, message)
            Decode =
                (fun j ->
                    match s.Decode j with
                    | Ok v ->
                        if predicate v then
                            Ok v
                        else
                            Error [ InvalidValue(message, j) ]
                    | Error e -> Error e) }

    /// Require a string of at least `n` characters. (Schema.isMinLength)
    let minLength (n: int) (s: Schema<string>) : Schema<string> =
        s
        |> refine (fun str -> String.length str >= n) (sprintf "Expected a string of length at least %d" n)

    /// Require a comparable value within `[lo, hi]`. (Schema.isBetween)
    let between (lo: 'T) (hi: 'T) (s: Schema<'T>) : Schema<'T> =
        s
        |> refine (fun v -> v >= lo && v <= hi) (sprintf "Expected a value between %A and %A" lo hi)

    /// Require a string matching a regular expression. (Schema.isPattern)
    let matches (pattern: string) (s: Schema<string>) : Schema<string> =
        let re = Regex(pattern)
        s |> refine re.IsMatch (sprintf "Expected a string matching /%s/" pattern)

    /// Annotate a schema with a brand name (v1 is a tag only; true nominal
    /// branding is done by `map`-ing into a single-case DU). (Schema.brand)
    let brand (name: string) (s: Schema<'T>) : Schema<'T> =
        { s with
            Ast = ARefine(s.Ast, sprintf "brand:%s" name) }

    /// Map a schema's decoded value through an isomorphism (e.g. into a
    /// single-case DU for nominal typing). Total — for fallible decode use
    /// `refine` first.
    let map (forward: 'T -> 'U) (backward: 'U -> 'T) (s: Schema<'T>) : Schema<'U> =
        { Ast = s.Ast
          Decode = (fun j -> s.Decode j |> Result.map forward)
          Encode = (fun u -> s.Encode(backward u)) }

    // -- composites -----------------------------------------------------------

    /// Wrap a schema so `null`/absent decodes to `None`. (Schema.optionalKey-ish)
    let option (s: Schema<'T>) : Schema<'T option> =
        { Ast = ARefine(s.Ast, "option")
          Decode =
            (function
            | JNull -> Ok None
            | j -> s.Decode j |> Result.map Some)
          Encode =
            (function
            | Some v -> s.Encode v
            | None -> JNull) }

    /// A homogeneous JSON array; per-element issues are tagged with their index.
    /// (Schema.Array)
    let array (elem: Schema<'T>) : Schema<'T list> =
        { Ast = AArray elem.Ast
          Encode = (fun xs -> JArray(List.map elem.Encode xs))
          Decode =
            (function
            | JArray items ->
                items
                |> List.mapi (fun i it ->
                    elem.Decode it
                    |> Result.mapError (List.map (fun e -> Pointer([ sprintf "%d" i ], e))))
                |> sequenceAccum
            | j -> Error [ InvalidType("array", j) ]) }

    /// A 2-tuple `[a, b]`. (Schema.Tuple — v1 ships tuple2/tuple3.)
    let tuple2 (a: Schema<'A>) (b: Schema<'B>) : Schema<'A * 'B> =
        { Ast = ATuple [ a.Ast; b.Ast ]
          Encode = (fun (x, y) -> JArray [ a.Encode x; b.Encode y ])
          Decode =
            (function
            | JArray [ ja; jb ] ->
                let ra = a.Decode ja |> Result.mapError (List.map (fun e -> Pointer([ "0" ], e)))
                let rb = b.Decode jb |> Result.mapError (List.map (fun e -> Pointer([ "1" ], e)))

                match ra, rb with
                | Ok x, Ok y -> Ok(x, y)
                | _ ->
                    let e1 =
                        match ra with
                        | Error e -> e
                        | Ok _ -> []

                    let e2 =
                        match rb with
                        | Error e -> e
                        | Ok _ -> []

                    Error(e1 @ e2)
            | JArray items ->
                Error
                    [ InvalidValue(
                          sprintf "Expected a tuple of length 2, got length %d" (List.length items),
                          JArray items
                      ) ]
            | j -> Error [ InvalidType("array", j) ]) }

    /// A 3-tuple `[a, b, c]`.
    let tuple3 (a: Schema<'A>) (b: Schema<'B>) (c: Schema<'C>) : Schema<'A * 'B * 'C> =
        { Ast = ATuple [ a.Ast; b.Ast; c.Ast ]
          Encode = (fun (x, y, z) -> JArray [ a.Encode x; b.Encode y; c.Encode z ])
          Decode =
            (function
            | JArray [ ja; jb; jc ] ->
                let ra = a.Decode ja |> Result.mapError (List.map (fun e -> Pointer([ "0" ], e)))
                let rb = b.Decode jb |> Result.mapError (List.map (fun e -> Pointer([ "1" ], e)))
                let rc = c.Decode jc |> Result.mapError (List.map (fun e -> Pointer([ "2" ], e)))

                match ra, rb, rc with
                | Ok x, Ok y, Ok z -> Ok(x, y, z)
                | _ ->
                    let pick =
                        function
                        | Error e -> e
                        | Ok _ -> []

                    Error(pick ra @ pick rb @ pick rc)
            | JArray items ->
                Error
                    [ InvalidValue(
                          sprintf "Expected a tuple of length 3, got length %d" (List.length items),
                          JArray items
                      ) ]
            | j -> Error [ InvalidType("array", j) ]) }

    /// Try each member schema in order; first success wins, otherwise all member
    /// issues are grouped under `AnyOf`. v1 encodes via the first member (general
    /// union encoding is ambiguous — see DESIGN.md; prefer `literalMap` for enums).
    /// (Schema.Union)
    let union (members: Schema<'T> list) : Schema<'T> =
        { Ast = AUnion(members |> List.map (fun m -> m.Ast))
          Encode = (fun v -> (List.head members).Encode v)
          Decode =
            (fun j ->
                let rec go acc =
                    function
                    | [] -> Error [ AnyOf(List.rev acc) ]
                    | (m: Schema<'T>) :: rest ->
                        match m.Decode j with
                        | Ok v -> Ok v
                        | Error errs ->
                            let one =
                                match errs with
                                | [ e ] -> e
                                | es -> Composite es

                            go (one :: acc) rest

                go [] members) }

    // -- applicative struct builder -------------------------------------------

    /// One required object field, carrying both its decoder and a getter (so the
    /// struct is bidirectional). Use inside `Schema.object { }`.
    let field (key: string) (schema: Schema<'F>) (getter: 'R -> 'F) : ObjectCodec<'R, 'F> =
        { Keys = [ key ]
          EncodeFields = (fun r -> [ key, schema.Encode(getter r) ])
          DecodeFields =
            (fun m ->
                match Map.tryFind key m with
                | None -> Error [ MissingKey key ]
                | Some j -> schema.Decode j |> Result.mapError (List.map (fun e -> Pointer([ key ], e)))) }

    /// Applicative builder for struct schemas. `let!`/`and!` run every field
    /// decoder and accumulate ALL issues; `return` builds the record (so a field
    /// mismatch is a *compile* error). Bidirectional via the field getters.
    type ObjectBuilder() =
        member _.MergeSources(a: ObjectCodec<'R, 'A>, b: ObjectCodec<'R, 'B>) : ObjectCodec<'R, 'A * 'B> =
            { Keys = a.Keys @ b.Keys
              EncodeFields = (fun r -> a.EncodeFields r @ b.EncodeFields r)
              DecodeFields =
                (fun m ->
                    match a.DecodeFields m, b.DecodeFields m with
                    | Ok x, Ok y -> Ok(x, y)
                    | ra, rb ->
                        let ex =
                            match ra with
                            | Error e -> e
                            | Ok _ -> []

                        let ey =
                            match rb with
                            | Error e -> e
                            | Ok _ -> []

                        Error(ex @ ey)) }

        member _.BindReturn(c: ObjectCodec<'R, 'A>, f: 'A -> 'R) : Schema<'R> =
            { Ast = AObject c.Keys
              Encode = (fun r -> JObject(Map.ofList (c.EncodeFields r)))
              Decode =
                (function
                | JObject m -> c.DecodeFields m |> Result.map f
                | j -> Error [ InvalidType("object", j) ]) }

    /// `Schema.object { let! a = field ...; and! b = field ...; return record }`
    let object = ObjectBuilder()

    // -- reflection-based derivation (records / option / enum-like DUs) --------

    let rec private deriveCodec (t: Type) : (Json -> Result<obj, SchemaIssue list>) * (obj -> Json) =
        if t = typeof<string> then
            (function
            | JString s -> Ok(box s)
            | j -> Error [ InvalidType("string", j) ]),
            (fun o -> JString(unbox<string> o))
        elif t = typeof<bool> then
            (function
            | JBool b -> Ok(box b)
            | j -> Error [ InvalidType("boolean", j) ]),
            (fun o -> JBool(unbox<bool> o))
        elif t = typeof<int> then
            (function
            | JNumber n when Double.IsInteger n -> Ok(box (toInt n))
            | JNumber n -> Error [ InvalidValue("Expected an integer", JNumber n) ]
            | j -> Error [ InvalidType("number", j) ]),
            (fun o -> JNumber(toFloat (unbox<int> o)))
        elif t = typeof<float> then
            (function
            | JNumber n -> Ok(box n)
            | j -> Error [ InvalidType("number", j) ]),
            (fun o -> JNumber(unbox<float> o))
        elif t = typeof<decimal> then
            (function
            | JNumber n -> Ok(box (toDecimal n))
            | j -> Error [ InvalidType("number", j) ]),
            (fun o -> JNumber(ofDecimal (unbox<decimal> o)))
        elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<_ option> then
            let inner = t.GetGenericArguments().[0]
            let dec, enc = deriveCodec inner
            let cases = FSharpType.GetUnionCases t
            let someCase = cases |> Array.find (fun c -> c.Name = "Some")
            let noneCase = cases |> Array.find (fun c -> c.Name = "None")

            (function
            | JNull -> Ok(FSharpValue.MakeUnion(noneCase, [||]))
            | j -> dec j |> Result.map (fun v -> FSharpValue.MakeUnion(someCase, [| v |]))),
            (fun o ->
                match FSharpValue.GetUnionFields(o, t) with
                | c, _ when c.Name = "None" -> JNull
                | _, [| v |] -> enc v
                | _ -> JNull)
        elif FSharpType.IsRecord t then
            let fields = FSharpType.GetRecordFields t
            let codecs = fields |> Array.map (fun pi -> pi.Name, deriveCodec pi.PropertyType)

            (fun j ->
                match j with
                | JObject m ->
                    let results =
                        codecs
                        |> Array.map (fun (name, (dec, _)) ->
                            match Map.tryFind name m with
                            | None -> Error [ MissingKey name ]
                            | Some jv -> dec jv |> Result.mapError (List.map (fun e -> Pointer([ name ], e))))

                    let errs =
                        results
                        |> Array.collect (function
                            | Error e -> List.toArray e
                            | Ok _ -> [||])

                    if errs.Length = 0 then
                        Ok(
                            FSharpValue.MakeRecord(
                                t,
                                results
                                |> Array.map (function
                                    | Ok v -> v
                                    | Error _ -> null)
                            )
                        )
                    else
                        Error(List.ofArray errs)
                | _ -> Error [ InvalidType("object", j) ]),
            (fun o ->
                let vals = FSharpValue.GetRecordFields o
                JObject(Array.map2 (fun (name, (_, enc)) v -> name, enc v) codecs vals |> Map.ofArray))
        elif
            FSharpType.IsUnion t
            && (FSharpType.GetUnionCases t |> Array.forall (fun c -> c.GetFields().Length = 0))
        then
            let cases = FSharpType.GetUnionCases t
            let names = cases |> Array.map (fun c -> c.Name) |> String.concat ", "

            (function
            | JString s ->
                match cases |> Array.tryFind (fun c -> c.Name = s) with
                | Some c -> Ok(FSharpValue.MakeUnion(c, [||]))
                | None -> Error [ InvalidValue(sprintf "Expected one of: %s" names, JString s) ]
            | j -> Error [ InvalidType("string", j) ]),
            (fun o -> let c, _ = FSharpValue.GetUnionFields(o, t) in JString c.Name)
        else
            failwithf
                "Schema.derive: unsupported type %s (v1 supports records, options, enum-like DUs, and primitives)"
                t.FullName

    /// Derive a default schema from an F# record / option / enum-like DU, keying
    /// object fields by record-field name. Reflection-based in v1 (Myriad in v2).
    /// (Schema.derive)
    let derive<'T> () : Schema<'T> =
        let dec, enc = deriveCodec typeof<'T>

        { Ast = ADeclare(typeof<'T>.Name)
          Decode = (fun j -> dec j |> Result.map unbox<'T>)
          Encode = (fun v -> enc (box v)) }

    // -- surfaces -------------------------------------------------------------

    let private toError (issues: SchemaIssue list) : SchemaError =
        match issues with
        | [ single ] -> { Issue = single }
        | many -> { Issue = Composite many }

    /// Decode JSON into `'T`, accumulating all issues. (Schema.decode)
    let decode (s: Schema<'T>) (j: Json) : Result<'T, SchemaError> = s.Decode j |> Result.mapError toError

    /// Validate JSON against a schema, discarding the decoded value.
    let validate (s: Schema<'T>) (j: Json) : Result<unit, SchemaError> = decode s j |> Result.map ignore

    /// Encode `'T` back to JSON. (Schema.encode)
    let encode (s: Schema<'T>) (value: 'T) : Json = s.Encode value

    /// Decode into an `Exit`: success or `Failure(Cause([Fail(SchemaError …)]))`.
    /// The failure is a typed `Reason.Fail`, never a `Die` defect.
    let decodeExit (s: Schema<'T>) (j: Json) : Exit<'T, SchemaError> =
        match decode s j with
        | Ok v -> Exit.succeed v
        | Error e -> Exit.fail e

    /// Decode into the `Effect` channel (so v2 effectful filters/services compose).
    let decodeEffect (s: Schema<'T>) (j: Json) : Effect<'T, SchemaError, 'R> = Effect.fromResult (decode s j)

    // -- error rendering (reuses the merged Formatter) ------------------------

    let rec private jsonToObj (j: Json) : obj =
        match j with
        | JNull -> null
        | JBool b -> box b
        | JNumber n -> box n
        | JString s -> box s
        | JArray xs -> box (xs |> List.map jsonToObj)
        | JObject m -> box (m |> Map.map (fun _ v -> jsonToObj v))

    let private at (path: string list) : string =
        match path with
        | [] -> ""
        | _ -> " at " + Formatter.formatPath path

    let rec private renderIssue (path: string list) (issue: SchemaIssue) : string list =
        match issue with
        | InvalidType(expected, actual) ->
            [ sprintf "Expected %s, got %s%s" expected (Formatter.format (jsonToObj actual)) (at path) ]
        | InvalidValue(message, actual) ->
            [ sprintf "%s, got %s%s" message (Formatter.format (jsonToObj actual)) (at path) ]
        | MissingKey key -> [ sprintf "Missing key%s" (at (path @ [ key ])) ]
        | UnexpectedKey key -> [ sprintf "Unexpected key%s" (at (path @ [ key ])) ]
        | Forbidden message -> [ sprintf "%s%s" message (at path) ]
        | Pointer(p, inner) -> renderIssue (path @ p) inner
        | Composite issues -> issues |> List.collect (renderIssue path)
        | AnyOf issues ->
            [ "No union member matched:" ]
            @ (issues |> List.collect (renderIssue path) |> List.map (sprintf "  %s"))

    /// Render a `SchemaError` into human-readable, path-annotated lines.
    /// (Schema/Formatter error rendering)
    let format (error: SchemaError) : string =
        renderIssue [] error.Issue |> String.concat "\n"
