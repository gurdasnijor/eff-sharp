namespace Effect

/// Schema v1.1 — a **type-first, schema-as-codec** validation/parsing library.
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
/// What changed from v1 (the "exploit F#/Fable, diverge from Effect where it
/// pays" pass):
///
///   1. The `Ast` is now **structural, not decorative**. `AObject` carries each
///      field's child `SchemaAst` (v1 kept only key *names*), and the object
///      builder threads child ASTs through. This is the prerequisite for every
///      AST-fold interpreter below — without a complete tree there is nothing to
///      fold over.
///   2. Four operations from Effect's table fall out of {Encode, AST}:
///        `equivalence` (Encode-equality), `pretty` (AST-guided render),
///        `jsonSchema` (pure AST fold), and `arbitrary` (in `Schema.FsCheck.fs`,
///        so the FsCheck dep never touches this module).
///   3. **Units of measure** schemas (`measured`) — dimensional safety *inside*
///      the parse boundary, erased to a plain number under Fable. Effect has no
///      type-level home for this.
///   4. **Tagged unions** via the DU runtime tag (`taggedUnion`) — lossless
///      round-trip, closing v1's documented "general union encoding is ambiguous".
///   5. Accumulation lives in one place: the `Validation<'T>` applicative. Every
///      composite combinator runs through `Validation.zip`/`sequence` instead of
///      re-implementing `match ra, rb with` plumbing.
///   6. `(|Decoded|_|)` active pattern so consumers can match on a schema.
///
/// Decoding accumulates a structured `SchemaIssue` tree; a validation failure is a
/// typed `Reason.Fail` carried in a `Cause` (NEVER a `Die` defect) so
/// `decodeExit` yields `Failure(Cause([Fail(SchemaError …)]))` — matching upstream
/// rendering — and `decodeEffect` lifts into the `Effect` channel.
///
/// Dropped vs Effect: `Standard Schema` (a TS-ecosystem interop contract; no F#
/// meaning). Deferred to v2: Myriad source-gen (real Fable-safe derivation,
/// replacing the reflection path + JS stub below), `Schema.diff` (via JsonPatch,
/// over the shared `Json`), recursive `suspend` (now far cheaper given the
/// structural AST + `ADeclare`). See DESIGN.md "Phased plan".

open System
open Microsoft.FSharp.Reflection

/// Reified shape of a schema. **Structural**: composites carry their children, so
/// the AST alone is enough to derive JSON Schema, equivalence, pretty printers,
/// and arbitraries. (v1 stored only object key names — that lossiness is why none
/// of those interpreters could exist.)
type SchemaAst =
    | AString
    | AInt
    | AFloat
    | ABool
    | ADecimal
    | ALiteral of Json
    | AArray of SchemaAst
    | ATuple of SchemaAst list
    | AObject of fields: (string * SchemaAst) list
    | AOption of SchemaAst
    | AUnion of SchemaAst list
    | ATaggedUnion of cases: (string * SchemaAst) list
    | ARefine of SchemaAst * description: string
    | AMeasured of SchemaAst
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

/// An accumulating validation result: `Result`-shaped so it composes with the rest
/// of the ecosystem, but its applicative `apply` **concatenates** errors rather
/// than short-circuiting. This is the property that keeps the port Effect-faithful
/// (and that a fail-fast decoder library like Thoth gives up). It is the single
/// home for accumulation — every composite combinator runs through it.
type Validation<'T> = Result<'T, SchemaIssue list>

/// A reified, bidirectional codec for `'T`. `Decode` returns accumulated issues so
/// composite schemas (structs/tuples/arrays) report every failure at once.
type Schema<'T> =
    { Decode: Json -> Validation<'T>
      Encode: 'T -> Json
      Ast: SchemaAst }

/// An applicative object-field codec used by the `Schema.object { }` builder. It
/// is bidirectional: `DecodeFields` reads the keys it owns out of a JSON object
/// (accumulating issues), `EncodeFields` projects them back out of the record, and
/// `Fields` carries each owned key's child AST so the finished struct schema has a
/// complete `AObject`.
type ObjectCodec<'R, 'A> =
    { DecodeFields: Map<string, Json> -> Validation<'A>
      EncodeFields: 'R -> (string * Json) list
      Keys: string list
      Fields: (string * SchemaAst) list }

/// The accumulating applicative over `Validation<'T>`. `apply`/`zip` concatenate
/// the error channel; this is where "report all failures" is implemented, once.
[<RequireQualifiedAccess>]
module Validation =

    let ok (v: 'T) : Validation<'T> = Ok v
    let error (issues: SchemaIssue list) : Validation<'T> = Error issues

    let map (f: 'a -> 'b) (v: Validation<'a>) : Validation<'b> = Result.map f v
    let mapError (f: SchemaIssue list -> SchemaIssue list) (v: Validation<'a>) : Validation<'a> = Result.mapError f v

    /// The accumulating apply: on two failures, both error lists are concatenated.
    let apply (vf: Validation<'a -> 'b>) (va: Validation<'a>) : Validation<'b> =
        match vf, va with
        | Ok f, Ok a -> Ok(f a)
        | Error e1, Error e2 -> Error(e1 @ e2)
        | Error e, Ok _ -> Error e
        | Ok _, Error e -> Error e

    let map2 (f: 'a -> 'b -> 'c) (a: Validation<'a>) (b: Validation<'b>) : Validation<'c> = apply (map f a) b

    let zip (a: Validation<'a>) (b: Validation<'b>) : Validation<'a * 'b> = map2 (fun x y -> x, y) a b

    let zip3 (a: Validation<'a>) (b: Validation<'b>) (c: Validation<'c>) : Validation<'a * 'b * 'c> =
        apply (apply (map (fun x y z -> x, y, z) a) b) c

    /// Run every element, concatenating all issues. Replaces v1's `sequenceAccum`.
    let sequence (results: Validation<'a> list) : Validation<'a list> =
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

[<RequireQualifiedAccess>]
module Schema =

    open System.Text.RegularExpressions

#if FABLE_COMPILER || NETSTANDARD2_0
    let inline private isIntegerNumber (n: float) =
        not (Double.IsNaN n || Double.IsInfinity n) && floor n = n
#else
    let inline private isIntegerNumber (n: float) = Double.IsInteger n
#endif

    // -- numeric conversions, captured BEFORE the `int`/`float`/`decimal` schema
    //    values shadow the built-in operators for the rest of the module. --
    let inline private toInt (n: float) : int = int n
    let inline private toFloat (i: int) : float = float i
    let inline private toDecimal (n: float) : decimal = decimal n
    let inline private ofDecimal (d: decimal) : float = float d
    let inline private eraseMeasure<[<Measure>] 'u> (n: float<'u>) : float = float n
    let inline private toText (value: 'T) : string = string value

    // -- tagged-union JSON tag helpers ----------------------------------------

    /// The discriminator key injected into / read from tagged-union JSON objects.
    let [<Literal>] private TagKey = "_tag"

    /// Inject the discriminator into an encoded case. For the (rare) case schema
    /// that encodes to a non-object payload we nest it under "value".
    let private addTag (tag: string) (j: Json) : Json =
        match j with
        | JObject m -> JObject(Map.add TagKey (JString tag) m)
        | other -> JObject(Map.ofList [ TagKey, JString tag; "value", other ])

    let private readTag (j: Json) : string option =
        match j with
        | JObject m ->
            match Map.tryFind TagKey m with
            | Some(JString t) -> Some t
            | _ -> None
        | _ -> None

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
            // `n % 1.0 = 0.0` is the Fable-portable equal of `Double.IsInteger`
            // (false for NaN / infinity, true for finite integers).
            (function
            | JNumber n when n % 1.0 = 0.0 -> Ok(toInt n)
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
    ///
    /// NOTE: the predicate is opaque to the AST (`ARefine` keeps only a
    /// description). So JSON Schema annotates but cannot express it, and arbitrary
    /// generation falls back to generate-and-filter. This mirrors how Effect
    /// treats refinements — see `Schema.FsCheck.fs` for the filtering hazard.
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
          Decode = (fun j -> s.Decode j |> Validation.map forward)
          Encode = (fun u -> s.Encode(backward u)) }

    // -- units of measure -----------------------------------------------------

    /// Lift a `Schema<float>` into a dimensionally-typed `Schema<float<'u>>`. The
    /// measure is erased before IL (and before Fable's JS), so the wire form is an
    /// ordinary JSON number and there is zero runtime cost — but inside the parse
    /// boundary a latency, a byte count, and an offset are no longer
    /// interchangeable. This is the headline F#-over-TS exploit: Effect has no
    /// type-level place to put a unit.
    ///
    ///   let latencyMs : Schema<float<ms>> =
    ///       Schema.float |> Schema.between 0.0 60_000.0 |> Schema.measured
    ///
    /// (`int<'u>` is analogous over `Schema.int`; add a sibling if you need it.)
    let measured<[<Measure>] 'u> (s: Schema<float>) : Schema<float<'u>> =
        { Ast = AMeasured s.Ast
          Decode = (fun j -> s.Decode j |> Validation.map LanguagePrimitives.FloatWithMeasure<'u>)
          Encode = (fun (v: float<'u>) -> s.Encode(eraseMeasure v)) }

    // -- composites -----------------------------------------------------------

    /// Wrap a schema so `null` decodes to `None`. (Schema.optionalKey-ish)
    ///
    /// This handles a *present-but-null* value; an absent object key is still a
    /// `MissingKey` at the `field` level (key required, value nullable — which is
    /// exactly what `jsonSchema` reflects). A true absence-tolerant `optionalKey`
    /// is a v2 combinator.
    let option (s: Schema<'T>) : Schema<'T option> =
        { Ast = AOption s.Ast
          Decode =
            (function
            | JNull -> Ok None
            | j -> s.Decode j |> Validation.map Some)
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
                    |> Validation.mapError (List.map (fun e -> Pointer([ toText i ], e))))
                |> Validation.sequence
            | j -> Error [ InvalidType("array", j) ]) }

    /// A 2-tuple `[a, b]`. (Schema.Tuple — v1 ships tuple2/tuple3.)
    let tuple2 (a: Schema<'A>) (b: Schema<'B>) : Schema<'A * 'B> =
        { Ast = ATuple [ a.Ast; b.Ast ]
          Encode = (fun (x, y) -> JArray [ a.Encode x; b.Encode y ])
          Decode =
            (function
            | JArray [ ja; jb ] ->
                let ra = a.Decode ja |> Validation.mapError (List.map (fun e -> Pointer([ "0" ], e)))
                let rb = b.Decode jb |> Validation.mapError (List.map (fun e -> Pointer([ "1" ], e)))
                Validation.zip ra rb
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
                let ra = a.Decode ja |> Validation.mapError (List.map (fun e -> Pointer([ "0" ], e)))
                let rb = b.Decode jb |> Validation.mapError (List.map (fun e -> Pointer([ "1" ], e)))
                let rc = c.Decode jc |> Validation.mapError (List.map (fun e -> Pointer([ "2" ], e)))
                Validation.zip3 ra rb rc
            | JArray items ->
                Error
                    [ InvalidValue(
                          sprintf "Expected a tuple of length 3, got length %d" (List.length items),
                          JArray items
                      ) ]
            | j -> Error [ InvalidType("array", j) ]) }

    /// Try each member schema in order; first success wins, otherwise all member
    /// issues are grouped under `AnyOf`. Untagged union encoding is ambiguous so v1
    /// encodes via the first member — for a *lossless* discriminated union prefer
    /// `taggedUnion` (below) or `literalMap` for enums. (Schema.Union)
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

    /// A **discriminated** union keyed by an F# DU's runtime tag — the exploit TS
    /// structural unions can't make: the case name *is* the discriminator, so the
    /// round-trip is lossless without a hand-maintained `_tag` field.
    ///
    /// `cases` pairs each tag with a `Schema<'T>` that decodes/encodes the whole
    /// value for that case (typically a `Schema.object { }`). Decode reads `_tag`
    /// from the JSON and dispatches; encode picks the member via `tagOf` and
    /// injects `_tag`. On .NET, `tagOf` can be derived with
    /// `FSharpValue.GetUnionFields`; on Fable pass the case-name selector
    /// explicitly. `cases`/`tagOf` must be exhaustive over the DU (an unknown tag
    /// on encode yields `JNull`). (≈ Schema.TaggedUnion / Schema.Union + discriminator)
    let taggedUnion (cases: (string * Schema<'T>) list) (tagOf: 'T -> string) : Schema<'T> =
        { Ast = ATaggedUnion(cases |> List.map (fun (t, s) -> t, s.Ast))
          Encode =
            (fun v ->
                let tag = tagOf v

                match cases |> List.tryFind (fun (t, _) -> t = tag) with
                | Some(_, s) -> addTag tag (s.Encode v)
                | None -> JNull)
          Decode =
            (fun j ->
                match readTag j with
                | Some tag ->
                    match cases |> List.tryFind (fun (t, _) -> t = tag) with
                    | Some(_, s) -> s.Decode j
                    | None -> Error [ InvalidValue(sprintf "Unknown tag %A" tag, j) ]
                | None -> Error [ MissingKey TagKey ]) }

    // -- applicative struct builder -------------------------------------------

    /// One required object field, carrying its decoder, a getter (so the struct is
    /// bidirectional), and its child AST (so the finished struct has a complete
    /// `AObject`). Use inside `Schema.object { }`.
    let field (key: string) (schema: Schema<'F>) (getter: 'R -> 'F) : ObjectCodec<'R, 'F> =
        { Keys = [ key ]
          Fields = [ key, schema.Ast ]
          EncodeFields = (fun r -> [ key, schema.Encode(getter r) ])
          DecodeFields =
            (fun m ->
                match Map.tryFind key m with
                | None -> Error [ MissingKey key ]
                | Some j -> schema.Decode j |> Validation.mapError (List.map (fun e -> Pointer([ key ], e)))) }

    /// Applicative builder for struct schemas. `let!`/`and!` run every field
    /// decoder and accumulate ALL issues (via `Validation.zip`); `return` builds
    /// the record (so a field mismatch is a *compile* error). Bidirectional via the
    /// field getters; structural via the threaded child ASTs.
    type ObjectBuilder() =
        member _.MergeSources(a: ObjectCodec<'R, 'A>, b: ObjectCodec<'R, 'B>) : ObjectCodec<'R, 'A * 'B> =
            { Keys = a.Keys @ b.Keys
              Fields = a.Fields @ b.Fields
              EncodeFields = (fun r -> a.EncodeFields r @ b.EncodeFields r)
              DecodeFields = (fun m -> Validation.zip (a.DecodeFields m) (b.DecodeFields m)) }

        member _.BindReturn(c: ObjectCodec<'R, 'A>, f: 'A -> 'R) : Schema<'R> =
            { Ast = AObject c.Fields
              Encode = (fun r -> JObject(Map.ofList (c.EncodeFields r)))
              Decode =
                (function
                | JObject m -> c.DecodeFields m |> Validation.map f
                | j -> Error [ InvalidType("object", j) ]) }

    /// `Schema.object { let! a = field ...; and! b = field ...; return record }`
    let object = ObjectBuilder()

    // -- active patterns ------------------------------------------------------

    /// Let a schema *be* a pattern: `match json with | Decoded userSchema u -> ...`.
    /// Pure F# surface sugar with no Effect analog — reads native at the call site.
    let (|Decoded|_|) (schema: Schema<'T>) (json: Json) : 'T option =
        match schema.Decode json with
        | Ok v -> Some v
        | Error _ -> None

    // -- reflection-based derivation (records / option / enum-like DUs) --------
    //
    // Reflective auto-derive: inspects an F# type at runtime via FSharpType /
    // FSharpValue, and now ALSO builds a structural `SchemaAst` so derived schemas
    // are first-class for `jsonSchema` / arbitraries (v1 returned a bare
    // `ADeclare name`, which would have produced a dangling `$ref`). Fable erases
    // generic type info at compile time, so this entire path is .NET-only; on the
    // Fable/JS target `derive` fails fast — build schemas with the explicit
    // combinators (`Schema.object` / primitives), which is all the JS consumers
    // (e.g. fluent-firegrid) ever use. v2 replaces this with Myriad source-gen.
#if !FABLE_COMPILER

    let rec private deriveCodec (t: Type) : (Json -> Validation<obj>) * (obj -> Json) * SchemaAst =
        if t = typeof<string> then
            (function
            | JString s -> Ok(box s)
            | j -> Error [ InvalidType("string", j) ]),
            (fun o -> JString(unbox<string> o)),
            AString
        elif t = typeof<bool> then
            (function
            | JBool b -> Ok(box b)
            | j -> Error [ InvalidType("boolean", j) ]),
            (fun o -> JBool(unbox<bool> o)),
            ABool
        elif t = typeof<int> then
            (function
            | JNumber n when isIntegerNumber n -> Ok(box (toInt n))
            | JNumber n -> Error [ InvalidValue("Expected an integer", JNumber n) ]
            | j -> Error [ InvalidType("number", j) ]),
            (fun o -> JNumber(toFloat (unbox<int> o))),
            AInt
        elif t = typeof<float> then
            (function
            | JNumber n -> Ok(box n)
            | j -> Error [ InvalidType("number", j) ]),
            (fun o -> JNumber(unbox<float> o)),
            AFloat
        elif t = typeof<decimal> then
            (function
            | JNumber n -> Ok(box (toDecimal n))
            | j -> Error [ InvalidType("number", j) ]),
            (fun o -> JNumber(ofDecimal (unbox<decimal> o))),
            ADecimal
        elif t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<_ option> then
            let inner = t.GetGenericArguments().[0]
            let dec, enc, innerAst = deriveCodec inner
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
                | _ -> JNull),
            AOption innerAst
        elif FSharpType.IsRecord t then
            let fields = FSharpType.GetRecordFields t
            let codecs = fields |> Array.map (fun pi -> pi.Name, deriveCodec pi.PropertyType)
            let ast = AObject(codecs |> Array.map (fun (name, (_, _, a)) -> name, a) |> Array.toList)

            let decode j =
                match j with
                | JObject m ->
                    let results =
                        codecs
                        |> Array.map (fun (name, (dec, _, _)) ->
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
                | _ -> Error [ InvalidType("object", j) ]

            let encode (o: obj) =
                let vals = FSharpValue.GetRecordFields o
                JObject(Array.map2 (fun (name, (_, enc, _)) v -> name, enc v) codecs vals |> Map.ofArray)

            decode, encode, ast
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
            (fun o -> let c, _ = FSharpValue.GetUnionFields(o, t) in JString c.Name),
            AUnion(cases |> Array.map (fun c -> ALiteral(JString c.Name)) |> Array.toList)
        else
            failwithf
                "Schema.derive: unsupported type %s (v1 supports records, options, enum-like DUs, and primitives)"
                t.FullName

    /// Derive a default schema from an F# record / option / enum-like DU, keying
    /// object fields by record-field name, with a structural AST. Reflection-based
    /// in v1 (Myriad in v2). (Schema.derive)
    let derive<'T> () : Schema<'T> =
        let dec, enc, ast = deriveCodec typeof<'T>

        { Ast = ast
          Decode = (fun j -> dec j |> Result.map unbox<'T>)
          Encode = (fun v -> enc (box v)) }
#else
    /// Reflective derivation is unavailable on the Fable/JS target (generic type
    /// info is erased at compile time). Build schemas with the explicit
    /// combinators instead. (Schema.derive — JS stub)
    let derive<'T> () : Schema<'T> =
        failwith
            "Schema.derive (reflective type derivation) is not supported on the Fable/JS target — build the schema explicitly with the combinators (Schema.object / Schema.string / ...)."
#endif

    // -- decode / encode surfaces ---------------------------------------------

    let private toError (issues: SchemaIssue list) : SchemaError =
        match issues with
        | [ single ] -> { Issue = single }
        | many -> { Issue = Composite many }

    /// Decode JSON into `'T`, accumulating all issues. (Schema.decode)
    let decode (s: Schema<'T>) (j: Json) : Result<'T, SchemaError> = s.Decode j |> Result.mapError toError

    /// Validate JSON against a schema, discarding the decoded value. (≈ asserting)
    let validate (s: Schema<'T>) (j: Json) : Result<unit, SchemaError> = decode s j |> Result.map ignore

    /// Boolean assertion form. (Schema.is)
    let isValid (s: Schema<'T>) (j: Json) : bool =
        match s.Decode j with
        | Ok _ -> true
        | Error _ -> false

    /// Encode `'T` back to JSON. (Schema.encode)
    let encode (s: Schema<'T>) (value: 'T) : Json = s.Encode value

    /// Type-erase a codec for heterogeneous runtime registries. Keep this as the
    /// only boxing boundary so the executable codec and AST cannot drift.
    let erase (s: Schema<'T>) : Schema<obj> =
        { Ast = s.Ast
          Decode = fun j -> s.Decode j |> Result.map box
          Encode = fun value -> s.Encode (unbox<'T> value) }

    /// Decode into an `Exit`: success or `Failure(Cause([Fail(SchemaError …)]))`.
    /// The failure is a typed `Reason.Fail`, never a `Die` defect.
    let decodeExit (s: Schema<'T>) (j: Json) : Exit<'T, SchemaError> =
        match decode s j with
        | Ok v -> Exit.succeed v
        | Error e -> Exit.fail e

    /// Decode into the `Effect` channel (so v2 effectful filters/services compose).
    let decodeEffect (s: Schema<'T>) (j: Json) : Effect<'T, SchemaError, 'R> = Effect.fromResult (decode s j)

    // == AST-fold interpreters ================================================
    //
    // The payoff of making the AST structural: four of Effect's seven operations
    // are now folds over `s.Ast` (optionally reading `s.Encode`). None of these
    // could exist over an opaque decoder function — there'd be nothing to walk.

    // -- equivalence ----------------------------------------------------------

    /// Structural equivalence = equality of the *encoded* form. One line, because
    /// the encoded `Json` is canonical. (Schema.equivalence)
    ///
    /// Inherits any lossiness in `Encode` (e.g. `decimal`, first-member `union`
    /// encoding) and JSON float semantics (NaN ≠ NaN) — fine for value identity in
    /// step-replay checks; know it before relying on it for, say, dedup keys.
    let equivalence (s: Schema<'T>) : 'T -> 'T -> bool =
        fun a b -> s.Encode a = s.Encode b

    // -- pretty printing ------------------------------------------------------

    let rec private prettyJson (j: Json) : string =
        match j with
        | JNull -> "null"
        | JBool b -> if b then "true" else "false"
        | JNumber n -> toText n
        | JString s -> "\"" + s + "\""
        | JArray xs -> "[" + (xs |> List.map prettyJson |> String.concat "; ") + "]"
        | JObject m ->
            "{ "
            + (m |> Map.toList |> List.map (fun (k, v) -> sprintf "%s: %s" k (prettyJson v)) |> String.concat ", ")
            + " }"

    let rec private prettyValue (ast: SchemaAst) (j: Json) : string =
        match ast, j with
        | ADeclare name, _ -> name + " " + prettyJson j
        | AMeasured inner, _ -> prettyValue inner j
        | ARefine(inner, _), _ -> prettyValue inner j
        | AOption _, JNull -> "None"
        | AOption inner, _ -> "Some " + prettyValue inner j
        | AObject fields, JObject m ->
            let body =
                fields
                |> List.map (fun (k, a) ->
                    match Map.tryFind k m with
                    | Some v -> sprintf "%s = %s" k (prettyValue a v)
                    | None -> sprintf "%s = <missing>" k)
                |> String.concat "; "

            "{ " + body + " }"
        | AArray a, JArray xs -> "[" + (xs |> List.map (prettyValue a) |> String.concat "; ") + "]"
        | ATuple asts, JArray xs when List.length asts = List.length xs ->
            "(" + (List.map2 prettyValue asts xs |> String.concat ", ") + ")"
        | ATaggedUnion cases, JObject m ->
            match Map.tryFind TagKey m with
            | Some(JString tag) ->
                match cases |> List.tryFind (fun (t, _) -> t = tag) with
                | Some(_, a) -> tag + " " + prettyValue a j
                | None -> prettyJson j
            | _ -> prettyJson j
        | _ -> prettyJson j // primitives, untagged unions, structural fallbacks

    /// Pretty-print a `'T` by AST-guided rendering of its encoded form. The AST
    /// earns its place via field names/order, Some/None, and tagged dispatch.
    /// (Schema.pretty — `render ast (encode value)`)
    let pretty (s: Schema<'T>) (value: 'T) : string = prettyValue s.Ast (s.Encode value)

    // -- JSON Schema (draft-07) ----------------------------------------------

    let rec private astToJsonSchema (ast: SchemaAst) : Json =
        let jobj pairs = JObject(Map.ofList pairs)

        match ast with
        | AString -> jobj [ "type", JString "string" ]
        | AInt -> jobj [ "type", JString "integer" ]
        | AFloat -> jobj [ "type", JString "number" ]
        | ADecimal -> jobj [ "type", JString "number" ]
        | ABool -> jobj [ "type", JString "boolean" ]
        | ALiteral v -> jobj [ "const", v ]
        | AArray a -> jobj [ "type", JString "array"; "items", astToJsonSchema a ]
        | ATuple asts ->
            let n = List.length asts

            jobj
                [ "type", JString "array"
                  "items", JArray(List.map astToJsonSchema asts)
                  "minItems", JNumber(toFloat n)
                  "maxItems", JNumber(toFloat n)
                  "additionalItems", JBool false ]
        | AObject fields ->
            // Every field is a required *key* (value may be null for AOption) —
            // this matches the decoder, which raises MissingKey on absence.
            jobj
                [ "type", JString "object"
                  "properties", JObject(fields |> List.map (fun (k, a) -> k, astToJsonSchema a) |> Map.ofList)
                  "required", JArray(fields |> List.map (fun (k, _) -> JString k))
                  "additionalProperties", JBool false ]
        | AOption a -> jobj [ "anyOf", JArray [ astToJsonSchema a; jobj [ "type", JString "null" ] ] ]
        | AUnion asts -> jobj [ "anyOf", JArray(List.map astToJsonSchema asts) ]
        | ATaggedUnion cases ->
            let variant (tag, a) =
                match astToJsonSchema a with
                | JObject m ->
                    let props =
                        match Map.tryFind "properties" m with
                        | Some(JObject p) -> p
                        | _ -> Map.empty

                    let props' = Map.add TagKey (jobj [ "const", JString tag ]) props

                    let required =
                        match Map.tryFind "required" m with
                        | Some(JArray r) -> r
                        | _ -> []

                    let required' = JString TagKey :: required

                    JObject(m |> Map.add "properties" (JObject props') |> Map.add "required" (JArray required'))
                | other -> other

            jobj [ "oneOf", JArray(cases |> List.map variant) ]
        | ARefine(a, desc) ->
            // The predicate is opaque; annotate with the description only.
            match astToJsonSchema a with
            | JObject m -> JObject(Map.add "description" (JString desc) m)
            | other -> other
        | AMeasured a -> astToJsonSchema a // unit is erased; not expressible in JSON Schema
        | ADeclare name -> jobj [ "$ref", JString("#/$defs/" + name) ]

    /// Emit a draft-07 JSON Schema document for the schema. Pure fold over the AST.
    /// (Schema.jsonSchema)
    let jsonSchema (s: Schema<'T>) : Json =
        match astToJsonSchema s.Ast with
        | JObject m -> JObject(Map.add "$schema" (JString "http://json-schema.org/draft-07/schema#") m)
        | other -> other

    // Arbitrary generation is the fourth AST-fold interpreter, but it depends on
    // FsCheck — it lives in `Schema.FsCheck.fs` (module `SchemaGen`) so this core
    // module carries no test dependency.

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
