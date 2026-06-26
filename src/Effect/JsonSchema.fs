namespace Effect

open System.Text

/// Normalizing and converting JSON Schema / OpenAPI documents.
///
/// Port of repos/effect-smol/packages/effect/src/JsonSchema.ts. Supported inputs:
/// JSON Schema Draft-07, Draft 2020-12, OpenAPI 3.0 and 3.1; conversions
/// normalize through `Document` (dialect `Draft2020_12`) before emitting another
/// dialect.
///
/// Porting notes (see CONVENTIONS.md):
///   * The open `JsonSchema` object (`[x: string]: unknown`) is the shared `Json`
///     DU (defined in JsonPatch.fs); object members are a `Map<string, Json>`,
///     so equality is order-independent — matching upstream's `deepStrictEqual`.
///   * `Dialect` is a DU rather than a string-literal union; `Document` /
///     `MultiDocument` drop the phantom dialect type parameter and carry the
///     dialect as a field.
///   * `resolve$ref` / `resolveTopLevel$ref` are renamed `resolveRef` /
///     `resolveTopLevelRef` (`$` is not a valid F# identifier char).
///   * The anchored prefix regexes (`/^#\/definitions(?=\/|$)/` etc.) are
///     expressed as a boundary-aware `rewritePrefix` string helper.
/// The JSON Schema dialects this module understands. (JsonSchema.Dialect)
type Dialect =
    | Draft07
    | Draft2020_12
    | OpenApi31
    | OpenApi30

/// A record of named JSON Schema definitions, keyed by name. (JsonSchema.Definitions)
type Definitions = Map<string, Json>

/// A root schema together with its shared definitions. (JsonSchema.Document)
type Document =
    { Dialect: Dialect
      Schema: Json
      Definitions: Definitions }

/// Like `Document`, but with several root schemas sharing one definitions pool. (JsonSchema.MultiDocument)
type MultiDocument =
    { Dialect: Dialect
      Schemas: Json list
      Definitions: Definitions }

[<RequireQualifiedAccess>]
module JsonSchema =

    /// The `$schema` meta-schema URI for JSON Schema Draft-07.
    [<Literal>]
    let META_SCHEMA_URI_DRAFT_07 = "http://json-schema.org/draft-07/schema"

    /// The `$schema` meta-schema URI for JSON Schema Draft 2020-12.
    [<Literal>]
    let META_SCHEMA_URI_DRAFT_2020_12 = "https://json-schema.org/draft/2020-12/schema"

    // --- internal helpers ---

    /// Replaces an anchored `#/...` ref prefix, honouring the `(?=/|$)` boundary
    /// of the upstream regexes (only matches at a path-segment boundary).
    let private rewritePrefix (segment: string) (replacement: string) (s: string) : string =
        if s = segment then
            replacement
        elif s.StartsWith(segment + "/") then
            replacement + s.Substring(segment.Length)
        else
            s

    /// Recursively rewrites every `$ref` string value with `f`. (internal rewrite_refs)
    let rec private rewriteRefs (f: string -> string) (node: Json) : Json =
        match node with
        | JArray xs -> JArray(xs |> List.map (rewriteRefs f))
        | JObject m ->
            JObject(
                m
                |> Map.map (fun k v ->
                    match k, v with
                    | "$ref", JString s -> JString(f s)
                    | "$ref", _ -> v
                    | _, (JArray _ | JObject _) -> rewriteRefs f v
                    | _ -> v)
            )
        | other -> other

    let private isValidKeyChar (cp: int) : bool =
        (cp >= 48 && cp <= 57) // 0-9
        || (cp >= 65 && cp <= 90) // A-Z
        || (cp >= 97 && cp <= 122) // a-z
        || cp = 46 // .
        || cp = 45 // -
        || cp = 95 // _

    /// Returns a key valid for OpenAPI `components.schemas` (matching
    /// `^[a-zA-Z0-9.\-_]+$`), replacing every other code point with `_`.
    /// (JsonSchema.sanitizeOpenApiComponentsSchemasKey)
    let sanitizeOpenApiComponentsSchemasKey (s: string) : string =
        if s.Length = 0 then
            "_"
        else
            let sb = StringBuilder()

#if FABLE_COMPILER || NETSTANDARD2_0
            // Fable lacks Rune / EnumerateRunes; iterate UTF-16 chars — sufficient
            // for key sanitization (non-BMP code points fall through to '_').
            for c in s do
                if isValidKeyChar (int c) then
                    sb.Append(c) |> ignore
                else
                    sb.Append('_') |> ignore
#else
            for r in s.EnumerateRunes() do
                if isValidKeyChar r.Value then
                    sb.Append(r.ToString()) |> ignore
                else
                    sb.Append('_') |> ignore
#endif

            sb.ToString()

    // Keyword allow-lists (everything else is dropped during a walk).
    let private draft07Keep =
        set
            [ "type"
              "required"
              "enum"
              "const"
              "title"
              "description"
              "default"
              "examples"
              "format"
              "readOnly"
              "writeOnly"
              "pattern"
              "minimum"
              "maximum"
              "exclusiveMinimum"
              "exclusiveMaximum"
              "minLength"
              "maxLength"
              "minItems"
              "maxItems"
              "minProperties"
              "maxProperties"
              "multipleOf"
              "uniqueItems" ]

    let private to07Keep =
        set
            [ "$ref"
              "type"
              "required"
              "enum"
              "const"
              "title"
              "description"
              "default"
              "examples"
              "format"
              "pattern"
              "minimum"
              "maximum"
              "exclusiveMinimum"
              "exclusiveMaximum"
              "minLength"
              "maxLength"
              "minItems"
              "maxItems"
              "minProperties"
              "maxProperties"
              "multipleOf"
              "uniqueItems" ]

    // --- decoding ---

    /// Parses a raw Draft-07 JSON Schema into a canonical `Document`. Rewrites
    /// `#/definitions/...` refs to `#/$defs/...`, lifts Draft-07 tuple `items`
    /// to `prefixItems`, extracts root `definitions`, and drops unsupported
    /// keywords. (JsonSchema.fromSchemaDraft07)
    let fromSchemaDraft07 (js: Json) : Document =
        let mutable definitions: Definitions option = None

        let rec walk (node: Json) (isRoot: bool) : Json =
            match node with
            | JArray xs -> JArray(xs |> List.map (fun v -> walk v false))
            | JObject src ->
                let mutable out: Definitions = Map.empty
                let mutable prefixItems: Json option = None
                let mutable additionalItems: Json option = None

                for KeyValue(k, v) in src do
                    match k with
                    | "$ref" ->
                        let nv =
                            match v with
                            | JString s -> JString(rewritePrefix "#/definitions" "#/$defs" s)
                            | _ -> v

                        out <- Map.add "$ref" nv out
                    | "definitions" ->
                        let mapped = walkObject v

                        if isRoot then
                            definitions <- mapped
                        else
                            out <-
                                Map.add
                                    "definitions"
                                    (match mapped with
                                     | Some d -> JObject d
                                     | None -> v)
                                    out
                    | "items" -> prefixItems <- Some v
                    | "additionalItems" -> additionalItems <- Some v
                    | "properties"
                    | "patternProperties" ->
                        let mapped = walkObject v

                        out <-
                            Map.add
                                k
                                (match mapped with
                                 | Some d -> JObject d
                                 | None -> v)
                                out
                    | "additionalProperties"
                    | "propertyNames" -> out <- Map.add k (walk v false) out
                    | "allOf"
                    | "anyOf"
                    | "oneOf" ->
                        let nv =
                            match v with
                            | JArray xs -> JArray(xs |> List.map (fun x -> walk x false))
                            | _ -> v

                        out <- Map.add k nv out
                    | _ when Set.contains k draft07Keep -> out <- Map.add k v out
                    | _ -> ()

                // Draft-07 tuples -> 2020-12 tuples
                match prefixItems with
                | Some(JArray xs) ->
                    out <- Map.add "prefixItems" (JArray(xs |> List.map (fun x -> walk x false))) out

                    match additionalItems with
                    | Some ai -> out <- Map.add "items" (walk ai false) out
                    | None -> ()
                | Some pi -> out <- Map.add "items" (walk pi false) out
                | None -> ()

                JObject out
            | other -> other

        and walkObject (v: Json) : Definitions option =
            match v with
            | JObject m -> Some(m |> Map.map (fun _ x -> walk x false))
            | _ -> None

        { Dialect = Draft2020_12
          Schema = walk js true
          Definitions = defaultArg definitions Map.empty }

    /// Parses a raw Draft 2020-12 JSON Schema into a `Document`, splitting `$defs`
    /// from the root schema without rewriting any keywords.
    /// (JsonSchema.fromSchemaDraft2020_12)
    let fromSchemaDraft2020_12 (js: Json) : Document =
        match js with
        | JObject m ->
            let definitions =
                match Map.tryFind "$defs" m with
                | Some(JObject d) -> d
                | _ -> Map.empty

            { Dialect = Draft2020_12
              Schema = JObject(Map.remove "$defs" m)
              Definitions = definitions }
        | _ ->
            { Dialect = Draft2020_12
              Schema = js
              Definitions = Map.empty }

    /// Parses a raw OpenAPI 3.1 JSON Schema into a `Document`, rewriting
    /// `#/components/schemas/...` refs to `#/$defs/...`. (JsonSchema.fromSchemaOpenApi3_1)
    let fromSchemaOpenApi3_1 (js: Json) : Document =
        fromSchemaDraft2020_12 (rewriteRefs (rewritePrefix "#/components/schemas" "#/$defs") js)

    // OpenAPI 3.0 -> Draft-07 normalization (nullable, singular example, boolean exclusivity).

    let private widenType (node: Map<string, Json>) : Map<string, Json> =
        match Map.tryFind "type" node with
        | Some(JString t) ->
            if t = "null" then
                node
            else
                Map.add "type" (JArray [ JString t; JString "null" ]) node
        | Some(JArray ts) ->
            if List.contains (JString "null") ts then
                node
            else
                Map.add "type" (JArray(ts @ [ JString "null" ])) node
        | _ -> node

    let private applyNullable (out: Map<string, Json>) : Map<string, Json> =
        match Map.tryFind "enum" out with
        | Some(JArray es) ->
            let es2 = if List.contains JNull es then es else es @ [ JNull ]
            widenType (Map.add "enum" (JArray es2) out)
        | _ ->
            match Map.tryFind "type" out with
            | Some _ -> widenType out
            | None ->
                match Map.tryFind "const" out with
                | Some JNull -> out
                | _ -> Map.ofList [ "anyOf", JArray [ JObject out; JObject(Map.ofList [ "type", JString "null" ]) ] ]

    let private adjustExclusivity (out: Map<string, Json>) : Map<string, Json> =
        let out =
            match Map.tryFind "exclusiveMinimum" out with
            | Some(JBool b) ->
                match b, Map.tryFind "minimum" out with
                | true, Some(JNumber mn) -> out |> Map.add "exclusiveMinimum" (JNumber mn) |> Map.remove "minimum"
                | _ -> Map.remove "exclusiveMinimum" out
            | _ -> out

        match Map.tryFind "exclusiveMaximum" out with
        | Some(JBool b) ->
            match b, Map.tryFind "maximum" out with
            | true, Some(JNumber mx) -> out |> Map.add "exclusiveMaximum" (JNumber mx) |> Map.remove "maximum"
            | _ -> Map.remove "exclusiveMaximum" out
        | _ -> out

    let rec private normalizeOpenApi30 (node: Json) : Json =
        match node with
        | JArray xs -> JArray(xs |> List.map normalizeOpenApi30)
        | JObject src ->
            let mutable out: Map<string, Json> = Map.empty

            for KeyValue(k, v) in src do
                match k, v with
                | "$ref", JString s ->
                    out <- Map.add "$ref" (JString(rewritePrefix "#/components/schemas" "#/definitions" s)) out
                | "example", _ ->
                    // Singular OpenAPI `example` becomes a Draft `examples` array,
                    // unless `examples` is already present; the `example` key is dropped.
                    if not (Map.containsKey "examples" src) then
                        out <- Map.add "examples" (JArray [ v ]) out
                | _, (JArray _ | JObject _) -> out <- Map.add k (normalizeOpenApi30 v) out
                | _ -> out <- Map.add k v out

            let out = adjustExclusivity out

            let out =
                if Map.tryFind "nullable" out = Some(JBool true) then
                    applyNullable out
                else
                    out

            JObject(Map.remove "nullable" out)
        | other -> other

    /// Parses a raw OpenAPI 3.0 JSON Schema into a `Document`, expanding 3.0-only
    /// extensions (`nullable`, singular `example`, boolean `exclusiveMinimum` /
    /// `exclusiveMaximum`) before delegating to `fromSchemaDraft07`.
    /// (JsonSchema.fromSchemaOpenApi3_0)
    let fromSchemaOpenApi3_0 (schema: Json) : Document =
        fromSchemaDraft07 (normalizeOpenApi30 schema)

    // --- encoding ---

    let private toSchemaDraft07 (schema: Json) : Json =
        let rewritten = rewriteRefs (rewritePrefix "#/$defs" "#/definitions") schema

        let rec walk (node: Json) : Json =
            match node with
            | JArray xs -> JArray(xs |> List.map walk)
            | JObject src ->
                let mutable out: Map<string, Json> = Map.empty
                let mutable prefixItems: Json option = None
                let mutable items: Json option = None

                for KeyValue(k, v) in src do
                    match k with
                    | "properties"
                    | "patternProperties" ->
                        let mapped =
                            match v with
                            | JObject m -> Some(m |> Map.map (fun _ x -> walk x))
                            | _ -> None

                        out <-
                            Map.add
                                k
                                (match mapped with
                                 | Some d -> JObject d
                                 | None -> v)
                                out
                    | "additionalProperties"
                    | "propertyNames" -> out <- Map.add k (walk v) out
                    | "allOf"
                    | "anyOf"
                    | "oneOf" ->
                        let nv =
                            match v with
                            | JArray xs -> JArray(xs |> List.map walk)
                            | _ -> v

                        out <- Map.add k nv out
                    | "prefixItems" -> prefixItems <- Some v
                    | "items" -> items <- Some v
                    | _ when Set.contains k to07Keep -> out <- Map.add k v out
                    | _ -> ()

                // 2020-12 tuples -> Draft-07 tuples
                match prefixItems with
                | Some(JArray xs) ->
                    out <- Map.add "items" (JArray(xs |> List.map walk)) out

                    match items with
                    | Some it -> out <- Map.add "additionalItems" (walk it) out
                    | None -> ()
                | Some pi -> out <- Map.add "items" (walk pi) out
                | None ->
                    match items with
                    | Some it -> out <- Map.add "items" (walk it) out
                    | None -> ()

                JObject out
            | other -> other

        walk rewritten

    /// Converts a canonical `Document` to a Draft-07 `Document`: rewrites `#/$defs`
    /// refs to `#/definitions`, lowers `prefixItems`/`items` to Draft-07 tuple
    /// syntax, and drops unsupported keywords. (JsonSchema.toDocumentDraft07)
    let toDocumentDraft07 (document: Document) : Document =
        { Dialect = Draft07
          Schema = toSchemaDraft07 document.Schema
          Definitions = document.Definitions |> Map.map (fun _ d -> toSchemaDraft07 d) }

    /// Converts a canonical `MultiDocument` to an OpenAPI 3.1 `MultiDocument`:
    /// rewrites `#/$defs` refs to `#/components/schemas`, sanitizes definition
    /// keys, and updates `$ref` pointers to the sanitized keys.
    /// (JsonSchema.toMultiDocumentOpenApi3_1)
    let toMultiDocumentOpenApi3_1 (multiDocument: MultiDocument) : MultiDocument =
        let keyMap =
            multiDocument.Definitions
            |> Map.toList
            |> List.choose (fun (key, _) ->
                let sanitized = sanitizeOpenApiComponentsSchemasKey key
                if sanitized <> key then Some(key, sanitized) else None)
            |> Map.ofList

        let rewrite (schema: Json) : Json =
            rewriteRefs
                (fun ref ->
                    let tokens = ref.Split('/')

                    let ref2 =
                        if tokens.Length > 0 then
                            let identifier = JsonPointer.unescapeToken (Array.last tokens)

                            match Map.tryFind identifier keyMap with
                            | Some sanitized -> (String.concat "/" tokens.[0 .. tokens.Length - 2]) + "/" + sanitized
                            | None -> ref
                        else
                            ref

                    rewritePrefix "#/$defs" "#/components/schemas" ref2)
                schema

        { Dialect = OpenApi31
          Schemas = multiDocument.Schemas |> List.map rewrite
          Definitions =
            multiDocument.Definitions
            |> Map.toList
            |> List.map (fun (key, def) -> (defaultArg (Map.tryFind key keyMap) key, rewrite def))
            |> Map.ofList }

    // --- ref resolution ---

    /// Resolves a `$ref` by looking up its last path segment in `definitions`;
    /// `None` if absent. Does not follow arbitrary JSON Pointer paths.
    /// (JsonSchema.resolve$ref)
    let resolveRef (ref: string) (definitions: Definitions) : Json option =
        let tokens = ref.Split('/')

        if tokens.Length > 0 then
            Map.tryFind (JsonPointer.unescapeToken (Array.last tokens)) definitions
        else
            None

    /// Resolves a document whose root schema is a top-level `$ref`, returning the
    /// document unchanged when there is nothing to resolve.
    /// (JsonSchema.resolveTopLevel$ref)
    let resolveTopLevelRef (document: Document) : Document =
        match document.Schema with
        | JObject m ->
            match Map.tryFind "$ref" m with
            | Some(JString r) ->
                match resolveRef r document.Definitions with
                | Some schema -> { document with Schema = schema }
                | None -> document
            | _ -> document
        | _ -> document
