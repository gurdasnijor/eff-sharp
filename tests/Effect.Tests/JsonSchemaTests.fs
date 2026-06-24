module Effect.Tests.JsonSchemaTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/JsonSchema.test.ts

// --- Json construction helpers ---
let private js (s: string) = JString s
let private jn (n: float) = JNumber n
let private jb (b: bool) = JBool b
let private ja (xs: Json list) = JArray xs
let private jo (pairs: (string * Json) list) = JObject(Map.ofList pairs)

let private doc dialect schema definitions =
    { Dialect = dialect; Schema = schema; Definitions = Map.ofList definitions }

// --- sanitizeOpenApiComponentsSchemasKey ---

[<Fact>]
let ``sanitize returns underscore for empty input`` () =
    Assert.Equal("_", JsonSchema.sanitizeOpenApiComponentsSchemasKey "")

[<Fact>]
let ``sanitize returns input when already valid`` () =
    for valid in [ "Simple"; "with-dash"; "with_underscore"; "with.dot"; "A1.B2-_" ] do
        Assert.Equal(valid, JsonSchema.sanitizeOpenApiComponentsSchemasKey valid)

[<Fact>]
let ``sanitize replaces invalid characters with underscore`` () =
    for input, expected in
        [ "a b", "a_b"; "a/b", "a_b"; "a:b", "a_b"; "a@b", "a_b"; "a#b", "a_b"; "a?b", "a_b"; "a+b", "a_b"
          "a*b", "a_b"; "a,b", "a_b"; "a;b", "a_b"; "a|b", "a_b"; "a=b", "a_b" ] do
        Assert.Equal(expected, JsonSchema.sanitizeOpenApiComponentsSchemasKey input)

[<Fact>]
let ``sanitize preserves length and handles non-ascii by code point`` () =
    Assert.Equal(3, (JsonSchema.sanitizeOpenApiComponentsSchemasKey "a b").Length)
    Assert.Equal("caf_", JsonSchema.sanitizeOpenApiComponentsSchemasKey "café")
    Assert.Equal("__", JsonSchema.sanitizeOpenApiComponentsSchemasKey "你好")
    Assert.Equal("_", JsonSchema.sanitizeOpenApiComponentsSchemasKey "🤖")
    Assert.Equal("a_b", JsonSchema.sanitizeOpenApiComponentsSchemasKey "a🤖b")

[<Fact>]
let ``sanitize is idempotent`` () =
    for input in [ ""; "Simple"; "a b"; "a/b"; "a..b"; "café"; "🤖"; "A1.B2-_" ] do
        let once = JsonSchema.sanitizeOpenApiComponentsSchemasKey input
        Assert.Equal(once, JsonSchema.sanitizeOpenApiComponentsSchemasKey once)

// --- fromSchemaDraft07 ---

[<Fact>]
let ``draft07 normalizes a schema without definitions`` () =
    let result = JsonSchema.fromSchemaDraft07 (jo [ "type", js "string" ])
    Assert.Equal(doc Draft2020_12 (jo [ "type", js "string" ]) [], result)

[<Fact>]
let ``draft07 extracts root definitions and rewrites refs`` () =
    let input =
        jo
            [ "type", js "object"
              "properties", jo [ "a", jo [ "$ref", js "#/definitions/A" ]; "b", jo [ "$ref", js "#/definitions/B" ] ]
              "definitions",
              jo [ "A", jo [ "type", js "string"; "$ref", js "#/definitions/B" ]; "B", jo [ "type", js "number" ] ] ]
    let result = JsonSchema.fromSchemaDraft07 input
    let expected =
        doc
            Draft2020_12
            (jo [ "type", js "object"
                  "properties", jo [ "a", jo [ "$ref", js "#/$defs/A" ]; "b", jo [ "$ref", js "#/$defs/B" ] ] ])
            [ "A", jo [ "type", js "string"; "$ref", js "#/$defs/B" ]; "B", jo [ "type", js "number" ] ]
    Assert.Equal(expected, result)

[<Fact>]
let ``draft07 converts tuple items to prefixItems`` () =
    let input =
        jo [ "type", js "array"; "items", ja [ jo [ "type", js "string" ]; jo [ "type", js "number" ] ]; "additionalItems", jo [ "type", js "boolean" ] ]
    let result = JsonSchema.fromSchemaDraft07 input
    let expected =
        doc Draft2020_12 (jo [ "type", js "array"; "prefixItems", ja [ jo [ "type", js "string" ]; jo [ "type", js "number" ] ]; "items", jo [ "type", js "boolean" ] ]) []
    Assert.Equal(expected, result)

[<Fact>]
let ``draft07 preserves a single items schema`` () =
    let result = JsonSchema.fromSchemaDraft07 (jo [ "type", js "array"; "items", jo [ "type", js "string" ] ])
    Assert.Equal(doc Draft2020_12 (jo [ "type", js "array"; "items", jo [ "type", js "string" ] ]) [], result)

[<Fact>]
let ``draft07 preserves annotations and constraints`` () =
    let input =
        jo [ "type", js "string"; "title", js "My String"; "description", js "A string value"; "default", js "default"
             "examples", ja [ js "example1"; js "example2" ]; "format", js "email"; "readOnly", jb true; "writeOnly", jb true ]
    Assert.Equal(doc Draft2020_12 input [], JsonSchema.fromSchemaDraft07 input)

[<Fact>]
let ``draft07 handles enum const allOf anyOf oneOf with tuple rewriting`` () =
    let input =
        jo [ "enum", ja [ js "a"; js "b"; js "c" ]
             "const", js "constant"
             "allOf", ja [ jo [ "type", js "array"; "items", jo [ "type", js "string" ] ]; jo [ "minItems", jn 1.0 ] ]
             "anyOf", ja [ jo [ "type", js "array"; "items", ja [ jo [ "type", js "string" ] ] ]; jo [ "type", js "number" ] ]
             "oneOf", ja [ jo [ "type", js "array"; "items", ja [ jo [ "type", js "string" ] ]; "additionalItems", jo [ "type", js "number" ] ]; jo [ "type", js "boolean" ] ] ]
    let expected =
        doc
            Draft2020_12
            (jo [ "enum", ja [ js "a"; js "b"; js "c" ]
                  "const", js "constant"
                  "allOf", ja [ jo [ "type", js "array"; "items", jo [ "type", js "string" ] ]; jo [ "minItems", jn 1.0 ] ]
                  "anyOf", ja [ jo [ "type", js "array"; "prefixItems", ja [ jo [ "type", js "string" ] ] ]; jo [ "type", js "number" ] ]
                  "oneOf", ja [ jo [ "type", js "array"; "prefixItems", ja [ jo [ "type", js "string" ] ]; "items", jo [ "type", js "number" ] ]; jo [ "type", js "boolean" ] ] ])
            []
    Assert.Equal(expected, JsonSchema.fromSchemaDraft07 input)

[<Fact>]
let ``draft07 preserves nested definitions and local refs`` () =
    let input =
        jo [ "type", js "object"
             "properties", jo [ "nested", jo [ "definitions", jo [ "NestedType", jo [ "type", js "number" ] ]; "$ref", js "#/properties/nested/definitions/NestedType" ] ] ]
    Assert.Equal(doc Draft2020_12 input [], JsonSchema.fromSchemaDraft07 input)

[<Fact>]
let ``draft07 drops non-standard properties`` () =
    let result = JsonSchema.fromSchemaDraft07 (jo [ "type", js "string"; "x-custom", js "value" ])
    Assert.Equal(doc Draft2020_12 (jo [ "type", js "string" ]) [], result)

// --- fromSchemaDraft2020_12 ---

[<Fact>]
let ``draft2020 extracts root defs without rewriting refs`` () =
    let input =
        jo [ "type", js "object"
             "properties", jo [ "a", jo [ "$ref", js "#/$defs/A" ] ]
             "$defs", jo [ "A", jo [ "type", js "string" ] ] ]
    let expected =
        doc Draft2020_12 (jo [ "type", js "object"; "properties", jo [ "a", jo [ "$ref", js "#/$defs/A" ] ] ]) [ "A", jo [ "type", js "string" ] ]
    Assert.Equal(expected, JsonSchema.fromSchemaDraft2020_12 input)

[<Fact>]
let ``draft2020 keeps non-standard properties`` () =
    let input = jo [ "type", js "string"; "x-custom", js "value" ]
    Assert.Equal(doc Draft2020_12 input [], JsonSchema.fromSchemaDraft2020_12 input)

// --- fromSchemaOpenApi3_1 ---

[<Fact>]
let ``openapi31 rewrites component refs to defs refs`` () =
    let input =
        jo [ "type", js "object"; "properties", jo [ "a", jo [ "$ref", js "#/components/schemas/A" ] ] ]
    let expected =
        doc Draft2020_12 (jo [ "type", js "object"; "properties", jo [ "a", jo [ "$ref", js "#/$defs/A" ] ] ]) []
    Assert.Equal(expected, JsonSchema.fromSchemaOpenApi3_1 input)

[<Fact>]
let ``openapi31 keeps non-standard properties`` () =
    let input = jo [ "type", js "string"; "x-custom", js "value" ]
    Assert.Equal(doc Draft2020_12 input [], JsonSchema.fromSchemaOpenApi3_1 input)

// --- fromSchemaOpenApi3_0 ---

[<Fact>]
let ``openapi30 normalizes singular example to examples array`` () =
    let result = JsonSchema.fromSchemaOpenApi3_0 (jo [ "type", js "string"; "example", js "a" ])
    Assert.Equal(doc Draft2020_12 (jo [ "type", js "string"; "examples", ja [ js "a" ] ]) [], result)

[<Fact>]
let ``openapi30 nullable expands and widens types`` () =
    // no other keywords -> anyOf
    Assert.Equal(
        doc Draft2020_12 (jo [ "anyOf", ja [ jo []; jo [ "type", js "null" ] ] ]) [],
        JsonSchema.fromSchemaOpenApi3_0 (jo [ "nullable", jb true ]))
    // string type -> [string, null]
    Assert.Equal(
        doc Draft2020_12 (jo [ "type", ja [ js "string"; js "null" ] ]) [],
        JsonSchema.fromSchemaOpenApi3_0 (jo [ "type", js "string"; "nullable", jb true ]))
    // type array -> append null
    Assert.Equal(
        doc Draft2020_12 (jo [ "type", ja [ js "string"; js "number"; js "null" ] ]) [],
        JsonSchema.fromSchemaOpenApi3_0 (jo [ "type", ja [ js "string"; js "number" ]; "nullable", jb true ]))

[<Fact>]
let ``openapi30 nullable handles const and enum`` () =
    // non-null const with type -> widen type, keep const
    Assert.Equal(
        doc Draft2020_12 (jo [ "type", ja [ js "string"; js "null" ]; "const", js "a" ]) [],
        JsonSchema.fromSchemaOpenApi3_0 (jo [ "type", js "string"; "const", js "a"; "nullable", jb true ]))
    // non-null const without type -> anyOf
    Assert.Equal(
        doc Draft2020_12 (jo [ "anyOf", ja [ jo [ "const", js "a" ]; jo [ "type", js "null" ] ] ]) [],
        JsonSchema.fromSchemaOpenApi3_0 (jo [ "const", js "a"; "nullable", jb true ]))
    // null const -> unchanged
    Assert.Equal(
        doc Draft2020_12 (jo [ "const", JNull ]) [],
        JsonSchema.fromSchemaOpenApi3_0 (jo [ "const", JNull; "nullable", jb true ]))
    // enum widening adds null once
    Assert.Equal(
        doc Draft2020_12 (jo [ "type", ja [ js "string"; js "null" ]; "enum", ja [ js "a"; js "b"; JNull ] ]) [],
        JsonSchema.fromSchemaOpenApi3_0 (jo [ "type", js "string"; "enum", ja [ js "a"; js "b" ]; "nullable", jb true ]))

[<Fact>]
let ``openapi30 nullable false is dropped`` () =
    Assert.Equal(
        doc Draft2020_12 (jo [ "type", js "string" ]) [],
        JsonSchema.fromSchemaOpenApi3_0 (jo [ "type", js "string"; "nullable", jb false ]))

[<Fact>]
let ``openapi30 nullable inside allOf is independent`` () =
    Assert.Equal(
        doc Draft2020_12 (jo [ "type", js "string"; "allOf", ja [ jo [ "anyOf", ja [ jo []; jo [ "type", js "null" ] ] ] ] ]) [],
        JsonSchema.fromSchemaOpenApi3_0 (jo [ "type", js "string"; "allOf", ja [ jo [ "nullable", jb true ] ] ]))

[<Fact>]
let ``openapi30 boolean exclusivity is normalized`` () =
    // exclusiveMinimum: true -> exclusiveMinimum: minimum
    Assert.Equal(
        doc Draft2020_12 (jo [ "type", js "number"; "exclusiveMinimum", jn 10.0 ]) [],
        JsonSchema.fromSchemaOpenApi3_0 (jo [ "type", js "number"; "minimum", jn 10.0; "exclusiveMinimum", jb true ]))
    // exclusiveMaximum: true -> exclusiveMaximum: maximum
    Assert.Equal(
        doc Draft2020_12 (jo [ "type", js "number"; "exclusiveMaximum", jn 100.0 ]) [],
        JsonSchema.fromSchemaOpenApi3_0 (jo [ "type", js "number"; "maximum", jn 100.0; "exclusiveMaximum", jb true ]))
    // exclusiveMinimum: false -> dropped, minimum kept
    Assert.Equal(
        doc Draft2020_12 (jo [ "type", js "number"; "minimum", jn 10.0 ]) [],
        JsonSchema.fromSchemaOpenApi3_0 (jo [ "type", js "number"; "minimum", jn 10.0; "exclusiveMinimum", jb false ]))
    // exclusiveMinimum: true but minimum absent -> dropped
    Assert.Equal(
        doc Draft2020_12 (jo [ "type", js "number" ]) [],
        JsonSchema.fromSchemaOpenApi3_0 (jo [ "type", js "number"; "exclusiveMinimum", jb true ]))

[<Fact>]
let ``openapi30 rewrites component refs`` () =
    let input = jo [ "type", js "object"; "properties", jo [ "a", jo [ "$ref", js "#/components/schemas/A" ] ] ]
    let expected = doc Draft2020_12 (jo [ "type", js "object"; "properties", jo [ "a", jo [ "$ref", js "#/$defs/A" ] ] ]) []
    Assert.Equal(expected, JsonSchema.fromSchemaOpenApi3_0 input)

// --- toDocumentDraft07 ---

[<Fact>]
let ``toDraft07 rewrites refs and lowers tuples`` () =
    let input =
        doc
            Draft2020_12
            (jo [ "type", js "object"; "properties", jo [ "a", jo [ "$ref", js "#/$defs/A" ] ] ])
            [ "A", jo [ "type", js "string"; "$ref", js "#/$defs/B" ]; "B", jo [ "type", js "number" ] ]
    let result = JsonSchema.toDocumentDraft07 input
    let expected =
        doc
            Draft07
            (jo [ "type", js "object"; "properties", jo [ "a", jo [ "$ref", js "#/definitions/A" ] ] ])
            [ "A", jo [ "type", js "string"; "$ref", js "#/definitions/B" ]; "B", jo [ "type", js "number" ] ]
    Assert.Equal(expected, result)

[<Fact>]
let ``toDraft07 converts prefixItems to tuple`` () =
    let input =
        doc Draft2020_12 (jo [ "type", js "array"; "prefixItems", ja [ jo [ "type", js "string" ]; jo [ "type", js "number" ] ]; "items", jo [ "type", js "boolean" ] ]) []
    let expected =
        doc Draft07 (jo [ "type", js "array"; "items", ja [ jo [ "type", js "string" ]; jo [ "type", js "number" ] ]; "additionalItems", jo [ "type", js "boolean" ] ]) []
    Assert.Equal(expected, JsonSchema.toDocumentDraft07 input)

[<Fact>]
let ``toDraft07 drops non-standard properties`` () =
    let input = doc Draft2020_12 (jo [ "type", js "string"; "x-custom", js "value" ]) []
    Assert.Equal(doc Draft07 (jo [ "type", js "string" ]) [], JsonSchema.toDocumentDraft07 input)

// --- toMultiDocumentOpenApi3_1 ---

[<Fact>]
let ``toOpenApi31 rewrites defs refs to components`` () =
    let input =
        { Dialect = Draft2020_12
          Schemas = [ jo [ "type", js "object"; "properties", jo [ "a", jo [ "$ref", js "#/$defs/A" ] ] ] ]
          Definitions = Map.ofList [ "A", jo [ "type", js "string"; "$ref", js "#/$defs/B" ]; "B", jo [ "type", js "number" ] ] }
    let result = JsonSchema.toMultiDocumentOpenApi3_1 input
    let expected =
        { Dialect = OpenApi31
          Schemas = [ jo [ "type", js "object"; "properties", jo [ "a", jo [ "$ref", js "#/components/schemas/A" ] ] ] ]
          Definitions = Map.ofList [ "A", jo [ "type", js "string"; "$ref", js "#/components/schemas/B" ]; "B", jo [ "type", js "number" ] ] }
    Assert.Equal(expected, result)

[<Fact>]
let ``toOpenApi31 sanitizes component keys and rewritten refs together`` () =
    let input =
        { Dialect = Draft2020_12
          Schemas = [ jo [ "type", js "object"; "properties", jo [ "A.B", jo [ "$ref", js "#/$defs/A$B" ] ] ] ]
          Definitions = Map.ofList [ "A$B", jo [ "$ref", js "#/$defs/B$C" ]; "B$C", jo [ "type", js "string" ] ] }
    let result = JsonSchema.toMultiDocumentOpenApi3_1 input
    let expected =
        { Dialect = OpenApi31
          Schemas = [ jo [ "type", js "object"; "properties", jo [ "A.B", jo [ "$ref", js "#/components/schemas/A_B" ] ] ] ]
          Definitions = Map.ofList [ "A_B", jo [ "$ref", js "#/components/schemas/B_C" ]; "B_C", jo [ "type", js "string" ] ] }
    Assert.Equal(expected, result)

// --- ref resolution ---

[<Fact>]
let ``resolveRef looks up the last segment`` () =
    let definitions = Map.ofList [ "User", jo [ "type", js "object" ] ]
    Assert.Equal(Some(jo [ "type", js "object" ]), JsonSchema.resolveRef "#/$defs/User" definitions)
    Assert.Equal(None, JsonSchema.resolveRef "#/$defs/Unknown" definitions)

[<Fact>]
let ``resolveTopLevelRef dereferences a root ref`` () =
    let document =
        doc Draft2020_12 (jo [ "$ref", js "#/$defs/User" ]) [ "User", jo [ "type", js "object" ] ]
    let resolved = JsonSchema.resolveTopLevelRef document
    Assert.Equal(jo [ "type", js "object" ], resolved.Schema)
    // unchanged when ref cannot be resolved
    let missing = doc Draft2020_12 (jo [ "$ref", js "#/$defs/Nope" ]) []
    Assert.Equal(missing, JsonSchema.resolveTopLevelRef missing)

// --- roundtrip ---

[<Fact>]
let ``roundtrip Draft07 through canonical form`` () =
    let original =
        jo [ "type", js "object"
             "properties",
             jo [ "name", jo [ "type", js "string" ]
                  "items", jo [ "type", js "array"; "items", ja [ jo [ "type", js "string" ]; jo [ "type", js "number" ] ]; "additionalItems", jo [ "type", js "boolean" ] ]
                  "ref", jo [ "$ref", js "#/definitions/MyType" ] ]
             "definitions", jo [ "MyType", jo [ "type", js "string" ] ] ]
    let backTo07 = JsonSchema.toDocumentDraft07 (JsonSchema.fromSchemaDraft07 original)
    let expectedSchema =
        jo [ "type", js "object"
             "properties",
             jo [ "name", jo [ "type", js "string" ]
                  "items", jo [ "type", js "array"; "items", ja [ jo [ "type", js "string" ]; jo [ "type", js "number" ] ]; "additionalItems", jo [ "type", js "boolean" ] ]
                  "ref", jo [ "$ref", js "#/definitions/MyType" ] ] ]
    Assert.Equal(expectedSchema, backTo07.Schema)
    Assert.Equal<Map<string, Json>>(Map.ofList [ "MyType", jo [ "type", js "string" ] ], backTo07.Definitions)
