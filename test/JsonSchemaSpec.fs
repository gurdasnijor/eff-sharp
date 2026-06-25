module JsonSchemaSpec

open Effect
open Effect.Vitest

let private js (s: string) = JString s
let private jn (n: float) = JNumber n
let private jb (b: bool) = JBool b
let private ja (xs: Json list) = JArray xs
let private jo (pairs: (string * Json) list) = JObject(Map.ofList pairs)

let private doc dialect schema definitions =
    { Dialect = dialect
      Schema = schema
      Definitions = Map.ofList definitions }

let private expectStructuralEqual actual expected =
    toBe (actual = expected) true

describe "JsonSchema" (fun () ->
    test "sanitize returns underscore for empty input" (fun () ->
        toBe (JsonSchema.sanitizeOpenApiComponentsSchemasKey "") "_")

    test "sanitize returns input when already valid" (fun () ->
        for valid in [ "Simple"; "with-dash"; "with_underscore"; "with.dot"; "A1.B2-_" ] do
            toBe (JsonSchema.sanitizeOpenApiComponentsSchemasKey valid) valid)

    test "sanitize replaces invalid characters with underscore" (fun () ->
        for input, expected in
            [ "a b", "a_b"
              "a/b", "a_b"
              "a:b", "a_b"
              "a@b", "a_b"
              "a#b", "a_b"
              "a?b", "a_b"
              "a+b", "a_b"
              "a*b", "a_b"
              "a,b", "a_b"
              "a;b", "a_b"
              "a|b", "a_b"
              "a=b", "a_b" ] do
            toBe (JsonSchema.sanitizeOpenApiComponentsSchemasKey input) expected)

    test "sanitize preserves length for BMP input and replaces non-ascii" (fun () ->
        toBe (JsonSchema.sanitizeOpenApiComponentsSchemasKey "a b").Length 3
        toBe (JsonSchema.sanitizeOpenApiComponentsSchemasKey "café") "caf_"
        toBe (JsonSchema.sanitizeOpenApiComponentsSchemasKey "你好") "__")

    test "sanitize is idempotent" (fun () ->
        for input in [ ""; "Simple"; "a b"; "a/b"; "a..b"; "café"; "A1.B2-_" ] do
            let once = JsonSchema.sanitizeOpenApiComponentsSchemasKey input
            toBe (JsonSchema.sanitizeOpenApiComponentsSchemasKey once) once)

    test "draft07 normalizes a schema without definitions" (fun () ->
        let result = JsonSchema.fromSchemaDraft07 (jo [ "type", js "string" ])
        expectStructuralEqual result (doc Draft2020_12 (jo [ "type", js "string" ]) []))

    test "draft07 extracts root definitions and rewrites refs" (fun () ->
        let input =
            jo
                [ "type", js "object"
                  "properties",
                  jo
                      [ "a", jo [ "$ref", js "#/definitions/A" ]
                        "b", jo [ "$ref", js "#/definitions/B" ] ]
                  "definitions",
                  jo
                      [ "A", jo [ "type", js "string"; "$ref", js "#/definitions/B" ]
                        "B", jo [ "type", js "number" ] ] ]

        let expected =
            doc
                Draft2020_12
                (jo
                    [ "type", js "object"
                      "properties", jo [ "a", jo [ "$ref", js "#/$defs/A" ]; "b", jo [ "$ref", js "#/$defs/B" ] ] ])
                [ "A", jo [ "type", js "string"; "$ref", js "#/$defs/B" ]
                  "B", jo [ "type", js "number" ] ]

        expectStructuralEqual (JsonSchema.fromSchemaDraft07 input) expected)

    test "draft07 converts tuple items to prefixItems" (fun () ->
        let input =
            jo
                [ "type", js "array"
                  "items", ja [ jo [ "type", js "string" ]; jo [ "type", js "number" ] ]
                  "additionalItems", jo [ "type", js "boolean" ] ]

        let expected =
            doc
                Draft2020_12
                (jo
                    [ "type", js "array"
                      "prefixItems", ja [ jo [ "type", js "string" ]; jo [ "type", js "number" ] ]
                      "items", jo [ "type", js "boolean" ] ])
                []

        expectStructuralEqual (JsonSchema.fromSchemaDraft07 input) expected)

    test "draft07 preserves a single items schema" (fun () ->
        let result =
            JsonSchema.fromSchemaDraft07 (jo [ "type", js "array"; "items", jo [ "type", js "string" ] ])

        expectStructuralEqual result (doc Draft2020_12 (jo [ "type", js "array"; "items", jo [ "type", js "string" ] ]) []))

    test "draft07 preserves annotations and constraints" (fun () ->
        let input =
            jo
                [ "type", js "string"
                  "title", js "My String"
                  "description", js "A string value"
                  "default", js "default"
                  "examples", ja [ js "example1"; js "example2" ]
                  "format", js "email"
                  "readOnly", jb true
                  "writeOnly", jb true ]

        expectStructuralEqual (JsonSchema.fromSchemaDraft07 input) (doc Draft2020_12 input []))

    test "draft07 handles enum const allOf anyOf oneOf with tuple rewriting" (fun () ->
        let input =
            jo
                [ "enum", ja [ js "a"; js "b"; js "c" ]
                  "const", js "constant"
                  "allOf", ja [ jo [ "type", js "array"; "items", jo [ "type", js "string" ] ]; jo [ "minItems", jn 1.0 ] ]
                  "anyOf", ja [ jo [ "type", js "array"; "items", ja [ jo [ "type", js "string" ] ] ]; jo [ "type", js "number" ] ]
                  "oneOf",
                  ja
                      [ jo
                            [ "type", js "array"
                              "items", ja [ jo [ "type", js "string" ] ]
                              "additionalItems", jo [ "type", js "number" ] ]
                        jo [ "type", js "boolean" ] ] ]

        let expected =
            doc
                Draft2020_12
                (jo
                    [ "enum", ja [ js "a"; js "b"; js "c" ]
                      "const", js "constant"
                      "allOf", ja [ jo [ "type", js "array"; "items", jo [ "type", js "string" ] ]; jo [ "minItems", jn 1.0 ] ]
                      "anyOf",
                      ja [ jo [ "type", js "array"; "prefixItems", ja [ jo [ "type", js "string" ] ] ]; jo [ "type", js "number" ] ]
                      "oneOf",
                      ja
                          [ jo
                                [ "type", js "array"
                                  "prefixItems", ja [ jo [ "type", js "string" ] ]
                                  "items", jo [ "type", js "number" ] ]
                            jo [ "type", js "boolean" ] ] ])
                []

        expectStructuralEqual (JsonSchema.fromSchemaDraft07 input) expected)

    test "draft07 preserves nested definitions and local refs" (fun () ->
        let input =
            jo
                [ "type", js "object"
                  "properties",
                  jo
                      [ "nested",
                        jo
                            [ "definitions", jo [ "NestedType", jo [ "type", js "number" ] ]
                              "$ref", js "#/properties/nested/definitions/NestedType" ] ] ]

        expectStructuralEqual (JsonSchema.fromSchemaDraft07 input) (doc Draft2020_12 input []))

    test "draft07 drops non-standard properties" (fun () ->
        let result =
            JsonSchema.fromSchemaDraft07 (jo [ "type", js "string"; "x-custom", js "value" ])

        expectStructuralEqual result (doc Draft2020_12 (jo [ "type", js "string" ]) []))

    test "draft2020 extracts root defs without rewriting refs" (fun () ->
        let input =
            jo
                [ "type", js "object"
                  "properties", jo [ "a", jo [ "$ref", js "#/$defs/A" ] ]
                  "$defs", jo [ "A", jo [ "type", js "string" ] ] ]

        let expected =
            doc
                Draft2020_12
                (jo [ "type", js "object"; "properties", jo [ "a", jo [ "$ref", js "#/$defs/A" ] ] ])
                [ "A", jo [ "type", js "string" ] ]

        expectStructuralEqual (JsonSchema.fromSchemaDraft2020_12 input) expected)

    test "draft2020 keeps non-standard properties" (fun () ->
        let input = jo [ "type", js "string"; "x-custom", js "value" ]
        expectStructuralEqual (JsonSchema.fromSchemaDraft2020_12 input) (doc Draft2020_12 input []))

    test "openapi31 rewrites component refs to defs refs" (fun () ->
        let input =
            jo
                [ "type", js "object"
                  "properties", jo [ "a", jo [ "$ref", js "#/components/schemas/A" ] ] ]

        let expected =
            doc Draft2020_12 (jo [ "type", js "object"; "properties", jo [ "a", jo [ "$ref", js "#/$defs/A" ] ] ]) []

        expectStructuralEqual (JsonSchema.fromSchemaOpenApi3_1 input) expected)

    test "openapi31 keeps non-standard properties" (fun () ->
        let input = jo [ "type", js "string"; "x-custom", js "value" ]
        expectStructuralEqual (JsonSchema.fromSchemaOpenApi3_1 input) (doc Draft2020_12 input []))

    test "openapi30 normalizes singular example to examples array" (fun () ->
        let result =
            JsonSchema.fromSchemaOpenApi3_0 (jo [ "type", js "string"; "example", js "a" ])

        expectStructuralEqual result (doc Draft2020_12 (jo [ "type", js "string"; "examples", ja [ js "a" ] ]) []))

    test "openapi30 nullable expands and widens types" (fun () ->
        expectStructuralEqual
            (JsonSchema.fromSchemaOpenApi3_0 (jo [ "nullable", jb true ]))
            (doc Draft2020_12 (jo [ "anyOf", ja [ jo []; jo [ "type", js "null" ] ] ]) [])

        expectStructuralEqual
            (JsonSchema.fromSchemaOpenApi3_0 (jo [ "type", js "string"; "nullable", jb true ]))
            (doc Draft2020_12 (jo [ "type", ja [ js "string"; js "null" ] ]) [])

        expectStructuralEqual
            (JsonSchema.fromSchemaOpenApi3_0 (jo [ "type", ja [ js "string"; js "number" ]; "nullable", jb true ]))
            (doc Draft2020_12 (jo [ "type", ja [ js "string"; js "number"; js "null" ] ]) []))

    test "openapi30 nullable handles const and enum" (fun () ->
        expectStructuralEqual
            (JsonSchema.fromSchemaOpenApi3_0 (jo [ "type", js "string"; "const", js "a"; "nullable", jb true ]))
            (doc Draft2020_12 (jo [ "type", ja [ js "string"; js "null" ]; "const", js "a" ]) [])

        expectStructuralEqual
            (JsonSchema.fromSchemaOpenApi3_0 (jo [ "const", js "a"; "nullable", jb true ]))
            (doc Draft2020_12 (jo [ "anyOf", ja [ jo [ "const", js "a" ]; jo [ "type", js "null" ] ] ]) [])

        expectStructuralEqual
            (JsonSchema.fromSchemaOpenApi3_0 (jo [ "const", JNull; "nullable", jb true ]))
            (doc Draft2020_12 (jo [ "const", JNull ]) [])

        expectStructuralEqual
            (JsonSchema.fromSchemaOpenApi3_0 (jo [ "type", js "string"; "enum", ja [ js "a"; js "b" ]; "nullable", jb true ]))
            (doc Draft2020_12 (jo [ "type", ja [ js "string"; js "null" ]; "enum", ja [ js "a"; js "b"; JNull ] ]) []))

    test "openapi30 nullable false is dropped" (fun () ->
        expectStructuralEqual
            (JsonSchema.fromSchemaOpenApi3_0 (jo [ "type", js "string"; "nullable", jb false ]))
            (doc Draft2020_12 (jo [ "type", js "string" ]) []))

    test "openapi30 nullable inside allOf is independent" (fun () ->
        expectStructuralEqual
            (JsonSchema.fromSchemaOpenApi3_0 (jo [ "type", js "string"; "allOf", ja [ jo [ "nullable", jb true ] ] ]))
            (doc
                Draft2020_12
                (jo
                    [ "type", js "string"
                      "allOf", ja [ jo [ "anyOf", ja [ jo []; jo [ "type", js "null" ] ] ] ] ])
                []))

    test "openapi30 boolean exclusivity is normalized" (fun () ->
        expectStructuralEqual
            (JsonSchema.fromSchemaOpenApi3_0 (jo [ "type", js "number"; "minimum", jn 10.0; "exclusiveMinimum", jb true ]))
            (doc Draft2020_12 (jo [ "type", js "number"; "exclusiveMinimum", jn 10.0 ]) [])

        expectStructuralEqual
            (JsonSchema.fromSchemaOpenApi3_0 (jo [ "type", js "number"; "maximum", jn 100.0; "exclusiveMaximum", jb true ]))
            (doc Draft2020_12 (jo [ "type", js "number"; "exclusiveMaximum", jn 100.0 ]) [])

        expectStructuralEqual
            (JsonSchema.fromSchemaOpenApi3_0 (jo [ "type", js "number"; "minimum", jn 10.0; "exclusiveMinimum", jb false ]))
            (doc Draft2020_12 (jo [ "type", js "number"; "minimum", jn 10.0 ]) [])

        expectStructuralEqual
            (JsonSchema.fromSchemaOpenApi3_0 (jo [ "type", js "number"; "exclusiveMinimum", jb true ]))
            (doc Draft2020_12 (jo [ "type", js "number" ]) []))

    test "openapi30 rewrites component refs" (fun () ->
        let input =
            jo
                [ "type", js "object"
                  "properties", jo [ "a", jo [ "$ref", js "#/components/schemas/A" ] ] ]

        let expected =
            doc Draft2020_12 (jo [ "type", js "object"; "properties", jo [ "a", jo [ "$ref", js "#/$defs/A" ] ] ]) []

        expectStructuralEqual (JsonSchema.fromSchemaOpenApi3_0 input) expected)

    test "toDraft07 rewrites refs and lowers tuples" (fun () ->
        let input =
            doc
                Draft2020_12
                (jo [ "type", js "object"; "properties", jo [ "a", jo [ "$ref", js "#/$defs/A" ] ] ])
                [ "A", jo [ "type", js "string"; "$ref", js "#/$defs/B" ]
                  "B", jo [ "type", js "number" ] ]

        let expected =
            doc
                Draft07
                (jo [ "type", js "object"; "properties", jo [ "a", jo [ "$ref", js "#/definitions/A" ] ] ])
                [ "A", jo [ "type", js "string"; "$ref", js "#/definitions/B" ]
                  "B", jo [ "type", js "number" ] ]

        expectStructuralEqual (JsonSchema.toDocumentDraft07 input) expected)

    test "toDraft07 converts prefixItems to tuple" (fun () ->
        let input =
            doc
                Draft2020_12
                (jo
                    [ "type", js "array"
                      "prefixItems", ja [ jo [ "type", js "string" ]; jo [ "type", js "number" ] ]
                      "items", jo [ "type", js "boolean" ] ])
                []

        let expected =
            doc
                Draft07
                (jo
                    [ "type", js "array"
                      "items", ja [ jo [ "type", js "string" ]; jo [ "type", js "number" ] ]
                      "additionalItems", jo [ "type", js "boolean" ] ])
                []

        expectStructuralEqual (JsonSchema.toDocumentDraft07 input) expected)

    test "toDraft07 drops non-standard properties" (fun () ->
        let input = doc Draft2020_12 (jo [ "type", js "string"; "x-custom", js "value" ]) []
        expectStructuralEqual (JsonSchema.toDocumentDraft07 input) (doc Draft07 (jo [ "type", js "string" ]) []))

    test "toOpenApi31 rewrites defs refs to components" (fun () ->
        let input =
            { Dialect = Draft2020_12
              Schemas = [ jo [ "type", js "object"; "properties", jo [ "a", jo [ "$ref", js "#/$defs/A" ] ] ] ]
              Definitions = Map.ofList [ "A", jo [ "type", js "string"; "$ref", js "#/$defs/B" ]; "B", jo [ "type", js "number" ] ] }

        let expected =
            { Dialect = OpenApi31
              Schemas = [ jo [ "type", js "object"; "properties", jo [ "a", jo [ "$ref", js "#/components/schemas/A" ] ] ] ]
              Definitions =
                Map.ofList
                    [ "A", jo [ "type", js "string"; "$ref", js "#/components/schemas/B" ]
                      "B", jo [ "type", js "number" ] ] }

        expectStructuralEqual (JsonSchema.toMultiDocumentOpenApi3_1 input) expected)

    test "toOpenApi31 sanitizes component keys and rewritten refs together" (fun () ->
        let input =
            { Dialect = Draft2020_12
              Schemas = [ jo [ "type", js "object"; "properties", jo [ "A.B", jo [ "$ref", js "#/$defs/A$B" ] ] ] ]
              Definitions = Map.ofList [ "A$B", jo [ "$ref", js "#/$defs/B$C" ]; "B$C", jo [ "type", js "string" ] ] }

        let expected =
            { Dialect = OpenApi31
              Schemas = [ jo [ "type", js "object"; "properties", jo [ "A.B", jo [ "$ref", js "#/components/schemas/A_B" ] ] ] ]
              Definitions =
                Map.ofList
                    [ "A_B", jo [ "$ref", js "#/components/schemas/B_C" ]
                      "B_C", jo [ "type", js "string" ] ] }

        expectStructuralEqual (JsonSchema.toMultiDocumentOpenApi3_1 input) expected)

    test "resolveRef looks up the last segment" (fun () ->
        let definitions = Map.ofList [ "User", jo [ "type", js "object" ] ]
        expectStructuralEqual (JsonSchema.resolveRef "#/$defs/User" definitions) (Some(jo [ "type", js "object" ]))
        expectStructuralEqual (JsonSchema.resolveRef "#/$defs/Unknown" definitions) None)

    test "resolveTopLevelRef dereferences a root ref" (fun () ->
        let document =
            doc Draft2020_12 (jo [ "$ref", js "#/$defs/User" ]) [ "User", jo [ "type", js "object" ] ]

        let resolved = JsonSchema.resolveTopLevelRef document
        expectStructuralEqual resolved.Schema (jo [ "type", js "object" ])

        let missing = doc Draft2020_12 (jo [ "$ref", js "#/$defs/Nope" ]) []
        expectStructuralEqual (JsonSchema.resolveTopLevelRef missing) missing)

    test "roundtrip Draft07 through canonical form" (fun () ->
        let original =
            jo
                [ "type", js "object"
                  "properties",
                  jo
                      [ "name", jo [ "type", js "string" ]
                        "items",
                        jo
                            [ "type", js "array"
                              "items", ja [ jo [ "type", js "string" ]; jo [ "type", js "number" ] ]
                              "additionalItems", jo [ "type", js "boolean" ] ]
                        "ref", jo [ "$ref", js "#/definitions/MyType" ] ]
                  "definitions", jo [ "MyType", jo [ "type", js "string" ] ] ]

        let backTo07 = JsonSchema.toDocumentDraft07 (JsonSchema.fromSchemaDraft07 original)

        let expectedSchema =
            jo
                [ "type", js "object"
                  "properties",
                  jo
                      [ "name", jo [ "type", js "string" ]
                        "items",
                        jo
                            [ "type", js "array"
                              "items", ja [ jo [ "type", js "string" ]; jo [ "type", js "number" ] ]
                              "additionalItems", jo [ "type", js "boolean" ] ]
                        "ref", jo [ "$ref", js "#/definitions/MyType" ] ] ]

        expectStructuralEqual backTo07.Schema expectedSchema
        expectStructuralEqual backTo07.Definitions (Map.ofList [ "MyType", jo [ "type", js "string" ] ])))
