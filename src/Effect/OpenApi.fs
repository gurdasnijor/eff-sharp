namespace Effect

[<RequireQualifiedAccess>]
module OpenApi =

    let private jo pairs = JObject(Map.ofList pairs)
    let private ja items = JArray items
    let private js value = JString value
    let private jn value = JNumber(float value)

    let rec private schemaAstToJson =
        function
        | AString -> jo [ "type", js "string" ]
        | AInt -> jo [ "type", js "integer" ]
        | AFloat
        | ADecimal -> jo [ "type", js "number" ]
        | ABool -> jo [ "type", js "boolean" ]
        | ALiteral value -> jo [ "const", value ]
        | AArray item -> jo [ "type", js "array"; "items", schemaAstToJson item ]
        | ATuple items ->
            jo [ "type", js "array"; "prefixItems", ja (items |> List.map schemaAstToJson) ]
        | AObject fields ->
            jo
                [ "type", js "object"
                  "properties", jo (fields |> List.map (fun (name, ast) -> name, schemaAstToJson ast))
                  "required", ja (fields |> List.map (fst >> js)) ]
        | AOption ast ->
            jo [ "anyOf", ja [ schemaAstToJson ast; jo [ "type", js "null" ] ] ]
        | AUnion members ->
            jo [ "anyOf", ja (members |> List.map schemaAstToJson) ]
        | ATaggedUnion cases ->
            jo [ "oneOf", ja (cases |> List.map (snd >> schemaAstToJson)) ]
        | ARefine(ast, _)
        | AMeasured ast -> schemaAstToJson ast
        | ADeclare name -> jo [ "$ref", js ("#/$defs/" + name) ]

    let private responseContent (schema: HttpApiContent) =
        match schema.Kind with
        | Empty -> None
        | Buffered ->
            Some(
                schema.ContentType,
                jo [ "schema", schema.Schema |> Option.map schemaAstToJson |> Option.defaultValue (jo []) ]
            )
        | StreamUint8Array ->
            Some(
                schema.ContentType,
                jo
                    [ "x-effect-stream",
                      jo
                          [ "encoding", js "uint8array"
                            "contentType", js schema.ContentType ] ]
            )
        | StreamSse(mode, _events, error) ->
            Some(
                schema.ContentType,
                jo
                    [ "x-effect-stream",
                      jo
                          [ "encoding", js "sse"
                            "mode", js mode
                            "failureEvent", js HttpApiSchema.streamFailureEvent
                            "errorSchema", schemaAstToJson error
                            "causeSchema", jo [ "type", js "object" ] ] ]
            )

    let private responseForStatus (schemas: HttpApiContent list) =
        let content =
            schemas
            |> List.choose responseContent
            |> Map.ofList

        if Map.isEmpty content then
            jo [ "description", js "No Content" ]
        else
            jo [ "description", js "Response"; "content", JObject content ]

    let private operationForEndpoint (endpoint: HttpApiEndpoint) =
        let responses =
            endpoint.Options.Success
            |> List.groupBy (fun schema -> schema.Status)
            |> List.map (fun (status, schemas) -> string status, responseForStatus schemas)
            |> Map.ofList

        jo
            [ "operationId", js endpoint.Name
              "responses", JObject responses ]

    let fromApi (api: HttpApi) : Json =
        let paths =
            api.Groups
            |> Map.toList
            |> List.collect (fun (_, group) -> group.Endpoints |> Map.toList |> List.map snd)
            |> List.groupBy (fun endpoint -> endpoint.Path)
            |> List.map (fun (path, endpoints) ->
                let operations =
                    endpoints
                    |> List.map (fun endpoint -> HttpApiEndpoint.methodString endpoint |> fun m -> m.ToLowerInvariant(), operationForEndpoint endpoint)
                    |> Map.ofList

                path, JObject operations)
            |> Map.ofList

        jo
            [ "openapi", js "3.1.0"
              "info", jo [ "title", js api.Identifier; "version", js "0.0.1" ]
              "paths", JObject paths ]
