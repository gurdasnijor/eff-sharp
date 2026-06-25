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

    let private contentMap (schemas: HttpApiContent list) =
        schemas
        |> List.choose responseContent
        |> Map.ofList

    let private responseForStatus (schemas: HttpApiContent list) =
        let content = contentMap schemas

        if Map.isEmpty content then
            jo [ "description", js "No Content" ]
        else
            jo [ "description", js "Response"; "content", JObject content ]

    let private requestBody (endpoint: HttpApiEndpoint) =
        let content = contentMap endpoint.Options.Payload

        if Map.isEmpty content then
            None
        else
            Some("requestBody", jo [ "required", JBool true; "content", JObject content ])

    let private pathParameter (name: string) =
        jo
            [ "name", js name
              "in", js "path"
              "required", JBool true
              "schema", jo [ "type", js "string" ] ]

    let private pathParameters (path: string) =
        path.Trim('/').Split('/')
        |> Array.toList
        |> List.choose (fun segment ->
            if segment.StartsWith(":") then
                Some(pathParameter (segment.Substring(1).TrimEnd('?')))
            else
                None)

    let private parameters (endpoint: HttpApiEndpoint) =
        match pathParameters endpoint.Path with
        | [] -> None
        | parameters -> Some("parameters", ja parameters)

    let private responses (endpoint: HttpApiEndpoint) =
        let byStatus =
            (endpoint.Options.Success @ endpoint.Options.Error)
            |> List.fold
                (fun acc schema ->
                    let existing = acc |> Map.tryFind schema.Status |> Option.defaultValue []
                    acc |> Map.add schema.Status (existing @ [ schema ]))
                Map.empty

        byStatus
        |> Map.toList
        |> List.map (fun (status, schemas) -> string status, responseForStatus schemas)
        |> Map.ofList

    let private securityIdentifier =
        function
        | Http "Bearer" -> "Bearer"
        | Http scheme -> "Http" + scheme
        | Basic -> "Basic"
        | ApiKey(name, location) ->
            match location with
            | Header -> "ApiKeyHeader_" + name
            | Query -> "ApiKeyQuery_" + name
            | Cookie -> "ApiKeyCookie_" + name

    let private securityRequirement (security: HttpApiSecurity) =
        jo [ securityIdentifier security, ja [] ]

    let private securityRequirements (endpoint: HttpApiEndpoint) =
        match endpoint.Options.Security with
        | [] -> None
        | securities -> Some("security", ja (securities |> List.map securityRequirement))

    let private securityScheme =
        function
        | Http scheme ->
            securityIdentifier (Http scheme),
            jo
                [ "type", js "http"
                  "scheme", js (scheme.ToLowerInvariant()) ]
        | Basic ->
            securityIdentifier Basic,
            jo
                [ "type", js "http"
                  "scheme", js "basic" ]
        | ApiKey(name, location) ->
            let locationName =
                match location with
                | Header -> "header"
                | Query -> "query"
                | Cookie -> "cookie"

            securityIdentifier (ApiKey(name, location)),
            jo
                [ "type", js "apiKey"
                  "name", js name
                  "in", js locationName ]

    let private operationForEndpoint (group: HttpApiGroup) (endpoint: HttpApiEndpoint) =
        [ Some("operationId", js endpoint.Name)
          Some("tags", ja [ js group.Identifier ])
          parameters endpoint
          requestBody endpoint
          securityRequirements endpoint
          Some("responses", JObject(responses endpoint)) ]
        |> List.choose id
        |> jo

    let fromApi (api: HttpApi) : Json =
        let endpoints =
            api.Groups
            |> Map.toList
            |> List.collect (fun (_, group) -> group.Endpoints |> Map.toList |> List.map (fun (_, endpoint) -> group, endpoint))

        let paths =
            endpoints
            |> List.groupBy (fun (_, endpoint) -> endpoint.Path)
            |> List.map (fun (path, endpoints) ->
                let operations =
                    endpoints
                    |> List.map (fun (group, endpoint) -> HttpApiEndpoint.methodString endpoint |> fun m -> m.ToLowerInvariant(), operationForEndpoint group endpoint)
                    |> Map.ofList

                path, JObject operations)
            |> Map.ofList

        let securitySchemes =
            endpoints
            |> List.collect (fun (_, endpoint) -> endpoint.Options.Security)
            |> List.fold
                (fun acc security ->
                    let id, scheme = securityScheme security
                    Map.add id scheme acc)
                Map.empty

        let components =
            if Map.isEmpty securitySchemes then
                None
            else
                Some("components", jo [ "securitySchemes", JObject securitySchemes ])

        [ Some("openapi", js "3.1.0")
          Some("info", jo [ "title", js api.Identifier; "version", js "0.0.1" ])
          Some("paths", JObject paths)
          components ]
        |> List.choose id
        |> jo
