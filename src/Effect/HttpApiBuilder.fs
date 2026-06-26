namespace Effect

open System.Text

type HttpApiServerRequest =
    { Request: HttpServerRequest
      Params: Map<string, string>
      Query: UrlParams
      Headers: Headers
      Payload: HttpBody
      ParamsValue: obj option
      QueryValue: obj option
      HeadersValue: obj option
      PayloadValue: obj option }

[<RequireQualifiedAccess>]
type HttpApiHandlerResult =
    | Buffered of obj
    | StreamSse of Stream<obj, obj, Context>
    | StreamBytes of Stream<byte[], obj, Context>

type HttpApiEndpointHandler = HttpApiServerRequest -> Effect<HttpServerResponse, HttpServerError, Context>
type HttpApiEndpointResultHandler = HttpApiServerRequest -> Effect<HttpApiHandlerResult, HttpServerError, Context>

type HttpApiGroupHandlers =
    { Group: HttpApiGroup
      Handlers: Map<string, HttpApiEndpointHandler> }

type HttpApiHandlers =
    { Api: HttpApi
      Groups: Map<string, HttpApiGroupHandlers> }

[<RequireQualifiedAccess>]
module HttpApiBuilder =

    let private missingSecurity request description =
        HttpServerError.RequestParseError(request, description)

    let private decodeBasic (value: string) =
        match Encoding.decodeBase64String value with
        | Error _ -> None
        | Ok decoded ->
            let index = decoded.IndexOf(':')

            if index < 0 then
                None
            else
                Some
                    { Username = decoded.Substring(0, index)
                      Password = Redacted.make (decoded.Substring(index + 1)) }

    let securityDecode
        (security: HttpApiSecurity)
        (request: HttpServerRequest)
        : Effect<HttpApiSecurityCredentials, HttpServerError, Context> =
        let missing description = Effect.fail (missingSecurity request description)

        match security with
        | Http scheme ->
            match Headers.get "authorization" request.Headers with
            | None -> missing "Missing Authorization header"
            | Some authorization ->
                let prefix = scheme + " "

                if authorization.StartsWith(prefix, System.StringComparison.OrdinalIgnoreCase) then
                    let credential = authorization.Substring(prefix.Length).TrimStart()

                    if credential = "" then
                        missing ("Missing " + scheme + " credentials")
                    else
                        Effect.succeed (HttpCredential(Redacted.make credential))
                elif authorization.Equals(scheme, System.StringComparison.OrdinalIgnoreCase) then
                    missing ("Missing " + scheme + " credentials")
                else
                    missing ("Expected Authorization scheme " + scheme)
        | Basic ->
            match Headers.get "authorization" request.Headers with
            | None -> missing "Missing Authorization header"
            | Some authorization ->
                let prefix = "Basic "

                if authorization.StartsWith(prefix, System.StringComparison.OrdinalIgnoreCase) then
                    match decodeBasic (authorization.Substring(prefix.Length).Trim()) with
                    | Some credentials -> Effect.succeed (BasicCredentials credentials)
                    | None -> missing "Invalid Basic credentials"
                else
                    missing "Expected Authorization scheme Basic"
        | ApiKey(name, location) ->
            let value =
                match location with
                | Header -> Headers.get name request.Headers
                | Query -> request |> HttpServerRequest.searchParams |> UrlParams.getFirst name
                | Cookie -> Map.tryFind name request.Cookies

            match value with
            | Some value when value <> "" -> Effect.succeed (ApiKeyCredential(Redacted.make value))
            | _ -> missing ("Missing API key: " + name)

    let private schemaErrorText (error: SchemaError) =
        sprintf "%A" error.Issue

    let private inputSchema (value: obj option) =
        HttpApiEndpoint.tryInputSchema value

    let private decodeInput (request: HttpServerRequest) name schema json =
        Schema.decode schema json
        |> Result.mapError (fun error -> HttpServerError.RequestParseError(request, name + ": " + schemaErrorText error))
        |> Effect.fromResult

    let private objectFromMap (values: Map<string, string>) =
        values
        |> Map.toList
        |> List.map (fun (key, value) -> key, JString value)
        |> Map.ofList
        |> JObject

    let private objectFromUrlParams (values: UrlParams) =
        values
        |> List.groupBy fst
        |> List.map (fun (key, values) ->
            let values = values |> List.map snd

            key,
            match values with
            | [ value ] -> JString value
            | many -> JArray(many |> List.map JString))
        |> Map.ofList
        |> JObject

    let private decodeOptionalInput (request: HttpServerRequest) name schemaOption json =
        match schemaOption with
        | None -> Effect.succeed None
        | Some schema ->
            decodeInput request name schema json
            |> Effect.map Some

    let private requestContentType (request: HttpServerRequest) =
        Headers.get "content-type" request.Headers
        |> Option.defaultValue ""
        |> HttpApiSchema.baseContentType

    let private selectPayloadContent (request: HttpServerRequest) (payloads: HttpApiContent list) =
        match payloads with
        | [] -> None
        | [ only ] -> Some only
        | many ->
            let contentType = requestContentType request

            many
            |> List.tryFind (fun content -> HttpApiSchema.baseContentType content.ContentType = contentType)

    let private payloadJson (request: HttpServerRequest) (content: HttpApiContent) =
        match content.Payload with
        | HttpApiPayload.Empty -> Ok JNull
        | HttpApiPayload.Buffered _ when HttpApiSchema.encoding content = Text ->
            match HttpBody.asText request.Body with
            | Some text -> Ok(JString text)
            | None -> Error "Expected text request body"
        | HttpApiPayload.Buffered _ when HttpApiSchema.encoding content = FormUrlEncoded ->
            match HttpBody.asText request.Body |> Option.orElseWith (fun () -> HttpBody.encodedText request.Body) with
            | Some text -> Ok(objectFromUrlParams (UrlParams.parseQueryString text))
            | None -> Error "Expected form-url-encoded request body"
        | HttpApiPayload.Buffered _ ->
            match request.Body with
            | JsonBody json -> Ok json
            | body ->
                match HttpBody.encodedText body with
                | Some text -> Json.parse text |> Result.mapError (fun error -> "Invalid JSON request body: " + error)
                | None -> Error "Expected buffered request body"
        | HttpApiPayload.StreamBytes
        | HttpApiPayload.StreamSse _ -> Error "Streaming request payload schemas are not supported"

    let private decodePayload (request: HttpServerRequest) (content: HttpApiContent) =
        match content.Payload with
        | HttpApiPayload.Buffered schema ->
            payloadJson request content
            |> Result.mapError (fun reason -> HttpServerError.RequestParseError(request, "payload: " + reason))
            |> Result.bind (fun json ->
                Schema.decode schema json
                |> Result.mapError (fun error -> HttpServerError.RequestParseError(request, "payload: " + schemaErrorText error)))
            |> Effect.fromResult
            |> Effect.map Some
        | HttpApiPayload.Empty -> Effect.succeed None
        | HttpApiPayload.StreamBytes
        | HttpApiPayload.StreamSse _ -> Effect.fail (HttpServerError.RequestParseError(request, "payload: Streaming request payload schemas are not supported"))

    let private endpointInput (endpoint: HttpApiEndpoint) (request: HttpServerRequest) (parameters: Map<string, string>) =
        let query = HttpServerRequest.searchParams request

        decodeOptionalInput request "params" (inputSchema endpoint.Options.Params) (objectFromMap parameters)
        |> Effect.flatMap (fun paramsValue ->
            decodeOptionalInput request "query" (inputSchema endpoint.Options.Query) (objectFromUrlParams query)
            |> Effect.flatMap (fun queryValue ->
                decodeOptionalInput request "headers" (inputSchema endpoint.Options.Headers) (objectFromMap request.Headers)
                |> Effect.flatMap (fun headersValue ->
                    let payloadEffect =
                        match selectPayloadContent request endpoint.Options.Payload with
                        | None -> Effect.succeed None
                        | Some content -> decodePayload request content

                    payloadEffect
                    |> Effect.map (fun payloadValue ->
                        { Request = request
                          Params = parameters
                          Query = query
                          Headers = request.Headers
                          Payload = request.Body
                          ParamsValue = paramsValue
                          QueryValue = queryValue
                          HeadersValue = headersValue
                          PayloadValue = payloadValue }))))

    let private requireSecurity (endpoint: HttpApiEndpoint) (request: HttpServerRequest) =
        let rec loop firstError securities =
            match securities with
            | [] ->
                firstError
                |> Option.map Effect.fail
                |> Option.defaultWith (fun () -> Effect.succeed ())
            | security :: rest ->
                securityDecode security request
                |> Effect.map ignore
                |> Effect.catchAll (fun error -> loop (firstError |> Option.orElse (Some error)) rest)

        loop None endpoint.Options.Security

    let group (api: HttpApi) (groupName: string) (handlers: Map<string, HttpApiEndpointHandler>) : HttpApiGroupHandlers =
        match Map.tryFind groupName api.Groups with
        | None -> invalidArg "groupName" ("Unknown HttpApi group: " + groupName)
        | Some group ->
            for endpointName in handlers |> Map.toSeq |> Seq.map fst do
                if not (Map.containsKey endpointName group.Endpoints) then
                    invalidArg "handlers" ("Unknown HttpApi endpoint: " + groupName + "." + endpointName)

            { Group = group
              Handlers = handlers }

    let handlers (api: HttpApi) (groups: HttpApiGroupHandlers list) : HttpApiHandlers =
        { Api = api
          Groups = groups |> List.map (fun group -> group.Group.Identifier, group) |> Map.ofList }

    let private applyMiddlewares (group: HttpApiGroup) (endpoint: HttpApiEndpoint) (effect: Effect<HttpServerResponse, HttpServerError, Context>) =
        let context =
            { Group = group.Identifier
              Endpoint = endpoint.Name }

        endpoint.Middlewares
        |> List.fold (fun acc middleware -> middleware.Apply context acc) effect

    let private sseFailureSchema (endpoint: HttpApiEndpoint) (response: HttpServerResponse) =
        let responseContentType =
            Headers.get "content-type" response.Headers
            |> Option.defaultValue ""
            |> HttpApiSchema.baseContentType

        endpoint.Options.Success
        |> List.tryPick (fun content ->
            if content.Status = response.Status then
                match content.Payload with
                | StreamSse(_, _, failure)
                    when responseContentType = ""
                         || responseContentType = HttpApiSchema.baseContentType content.ContentType ->
                    Some failure
                | _ -> None
            else
                None)

    let private sseFailureBytes (failureSchema: Schema<obj>) (error: obj) =
        Cause.fail error
        |> box
        |> failureSchema.Encode
        |> HttpBody.toJsonString
        |> Sse.event HttpApiSchema.streamFailureEvent
        |> Sse.encodeEvent
        |> Encoding.UTF8.GetBytes

    let private successForResult (endpoint: HttpApiEndpoint) (result: HttpApiHandlerResult) =
        endpoint.Options.Success
        |> List.tryFind (fun content ->
            match result, content.Payload with
            | HttpApiHandlerResult.Buffered _, HttpApiPayload.Buffered _
            | HttpApiHandlerResult.Buffered _, Empty
            | HttpApiHandlerResult.StreamSse _, HttpApiPayload.StreamSse _
            | HttpApiHandlerResult.StreamBytes _, HttpApiPayload.StreamBytes -> true
            | _ -> false)
        |> Option.orElseWith (fun () -> endpoint.Options.Success |> List.tryHead)
        |> Option.defaultValue HttpApiSchema.noContent

    let private responseOptions (content: HttpApiContent) =
        { HttpServerResponseOptions.empty with
            Status = Some content.Status
            ContentType =
                if content.ContentType = "" then
                    None
                else
                    Some content.ContentType }

    let private stringFromJson (json: Json) =
        match json with
        | JString value -> value
        | _ -> HttpBody.toJsonString json

    let private sseEventFromJson (json: Json) =
        match json with
        | JObject fields ->
            match Map.tryFind "event" fields, Map.tryFind "data" fields with
            | Some(JString event), Some(JString data) -> Sse.event event data
            | Some(JString event), Some data -> Sse.event event (HttpBody.toJsonString data)
            | _, Some(JString data) -> Sse.message data
            | _, Some data -> Sse.message (HttpBody.toJsonString data)
            | _ -> Sse.message (HttpBody.toJsonString json)
        | _ -> Sse.message (HttpBody.toJsonString json)

    let private renderSseValue (mode: string) (eventSchema: Schema<obj>) (failureSchema: Schema<obj>) (stream: Stream<obj, obj, Context>) =
        stream
        |> Stream.map (fun event ->
            let encoded = eventSchema.Encode event

            if mode = "data" then
                encoded
                |> HttpBody.toJsonString
                |> Sse.message
            else
                sseEventFromJson encoded
            |> Sse.encodeEvent
            |> Encoding.UTF8.GetBytes)
        |> Stream.catchAll (fun error ->
            sseFailureBytes failureSchema error
            |> Stream.succeed)

    let renderSuccess (endpoint: HttpApiEndpoint) (result: HttpApiHandlerResult) : HttpServerResponse =
        let content = successForResult endpoint result
        let options = responseOptions content

        match result, content.Payload with
        | HttpApiHandlerResult.Buffered _, Empty -> HttpServerResponse.emptyWith options
        | HttpApiHandlerResult.Buffered value, HttpApiPayload.Buffered schema ->
            let json = schema.Encode value

            match HttpApiSchema.encoding content with
            | Text -> HttpServerResponse.textWith options (stringFromJson json)
            | Uint8Array ->
                match value with
                | :? (byte[]) as bytes -> HttpServerResponse.bytesWith options bytes
                | _ -> HttpServerResponse.textWith options (stringFromJson json)
            | Json
            | FormUrlEncoded
            | Multipart -> HttpServerResponse.jsonWith options json
        | HttpApiHandlerResult.StreamBytes stream, HttpApiPayload.StreamBytes ->
            stream
            |> HttpServerResponse.streamBytesWith options
        | HttpApiHandlerResult.StreamSse stream, HttpApiPayload.StreamSse(mode, eventSchema, failureSchema) ->
            stream
            |> renderSseValue mode eventSchema failureSchema
            |> HttpServerResponse.streamBytesWith options
        | _ -> invalidOp "No compatible HttpApi success schema for handler result"

    let private renderStreamFailures (endpoint: HttpApiEndpoint) (response: HttpServerResponse) =
        match response.Body, sseFailureSchema endpoint response with
        | StreamBody(stream, contentType), Some failureSchema ->
            let stream =
                stream
                |> Stream.catchAll (fun error ->
                    sseFailureBytes failureSchema error
                    |> Stream.succeed)

            { response with Body = StreamBody(stream, contentType) }
        | _ -> response

    let private valueHandler (endpoint: HttpApiEndpoint) (handler: HttpApiEndpointResultHandler) : HttpApiEndpointHandler =
        fun input ->
            handler input
            |> Effect.map (renderSuccess endpoint)

    let groupTyped (api: HttpApi) (groupName: string) (handlers: Map<string, HttpApiEndpointResultHandler>) : HttpApiGroupHandlers =
        match Map.tryFind groupName api.Groups with
        | None -> invalidArg "groupName" ("Unknown HttpApi group: " + groupName)
        | Some groupInfo ->
            let responseHandlers =
                handlers
                |> Map.map (fun endpointName handler ->
                    match Map.tryFind endpointName groupInfo.Endpoints with
                    | Some endpoint -> valueHandler endpoint handler
                    | None -> invalidArg "handlers" ("Unknown HttpApi endpoint: " + groupName + "." + endpointName))

            group api groupName responseHandlers

    let private routeFor (group: HttpApiGroup) (endpoint: HttpApiEndpoint) (handler: HttpApiEndpointHandler) : HttpRoute =
        HttpRouter.routeHandler
            (HttpApiEndpoint.methodString endpoint)
            endpoint.Path
            (fun request parameters ->
                requireSecurity endpoint request
                |> Effect.flatMap (fun () ->
                    endpointInput endpoint request parameters
                    |> Effect.flatMap (fun input ->
                        handler input
                        |> applyMiddlewares group endpoint
                        |> Effect.map (renderStreamFailures endpoint))))

    let toRouter (handlers: HttpApiHandlers) : HttpRouter =
        handlers.Api.Groups
        |> Map.toList
        |> List.fold
            (fun router (groupName, group) ->
                match Map.tryFind groupName handlers.Groups with
                | None -> invalidArg "handlers" ("Missing HttpApi group handlers: " + groupName)
                | Some groupHandlers ->
                    group.Endpoints
                    |> Map.toList
                    |> List.fold
                        (fun router (endpointName, endpoint) ->
                            match Map.tryFind endpointName groupHandlers.Handlers with
                            | None -> invalidArg "handlers" ("Missing HttpApi endpoint handler: " + groupName + "." + endpointName)
                            | Some handler -> HttpRouter.addRoute (routeFor group endpoint handler) router)
                        router)
            HttpRouter.empty

    let route (api: HttpApi) (groups: HttpApiGroupHandlers list) : HttpRouter =
        handlers api groups |> toRouter
