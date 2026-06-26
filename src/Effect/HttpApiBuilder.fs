namespace Effect

open System.Text

type HttpApiServerRequest =
    { Request: HttpServerRequest
      Params: Map<string, string>
      Query: UrlParams
      Headers: Headers
      Payload: HttpBody }

type HttpApiEndpointHandler = HttpApiServerRequest -> Effect<HttpServerResponse, HttpServerError, Context>
type HttpApiEndpointValueHandler = HttpApiServerRequest -> Effect<obj, HttpServerError, Context>

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

    let private endpointInput (request: HttpServerRequest) (parameters: Map<string, string>) =
        { Request = request
          Params = parameters
          Query = HttpServerRequest.searchParams request
          Headers = request.Headers
          Payload = request.Body }

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

    let private isStreamValue (value: obj) =
        match value with
        | :? Stream<obj, obj, Context> -> true
        | :? Stream<byte[], obj, Context> -> true
        | _ -> false

    let private successForValue (endpoint: HttpApiEndpoint) (value: obj) =
        let isStream = isStreamValue value

        endpoint.Options.Success
        |> List.tryFind (fun content ->
            match content.Payload with
            | StreamSse _
            | StreamBytes -> isStream
            | _ -> not isStream)
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

    let private renderSseValue (mode: string) (eventSchema: Schema<obj>) (failureSchema: Schema<obj>) (value: obj) =
        let source = unbox<Stream<obj, obj, Context>> value

        source
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

    let renderSuccess (endpoint: HttpApiEndpoint) (value: obj) : HttpServerResponse =
        let content = successForValue endpoint value
        let options = responseOptions content

        match content.Payload with
        | Empty -> HttpServerResponse.emptyWith options
        | Buffered schema ->
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
        | StreamBytes ->
            value
            |> unbox<Stream<byte[], obj, Context>>
            |> HttpServerResponse.streamBytesWith options
        | StreamSse(mode, eventSchema, failureSchema) ->
            value
            |> renderSseValue mode eventSchema failureSchema
            |> HttpServerResponse.streamBytesWith options

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

    let private valueHandler (endpoint: HttpApiEndpoint) (handler: HttpApiEndpointValueHandler) : HttpApiEndpointHandler =
        fun input ->
            handler input
            |> Effect.map (renderSuccess endpoint)

    let groupTyped (api: HttpApi) (groupName: string) (handlers: Map<string, HttpApiEndpointValueHandler>) : HttpApiGroupHandlers =
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
                    handler (endpointInput request parameters)
                    |> applyMiddlewares group endpoint
                    |> Effect.map (renderStreamFailures endpoint)))

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
