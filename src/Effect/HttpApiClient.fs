namespace Effect

open System
open System.Text

type HttpApiClientUrlOptions = { BaseUrl: string }

type HttpApiClientUrlInput =
    { Params: Map<string, string>
      Query: UrlParams }

type HttpApiClientEndpointInput =
    { Params: Map<string, string>
      Query: UrlParams
      Headers: Headers
      Payload: HttpBody option }

type HttpApiUrlBuilder =
    { Api: HttpApi
      Options: HttpApiClientUrlOptions
      Endpoints: Map<string, Map<string, HttpApiClientUrlInput -> string>> }

type HttpApiClient =
    { Api: HttpApi
      Options: HttpApiClientUrlOptions
      Endpoints: Map<string, Map<string, HttpApiClientEndpointInput -> Effect<HttpClientResponse, HttpClientError, Context>>> }

type HttpApiClientResponseMode =
    | DecodedOnly
    | DecodedAndResponse
    | ResponseOnly

type HttpApiClientDecoded =
    | DecodedNoContent
    | DecodedBody of obj
    | DecodedStreamBytes of Stream<byte[], obj, Context>
    | DecodedStreamSse of Stream<obj, obj, Context>

type HttpApiClientResponse =
    | Decoded of HttpApiClientDecoded
    | DecodedWithResponse of HttpApiClientDecoded * HttpClientResponse
    | ResponseOnlyResponse of HttpClientResponse

type HttpApiClientError =
    | TransportError of HttpClientError
    | DecodeError of string
    | ResponseError of obj * HttpClientResponse

[<RequireQualifiedAccess>]
module HttpApiClient =

    let relativeOptions = { BaseUrl = "" }

    let emptyInput =
        { Params = Map.empty
          Query = UrlParams.empty }

    let emptyEndpointInput =
        { Params = Map.empty
          Query = UrlParams.empty
          Headers = Headers.empty
          Payload = None }

    let private encodePathSegment (value: string) = Uri.EscapeDataString(value)

    let private trimBaseUrl (baseUrl: string) = baseUrl.TrimEnd('/')

    let private pathSegments (path: string) =
        path.Trim('/').Split('/')
        |> Array.toList
        |> List.filter (fun segment -> segment <> "")

    let private buildPath (endpoint: HttpApiEndpoint) (input: HttpApiClientUrlInput) =
        let segments =
            pathSegments endpoint.Path
            |> List.choose (fun segment ->
                if segment.StartsWith(":") then
                    let rawName = segment.Substring(1)
                    let optional = rawName.EndsWith("?")
                    let name = if optional then rawName.Substring(0, rawName.Length - 1) else rawName

                    match Map.tryFind name input.Params with
                    | Some value when value <> "" -> Some(encodePathSegment value)
                    | Some _ when optional -> None
                    | Some _ -> invalidArg "params" ("Missing required path parameter: " + name)
                    | None when optional -> None
                    | None -> invalidArg "params" ("Missing required path parameter: " + name)
                else
                    Some segment)

        "/" + String.concat "/" segments

    let buildUrl (options: HttpApiClientUrlOptions) (endpoint: HttpApiEndpoint) (input: HttpApiClientUrlInput) : string =
        let path = buildPath endpoint input
        let baseUrl = trimBaseUrl options.BaseUrl
        let url = if baseUrl = "" then path else baseUrl + path
        UrlParams.appendToUrl url input.Query

    let urlBuilder (api: HttpApi) (options: HttpApiClientUrlOptions) : HttpApiUrlBuilder =
        let endpoints =
            api.Groups
            |> Map.map (fun _ group ->
                group.Endpoints
                |> Map.map (fun _ endpoint -> fun input -> buildUrl options endpoint input))

        { Api = api
          Options = options
          Endpoints = endpoints }

    let urlBuilderRelative (api: HttpApi) : HttpApiUrlBuilder =
        urlBuilder api relativeOptions

    let endpointUrl (group: string) (endpoint: string) (input: HttpApiClientUrlInput) (builder: HttpApiUrlBuilder) : string =
        match Map.tryFind group builder.Endpoints with
        | None -> invalidArg "group" ("Unknown HttpApi group: " + group)
        | Some endpoints ->
            match Map.tryFind endpoint endpoints with
            | Some build -> build input
            | None -> invalidArg "endpoint" ("Unknown HttpApi endpoint: " + group + "." + endpoint)

    let topLevelEndpointUrl (endpoint: string) (input: HttpApiClientUrlInput) (builder: HttpApiUrlBuilder) : string =
        builder.Api.Groups
        |> Map.toList
        |> List.tryPick (fun (groupName, group) ->
            if group.TopLevel && Map.containsKey endpoint group.Endpoints then
                Some(endpointUrl groupName endpoint input builder)
            else
                None)
        |> Option.defaultWith (fun () -> invalidArg "endpoint" ("Unknown top-level HttpApi endpoint: " + endpoint))

    let private endpointRequest (options: HttpApiClientUrlOptions) (endpoint: HttpApiEndpoint) (input: HttpApiClientEndpointInput) =
        let url =
            buildUrl
                options
                endpoint
                { Params = input.Params
                  Query = input.Query }

        let body = defaultArg input.Payload HttpBody.empty

        { HttpClientRequest.make (HttpApiEndpoint.methodString endpoint) url with
            Headers = input.Headers
            Body = body }

    let executeEndpoint
        (options: HttpApiClientUrlOptions)
        (endpoint: HttpApiEndpoint)
        (input: HttpApiClientEndpointInput)
        : Effect<HttpClientResponse, HttpClientError, Context> =
        endpointRequest options endpoint input |> HttpClient.execute

    let private schemaErrorText (error: SchemaError) =
        sprintf "%A" error.Issue

    let private decodeSchema (schema: Schema<obj>) (json: Json) : Result<obj, HttpApiClientError> =
        Schema.decode schema json
        |> Result.mapError (schemaErrorText >> DecodeError)

    let private decodeJsonText (text: string) =
        Json.parse text
        |> Result.mapError (fun message -> DecodeError("Invalid JSON response: " + message))

    let private bodyJson (content: HttpApiContent) (response: HttpClientResponse) : Result<Json, HttpApiClientError> =
        match content.Payload with
        | Empty -> Ok JNull
        | _ when HttpApiSchema.encoding content = Text ->
            match response.Body with
            | TextBody(text, _) -> Ok(JString text)
            | EmptyBody -> Ok(JString "")
            | _ -> Error(DecodeError "Expected text response body")
        | _ ->
            match response.Body with
            | JsonBody json -> Ok json
            | body ->
                match HttpBody.encodedText body with
                | Some text -> decodeJsonText text
                | None -> Error(DecodeError "Expected buffered response body")

    let private decodeBuffered (schema: Schema<obj>) (content: HttpApiContent) (response: HttpClientResponse) =
        bodyJson content response
        |> Result.bind (decodeSchema schema)
        |> Result.map DecodedBody
        |> Effect.fromResult

    let private streamOrError (response: HttpClientResponse) =
        match HttpClientResponse.bodyStream response with
        | Some stream -> Ok stream
        | None -> Error(DecodeError "Expected streaming response body")

    let private decodeSseData (eventSchema: Schema<obj>) (event: SseEvent) : Effect<obj, obj, Context> =
        Json.parse event.Data
        |> Result.mapError box
        |> Result.bind (fun json -> Schema.decode eventSchema json |> Result.mapError (schemaErrorText >> box))
        |> Effect.fromResult

    let private decodeSseEventObject (eventSchema: Schema<obj>) (event: SseEvent) : Effect<obj, obj, Context> =
        JObject(
            Map.ofList
                [ "event", JString event.Event
                  "data", JString event.Data ]
        )
        |> Schema.decode eventSchema
        |> Result.mapError (schemaErrorText >> box)
        |> Effect.fromResult

    let private decodeSseFailure (failureSchema: Schema<obj>) (event: SseEvent) : Effect<obj, obj, Context> =
        Json.parse event.Data
        |> Result.mapError box
        |> Result.bind (fun json -> Schema.decode failureSchema json |> Result.mapError (schemaErrorText >> box))
        |> function
            | Ok failure -> Effect.fail failure
            | Error error -> Effect.fail error

    let private decodeSseParsed (mode: string) (eventSchema: Schema<obj>) (failureSchema: Schema<obj>) (parsed: SseParsed) =
        match parsed with
        | SseRetry _ -> Effect.succeed None
        | SseEvent event when event.Event = HttpApiSchema.streamFailureEvent ->
            decodeSseFailure failureSchema event |> Effect.map Some
        | SseEvent event ->
            let decode =
                if mode = "data" then
                    decodeSseData eventSchema event
                else
                    decodeSseEventObject eventSchema event

            decode |> Effect.map Some

    let private decodeSseStream mode eventSchema failureSchema (bytes: Stream<byte[], obj, Context>) =
        let parser = Sse.makeParser ()

        bytes
        |> Stream.flatMap (fun chunk ->
            Encoding.UTF8.GetString chunk
            |> parser.Feed
            |> Stream.fromIterable)
        |> Stream.mapEffect (decodeSseParsed mode eventSchema failureSchema)
        |> Stream.flatMap (function
            | Some value -> Stream.succeed value
            | None -> Stream.empty)

    let private decodeContent (content: HttpApiContent) (response: HttpClientResponse) : Effect<HttpApiClientDecoded, HttpApiClientError, Context> =
        match content.Payload with
        | Empty -> Effect.succeed DecodedNoContent
        | Buffered schema -> decodeBuffered schema content response
        | StreamBytes ->
            streamOrError response
            |> Result.map DecodedStreamBytes
            |> Effect.fromResult
        | StreamSse(mode, eventSchema, failureSchema) ->
            streamOrError response
            |> Result.map (decodeSseStream mode eventSchema failureSchema >> DecodedStreamSse)
            |> Effect.fromResult

    let private selectContent (endpoint: HttpApiEndpoint) (response: HttpClientResponse) =
        let schemas =
            if HttpClientResponse.isSuccess response then
                endpoint.Options.Success
            else
                endpoint.Options.Error

        let candidates = schemas |> List.filter (fun content -> content.Status = response.Status)

        match candidates with
        | [] -> None
        | [ only ] -> Some only
        | many ->
            let responseContentType =
                Headers.get "content-type" response.Headers
                |> Option.defaultValue ""
                |> HttpApiSchema.baseContentType

            many
            |> List.tryFind (fun content -> HttpApiSchema.baseContentType content.ContentType = responseContentType)

    let private decodeResponse endpoint response =
        match selectContent endpoint response with
        | None -> Effect.fail (DecodeError(sprintf "No response schema for status %d" response.Status))
        | Some content ->
            decodeContent content response
            |> Effect.flatMap (fun decoded ->
                if HttpClientResponse.isSuccess response then
                    Effect.succeed decoded
                else
                    match decoded with
                    | DecodedBody error -> Effect.fail (ResponseError(error, response))
                    | DecodedNoContent -> Effect.fail (DecodeError(sprintf "Response failed with status %d" response.Status))
                    | DecodedStreamBytes _
                    | DecodedStreamSse _ -> Effect.fail (DecodeError "Streaming error responses are not supported"))

    let executeEndpointWithMode
        (mode: HttpApiClientResponseMode)
        (options: HttpApiClientUrlOptions)
        (endpoint: HttpApiEndpoint)
        (input: HttpApiClientEndpointInput)
        : Effect<HttpApiClientResponse, HttpApiClientError, Context> =
        executeEndpoint options endpoint input
        |> Effect.mapError TransportError
        |> Effect.flatMap (fun response ->
            match mode with
            | ResponseOnly -> Effect.succeed (ResponseOnlyResponse response)
            | DecodedOnly -> decodeResponse endpoint response |> Effect.map Decoded
            | DecodedAndResponse -> decodeResponse endpoint response |> Effect.map (fun decoded -> DecodedWithResponse(decoded, response)))

    let make (api: HttpApi) (options: HttpApiClientUrlOptions) : HttpApiClient =
        let endpoints =
            api.Groups
            |> Map.map (fun _ group ->
                group.Endpoints
                |> Map.map (fun _ endpoint -> fun input -> executeEndpoint options endpoint input))

        { Api = api
          Options = options
          Endpoints = endpoints }

    let makeRelative (api: HttpApi) : HttpApiClient =
        make api relativeOptions

    let endpoint
        (group: string)
        (endpoint: string)
        (input: HttpApiClientEndpointInput)
        (client: HttpApiClient)
        : Effect<HttpClientResponse, HttpClientError, Context> =
        match Map.tryFind group client.Endpoints with
        | None -> invalidArg "group" ("Unknown HttpApi group: " + group)
        | Some endpoints ->
            match Map.tryFind endpoint endpoints with
            | Some execute -> execute input
            | None -> invalidArg "endpoint" ("Unknown HttpApi endpoint: " + group + "." + endpoint)

    let topLevelEndpoint
        (endpointName: string)
        (input: HttpApiClientEndpointInput)
        (client: HttpApiClient)
        : Effect<HttpClientResponse, HttpClientError, Context> =
        client.Api.Groups
        |> Map.toList
        |> List.tryPick (fun (groupName, group) ->
            if group.TopLevel && Map.containsKey endpointName group.Endpoints then
                Some(endpoint groupName endpointName input client)
            else
                None)
        |> Option.defaultWith (fun () -> invalidArg "endpoint" ("Unknown top-level HttpApi endpoint: " + endpointName))

    let private findEndpoint (group: string) (endpointName: string) (api: HttpApi) =
        match Map.tryFind group api.Groups with
        | None -> invalidArg "group" ("Unknown HttpApi group: " + group)
        | Some group ->
            match Map.tryFind endpointName group.Endpoints with
            | Some endpoint -> endpoint
            | None -> invalidArg "endpoint" ("Unknown HttpApi endpoint: " + group.Identifier + "." + endpointName)

    let endpointWithMode
        (group: string)
        (endpointName: string)
        (mode: HttpApiClientResponseMode)
        (input: HttpApiClientEndpointInput)
        (client: HttpApiClient)
        : Effect<HttpApiClientResponse, HttpApiClientError, Context> =
        let endpoint = findEndpoint group endpointName client.Api
        executeEndpointWithMode mode client.Options endpoint input

    let topLevelEndpointWithMode
        (endpointName: string)
        (mode: HttpApiClientResponseMode)
        (input: HttpApiClientEndpointInput)
        (client: HttpApiClient)
        : Effect<HttpApiClientResponse, HttpApiClientError, Context> =
        client.Api.Groups
        |> Map.toList
        |> List.tryPick (fun (groupName, group) ->
            if group.TopLevel && Map.containsKey endpointName group.Endpoints then
                Some(endpointWithMode groupName endpointName mode input client)
            else
                None)
        |> Option.defaultWith (fun () -> invalidArg "endpoint" ("Unknown top-level HttpApi endpoint: " + endpointName))
