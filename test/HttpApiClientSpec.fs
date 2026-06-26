module HttpApiClientSpec

open System.Text
open Effect
open Effect.Vitest

type ClientEvent = { Event: string; Data: string }
type ClientData = { Text: string }
type ClientErrorBody = { Reason: string }

let private getUser =
    HttpApiEndpoint.get
        "getUser"
        "/users/:id"
        { HttpApiEndpoint.empty with
            Params = Some(box [ "id" ])
            Query = Some(box [ "page"; "tags" ]) }

let private health = HttpApiEndpoint.get "health" "/health" HttpApiEndpoint.empty

let private clientEventSchema: Schema<ClientEvent> =
    Schema.object {
        let! event = Schema.field "event" Schema.string (fun e -> e.Event)
        and! data = Schema.field "data" Schema.string (fun e -> e.Data)
        return { Event = event; Data = data }
    }

let private clientDataSchema: Schema<ClientData> =
    Schema.object {
        let! text = Schema.field "text" Schema.string (fun e -> e.Text)
        return { Text = text }
    }

let private clientErrorSchema: Schema<ClientErrorBody> =
    Schema.object {
        let! reason = Schema.field "reason" Schema.string (fun e -> e.Reason)
        return { Reason = reason }
    }

let private api =
    HttpApi.make "Api"
    |> HttpApi.add (HttpApiGroup.make "users" |> HttpApiGroup.addMany [ getUser; health ])

let private runWithClient httpClient effect =
    effect |> Runtime.runSync (Runtime.make (Context.make HttpClient.tag httpClient))

let private fakeClient response =
    { Request = fun _ _ -> Effect.succeed { Status = 200; Body = "" }
      Execute = fun request -> response request |> Effect.succeed }

let private bytes (text: string) = Encoding.UTF8.GetBytes text

describe "HttpApiClient.urlBuilder" (fun () ->
    test "builds urls from endpoint path params and query params" (fun () ->
        let builder = HttpApiClient.urlBuilder api { BaseUrl = "https://api.example.com" }

        let url =
            builder
            |> HttpApiClient.endpointUrl
                "users"
                "getUser"
                { Params = Map.ofList [ "id", "123" ]
                  Query = UrlParams.ofList [ "page", "1"; "tags", "1"; "tags", "2" ] }

        toBe url "https://api.example.com/users/123?page=1&tags=1&tags=2")

    test "encodes path parameters" (fun () ->
        let endpoint =
            HttpApiEndpoint.get
                "listResources"
                "/state/stacks/:stack/stages/:stage/resources"
                { HttpApiEndpoint.empty with Params = Some(box [ "stack"; "stage" ]) }

        let api = HttpApi.make "Api" |> HttpApi.add (HttpApiGroup.make "stacks" |> HttpApiGroup.add endpoint)
        let builder = HttpApiClient.urlBuilder api { BaseUrl = "https://api.example.com/" }

        let url =
            builder
            |> HttpApiClient.endpointUrl
                "stacks"
                "listResources"
                { Params = Map.ofList [ "stack", "a/b"; "stage", "prod/blue" ]
                  Query = UrlParams.empty }

        toBe url "https://api.example.com/state/stacks/a%2Fb/stages/prod%2Fblue/resources")

    test "omits missing optional path parameters" (fun () ->
        let endpoint =
            HttpApiEndpoint.get
                "download"
                "/files/:path?"
                { HttpApiEndpoint.empty with Params = Some(box [ "path" ]) }

        let api = HttpApi.make "Api" |> HttpApi.add (HttpApiGroup.make "files" |> HttpApiGroup.add endpoint)
        let builder = HttpApiClient.urlBuilder api { BaseUrl = "https://api.example.com" }

        let missing =
            builder
            |> HttpApiClient.endpointUrl
                "files"
                "download"
                { Params = Map.empty
                  Query = UrlParams.empty }

        let present =
            builder
            |> HttpApiClient.endpointUrl
                "files"
                "download"
                { Params = Map.ofList [ "path", "a/b" ]
                  Query = UrlParams.empty }

        toBe missing "https://api.example.com/files"
        toBe present "https://api.example.com/files/a%2Fb")

    test "builds relative urls when no base url is configured" (fun () ->
        let builder = HttpApiClient.urlBuilderRelative api

        let url =
            builder
            |> HttpApiClient.endpointUrl
                "users"
                "health"
                { Params = Map.empty
                  Query = UrlParams.empty }

        toBe url "/health")

    test "builds top-level endpoint urls" (fun () ->
        let topLevelApi =
            HttpApi.make "Api"
            |> HttpApi.add (HttpApiGroup.makeTopLevel "top" |> HttpApiGroup.add health)
            |> HttpApi.prefix "/v1"

        let builder = HttpApiClient.urlBuilder topLevelApi { BaseUrl = "https://api.example.com" }

        let url =
            builder
            |> HttpApiClient.topLevelEndpointUrl
                "health"
                { Params = Map.empty
                  Query = UrlParams.empty }

        toBe url "https://api.example.com/v1/health")

    test "throws for missing required path parameters" (fun () ->
        let builder = HttpApiClient.urlBuilder api { BaseUrl = "https://api.example.com" }

        toThrow (fun () ->
            builder
            |> HttpApiClient.endpointUrl
                "users"
                "getUser"
                { Params = Map.empty
                  Query = UrlParams.empty }
            |> ignore))

    test "executes endpoints through HttpClient" (fun () ->
        let mutable captured: HttpClientRequest option = None

        let httpClient =
            { Request =
                fun method url ->
                    Effect.succeed
                        { Status = 200
                          Body = method + " " + url }
              Execute =
                fun request ->
                    captured <- Some request
                    Effect.succeed (HttpClientResponse.text request 200 Headers.empty "ok") }

        let client = HttpApiClient.make api { BaseUrl = "https://api.example.com" }

        let response =
            client
            |> HttpApiClient.endpoint
                "users"
                "getUser"
                { HttpApiClient.emptyEndpointInput with
                    Params = Map.ofList [ "id", "123" ]
                    Query = UrlParams.ofList [ "page", "1" ]
                    Headers = Headers.ofList [ "x-test", "ok" ] }
            |> Runtime.runSync (Runtime.make (Context.make HttpClient.tag httpClient))

        toBe response.Status 200

        match captured with
        | Some request ->
            toBe request.Method "GET"
            toBe request.Url "https://api.example.com/users/123?page=1"
            toBe (Headers.get "x-test" request.Headers) (Some "ok")
        | None -> failwith "expected request capture")

    test "executes top-level endpoints through HttpClient" (fun () ->
        let topLevelApi =
            HttpApi.make "Api"
            |> HttpApi.add (HttpApiGroup.makeTopLevel "top" |> HttpApiGroup.add health)

        let mutable captured: HttpClientRequest option = None

        let httpClient =
            { Request = fun _ _ -> Effect.succeed { Status = 200; Body = "" }
              Execute =
                fun request ->
                    captured <- Some request
                    Effect.succeed (HttpClientResponse.text request 204 Headers.empty "") }

        let client = HttpApiClient.makeRelative topLevelApi

        let response =
            client
            |> HttpApiClient.topLevelEndpoint "health" HttpApiClient.emptyEndpointInput
            |> Runtime.runSync (Runtime.make (Context.make HttpClient.tag httpClient))

        toBe response.Status 204

        match captured with
        | Some request -> toBe request.Url "/health"
        | None -> failwith "expected request capture"))

describe "HttpApiClient response modes" (fun () ->
    test "decodes buffered JSON responses through the endpoint success schema" (fun () ->
        let endpoint =
            HttpApiEndpoint.get
                "hello"
                "/hello"
                { HttpApiEndpoint.empty with Success = [ HttpApiSchema.asJson Schema.string ] }

        let api = HttpApi.make "Api" |> HttpApi.add (HttpApiGroup.make "test" |> HttpApiGroup.add endpoint)

        let httpClient =
            fakeClient (fun request ->
                HttpClientResponse.json
                    request
                    200
                    (Headers.ofList [ "content-type", "application/json" ])
                    (JString "ok"))

        let client = HttpApiClient.make api { BaseUrl = "https://api.example.com" }

        match
            client
            |> HttpApiClient.endpointWithMode "test" "hello" DecodedOnly HttpApiClient.emptyEndpointInput
            |> runWithClient httpClient
        with
        | Decoded(DecodedBody value) -> toBe (unbox<string> value) "ok"
        | other -> failwithf "expected decoded body, got %A" other)

    test "returns raw responses in response-only mode" (fun () ->
        let endpoint =
            HttpApiEndpoint.get
                "download"
                "/download"
                { HttpApiEndpoint.empty with Success = [ HttpApiSchema.streamUint8Array ] }

        let api = HttpApi.make "Api" |> HttpApi.add (HttpApiGroup.make "test" |> HttpApiGroup.add endpoint)

        let httpClient =
            fakeClient (fun request ->
                HttpClientResponse.streamBytes
                    request
                    200
                    (Headers.ofList [ "content-type", "application/octet-stream" ])
                    (Stream.fromIterable [ [| 1uy |]; [| 2uy; 3uy |] ]))

        let client = HttpApiClient.make api { BaseUrl = "https://api.example.com" }

        match
            client
            |> HttpApiClient.endpointWithMode "test" "download" ResponseOnly HttpApiClient.emptyEndpointInput
            |> runWithClient httpClient
        with
        | ResponseOnlyResponse response ->
            toBe response.Status 200
            toBe (HttpClientResponse.bodyStream response |> Option.isSome) true
        | other -> failwithf "expected raw response, got %A" other)

    test "decodes StreamUint8Array responses as byte streams" (fun () ->
        let endpoint =
            HttpApiEndpoint.get
                "download"
                "/download"
                { HttpApiEndpoint.empty with Success = [ HttpApiSchema.streamUint8Array ] }

        let api = HttpApi.make "Api" |> HttpApi.add (HttpApiGroup.make "test" |> HttpApiGroup.add endpoint)

        let httpClient =
            fakeClient (fun request ->
                HttpClientResponse.streamBytes
                    request
                    200
                    (Headers.ofList [ "content-type", "application/octet-stream" ])
                    (Stream.fromIterable [ [| 1uy |]; [| 2uy; 3uy |] ]))

        let runtime = Runtime.make (Context.make HttpClient.tag httpClient)
        let client = HttpApiClient.make api { BaseUrl = "https://api.example.com" }

        match
            client
            |> HttpApiClient.endpointWithMode "test" "download" DecodedOnly HttpApiClient.emptyEndpointInput
            |> Runtime.runSync runtime
        with
        | Decoded(DecodedStreamBytes stream) ->
            let chunks = Stream.runCollect stream |> Runtime.runSync runtime
            toEqual (chunks |> List.map Array.toList) [ [ 1uy ]; [ 2uy; 3uy ] ]
        | other -> failwithf "expected decoded byte stream, got %A" other)

    test "decodes StreamSse event objects incrementally" (fun () ->
        let endpoint =
            HttpApiEndpoint.get
                "events"
                "/events"
                { HttpApiEndpoint.empty with Success = [ HttpApiSchema.streamSse clientEventSchema clientErrorSchema ] }

        let api = HttpApi.make "Api" |> HttpApi.add (HttpApiGroup.make "test" |> HttpApiGroup.add endpoint)

        let encoded =
            Sse.encodeEvent (Sse.event "first" "one")
            + Sse.encodeEvent (Sse.event "second" "two")

        let httpClient =
            fakeClient (fun request ->
                HttpClientResponse.streamBytes
                    request
                    200
                    (Headers.ofList [ "content-type", "text/event-stream; charset=utf-8" ])
                    (Stream.fromIterable [ bytes encoded ]))

        let runtime = Runtime.make (Context.make HttpClient.tag httpClient)
        let client = HttpApiClient.make api { BaseUrl = "https://api.example.com" }

        match
            client
            |> HttpApiClient.endpointWithMode "test" "events" DecodedOnly HttpApiClient.emptyEndpointInput
            |> Runtime.runSync runtime
        with
        | Decoded(DecodedStreamSse stream) ->
            let events = Stream.runCollect stream |> Runtime.runSync runtime
            let decoded = events |> List.map unbox<ClientEvent>
            toEqual decoded [ { Event = "first"; Data = "one" }; { Event = "second"; Data = "two" } ]
        | other -> failwithf "expected decoded sse stream, got %A" other)

    test "decodes StreamSse data events as JSON payloads" (fun () ->
        let endpoint =
            HttpApiEndpoint.get
                "events"
                "/events"
                { HttpApiEndpoint.empty with Success = [ HttpApiSchema.streamSseData clientDataSchema clientErrorSchema ] }

        let api = HttpApi.make "Api" |> HttpApi.add (HttpApiGroup.make "test" |> HttpApiGroup.add endpoint)

        let httpClient =
            fakeClient (fun request ->
                HttpClientResponse.streamBytes
                    request
                    200
                    (Headers.ofList [ "content-type", "text/event-stream" ])
                    (Stream.fromIterable [ bytes (Sse.encodeEvent (Sse.event "token" """{"text":"hello"}""")) ]))

        let runtime = Runtime.make (Context.make HttpClient.tag httpClient)
        let client = HttpApiClient.make api { BaseUrl = "https://api.example.com" }

        match
            client
            |> HttpApiClient.endpointWithMode "test" "events" DecodedOnly HttpApiClient.emptyEndpointInput
            |> Runtime.runSync runtime
        with
        | Decoded(DecodedStreamSse stream) ->
            match Stream.runCollect stream |> Runtime.runSync runtime with
            | [ value ] -> toBe (unbox<ClientData> value).Text "hello"
            | other -> failwithf "expected one event, got %A" other
        | other -> failwithf "expected decoded sse data stream, got %A" other))
