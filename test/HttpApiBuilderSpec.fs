module HttpApiBuilderSpec

open System.Text
open Effect
open Effect.Vitest

type StreamErrorBody = { Reason: string }
type StreamMessage = { Message: string }
type DecodedParams = { Id: string }
type DecodedQuery = { Include: string; Tags: string list }
type DecodedHeaders = { Trace: string }
type DecodedPayload = { Name: string }
type MultipartUploadPayload = { Name: string; Files: MultipartPart list }

let private getTodo =
    HttpApiEndpoint.get
        "getTodo"
        "/todos/:id"
        { HttpApiEndpoint.empty with Params = Some(box [ "id" ]) }

let private createTodo =
    HttpApiEndpoint.post
        "createTodo"
        "/todos"
        { HttpApiEndpoint.empty with Payload = [ HttpApiSchema.asText Schema.string ] }

let private api =
    HttpApi.make "TodoApi"
    |> HttpApi.add (HttpApiGroup.make "todos" |> HttpApiGroup.addMany [ getTodo; createTodo ])

let private run effect = Runtime.runSync Runtime.defaultRuntime effect

let private streamErrorSchema: Schema<StreamErrorBody> =
    Schema.object {
        let! reason = Schema.field "reason" Schema.string (fun e -> e.Reason)
        return { Reason = reason }
    }

let private streamMessageSchema: Schema<StreamMessage> =
    Schema.object {
        let! message = Schema.field "message" Schema.string (fun m -> m.Message)
        return { Message = message }
    }

let private decodedParamsSchema: Schema<DecodedParams> =
    Schema.object {
        let! id = Schema.field "id" Schema.string (fun p -> p.Id)
        return { Id = id }
    }

let private decodedQuerySchema: Schema<DecodedQuery> =
    Schema.object {
        let! include_ = Schema.field "include" Schema.string (fun q -> q.Include)
        and! tags = Schema.field "tags" (Schema.array Schema.string) (fun q -> q.Tags)
        return { Include = include_; Tags = tags }
    }

let private decodedHeadersSchema: Schema<DecodedHeaders> =
    Schema.object {
        let! trace = Schema.field "x-trace" Schema.string (fun h -> h.Trace)
        return { Trace = trace }
    }

let private decodedPayloadSchema: Schema<DecodedPayload> =
    Schema.object {
        let! name = Schema.field "name" Schema.string (fun p -> p.Name)
        return { Name = name }
    }

let private multipartUploadSchema: MultipartSchema<MultipartUploadPayload> =
    Multipart.object {
        let! name = Multipart.fieldSchema "name" Schema.string
        and! files = Multipart.filesSchema "files"
        return { Name = name; Files = files }
    }

describe "HttpApiBuilder" (fun () ->
    test "turns handlers into router routes with params and query" (fun () ->
        let group =
            HttpApiBuilder.group
                api
                "todos"
                (Map.ofList
                    [ "getTodo",
                      fun input ->
                          let id = input.Params.["id"]
                          let include_ = UrlParams.getFirst "include" input.Query |> Option.defaultValue "none"
                          Effect.succeed (HttpServerResponse.text (id + ":" + include_))
                      "createTodo",
                      fun input ->
                          let payload = HttpBody.asText input.Payload |> Option.defaultValue ""
                          Effect.succeed (HttpServerResponse.textWith { HttpServerResponseOptions.empty with Status = Some 201 } payload) ])

        let router = HttpApiBuilder.route api [ group ]

        let getResponse =
            router
            |> HttpRouter.handle (HttpServerRequest.make "GET" "/todos/42?include=comments" Headers.empty HttpBody.empty)
            |> run

        let postResponse =
            router
            |> HttpRouter.handle
                (HttpServerRequest.make
                    "POST"
                    "/todos"
                    (Headers.ofList [ "content-type", "text/plain" ])
                    (HttpBody.text "new todo"))
            |> run

        toBe (HttpBody.asText getResponse.Body) (Some "42:comments")
        toBe postResponse.Status 201
        toBe (HttpBody.asText postResponse.Body) (Some "new todo"))

    test "decodes params, query, headers, and JSON payload through endpoint schemas" (fun () ->
        let endpoint =
            HttpApiEndpoint.post
                "create"
                "/todos/:id"
                { HttpApiEndpoint.empty with
                    Params = HttpApiEndpoint.paramsSchema decodedParamsSchema
                    Query = HttpApiEndpoint.querySchema decodedQuerySchema
                    Headers = HttpApiEndpoint.headersSchema decodedHeadersSchema
                    Payload = [ HttpApiSchema.asJson decodedPayloadSchema ]
                    Success = [ HttpApiSchema.asText Schema.string ] }

        let api =
            HttpApi.make "DecodedApi"
            |> HttpApi.add (HttpApiGroup.make "todos" |> HttpApiGroup.add endpoint)

        let group =
            HttpApiBuilder.group
                api
                "todos"
                (Map.ofList
                    [ "create",
                      fun input ->
                          let params_ = input.ParamsValue |> Option.get |> unbox<DecodedParams>
                          let query = input.QueryValue |> Option.get |> unbox<DecodedQuery>
                          let headers = input.HeadersValue |> Option.get |> unbox<DecodedHeaders>
                          let payload = input.PayloadValue |> Option.get |> unbox<DecodedPayload>

                          sprintf "%s:%s:%s:%s:%s" params_.Id query.Include (String.concat "," query.Tags) headers.Trace payload.Name
                          |> HttpServerResponse.text
                          |> Effect.succeed ])

        let response =
            HttpApiBuilder.route api [ group ]
            |> HttpRouter.handle
                (HttpServerRequest.make
                    "POST"
                    "/todos/42?include=comments&tags=urgent&tags=home"
                    (Headers.ofList [ "x-trace", "abc"; "content-type", "application/json" ])
                    (HttpBody.json (JObject(Map.ofList [ "name", JString "write" ]))))
            |> run

        toBe response.Status 200
        toBe (HttpBody.asText response.Body) (Some "42:comments:urgent,home:abc:write"))

    test "decodes form-url-encoded payloads through endpoint schemas" (fun () ->
        let endpoint =
            HttpApiEndpoint.post
                "submit"
                "/submit"
                { HttpApiEndpoint.empty with
                    Payload = [ HttpApiSchema.asFormUrlEncoded decodedPayloadSchema ]
                    Success = [ HttpApiSchema.asText Schema.string ] }

        let api =
            HttpApi.make "DecodedApi"
            |> HttpApi.add (HttpApiGroup.make "forms" |> HttpApiGroup.add endpoint)

        let group =
            HttpApiBuilder.group
                api
                "forms"
                (Map.ofList
                    [ "submit",
                      fun input ->
                          let payload = input.PayloadValue |> Option.get |> unbox<DecodedPayload>
                          Effect.succeed (HttpServerResponse.text payload.Name) ])

        let response =
            HttpApiBuilder.route api [ group ]
            |> HttpRouter.handle
                (HttpServerRequest.make
                    "POST"
                    "/submit"
                    (Headers.ofList [ "content-type", "application/x-www-form-urlencoded" ])
                    (HttpBody.textWithContentType "application/x-www-form-urlencoded" "name=write+tests"))
            |> run

        toBe response.Status 200
        toBe (HttpBody.asText response.Body) (Some "write tests"))

    test "decodes multipart payloads through request multipart accessors" (fun () ->
        let endpoint =
            HttpApiEndpoint.post
                "upload"
                "/upload"
                { HttpApiEndpoint.empty with
                    Payload = [ HttpApiSchema.asMultipart decodedPayloadSchema ]
                    Success = [ HttpApiSchema.asText Schema.string ] }

        let api =
            HttpApi.make "DecodedApi"
            |> HttpApi.add (HttpApiGroup.make "forms" |> HttpApiGroup.add endpoint)

        let group =
            HttpApiBuilder.group
                api
                "forms"
                (Map.ofList
                    [ "upload",
                      fun input ->
                          let payload = input.PayloadValue |> Option.get |> unbox<DecodedPayload>
                          Effect.succeed (HttpServerResponse.text payload.Name) ])

        let request =
            { HttpServerRequest.make
                "POST"
                "/upload"
                (Headers.ofList [ "content-type", "multipart/form-data; boundary=test" ])
                HttpBody.empty with
                Multipart =
                    Some(fun () ->
                        Effect.succeed
                            { Fields = Map.ofList [ "name", "from multipart" ]
                              Files = Map.empty }) }

        let response =
            HttpApiBuilder.route api [ group ]
            |> HttpRouter.handle request
            |> run

        toBe response.Status 200
        toBe (HttpBody.asText response.Body) (Some "from multipart"))

    test "decodes multipart file payloads through multipart-native schemas" (fun () ->
        let endpoint =
            HttpApiEndpoint.post
                "upload"
                "/upload"
                { HttpApiEndpoint.empty with
                    Payload = [ HttpApiSchema.asMultipartSchema multipartUploadSchema ]
                    Success = [ HttpApiSchema.asText Schema.string ] }

        let api =
            HttpApi.make "DecodedApi"
            |> HttpApi.add (HttpApiGroup.make "forms" |> HttpApiGroup.add endpoint)

        let group =
            HttpApiBuilder.group
                api
                "forms"
                (Map.ofList
                    [ "upload",
                      fun input ->
                          let payload = input.PayloadValue |> Option.get |> unbox<MultipartUploadPayload>

                          let fileName =
                              match payload.Files with
                              | MultipartFile(_, name, _, _, _) :: _ -> name
                              | _ -> "missing"

                          Effect.succeed (HttpServerResponse.text (payload.Name + ":" + fileName)) ])

        let file = Multipart.file "files" "upload.txt" "text/plain" Stream.empty (box "source")

        let request =
            { HttpServerRequest.make
                "POST"
                "/upload"
                (Headers.ofList [ "content-type", "multipart/form-data; boundary=test" ])
                HttpBody.empty with
                Multipart =
                    Some(fun () ->
                        Effect.succeed
                            { Fields = Map.ofList [ "name", "from multipart" ]
                              Files = Map.ofList [ "files", [ file ] ] }) }

        let response =
            HttpApiBuilder.route api [ group ]
            |> HttpRouter.handle request
            |> run

        toBe response.Status 200
        toBe (HttpBody.asText response.Body) (Some "from multipart:upload.txt"))

    test "fails before invoking handlers when request schema decoding fails" (fun () ->
        let endpoint =
            HttpApiEndpoint.get
                "search"
                "/search"
                { HttpApiEndpoint.empty with
                    Query = HttpApiEndpoint.querySchema decodedQuerySchema }

        let api =
            HttpApi.make "DecodedApi"
            |> HttpApi.add (HttpApiGroup.make "todos" |> HttpApiGroup.add endpoint)

        let mutable called = false

        let group =
            HttpApiBuilder.group
                api
                "todos"
                (Map.ofList
                    [ "search",
                      fun _ ->
                          called <- true
                          Effect.succeed HttpServerResponse.empty ])

        let result =
            HttpApiBuilder.route api [ group ]
            |> HttpRouter.handle (HttpServerRequest.make "GET" "/search?include=comments" Headers.empty HttpBody.empty)
            |> Effect.runSync Runtime.defaultRuntime.Context

        toBe called false

        match result with
        | Success response -> failwithf "expected request parse failure, got %A" response
        | Failure cause ->
            match Cause.failures cause with
            | RequestParseError(_, reason) :: _ -> toBe (reason.StartsWith "query:") true
            | other -> failwithf "expected RequestParseError, got %A" other)

    test "fails fast when endpoint handlers are missing" (fun () ->
        let group =
            HttpApiBuilder.group
                api
                "todos"
                (Map.ofList [ "getTodo", fun _ -> Effect.succeed (HttpServerResponse.text "ok") ])

        toThrow (fun () -> HttpApiBuilder.route api [ group ] |> ignore))

    test "rejects secured endpoints before invoking handlers" (fun () ->
        let securedEndpoint =
            HttpApiEndpoint.get "secure" "/secure" HttpApiEndpoint.empty
            |> HttpApiEndpoint.addSecurity HttpApiSecurity.bearer

        let securedApi =
            HttpApi.make "SecureApi"
            |> HttpApi.add (HttpApiGroup.make "secure" |> HttpApiGroup.add securedEndpoint)

        let mutable called = false

        let group =
            HttpApiBuilder.group
                securedApi
                "secure"
                (Map.ofList
                    [ "secure",
                      fun _ ->
                          called <- true
                          Effect.succeed (HttpServerResponse.text "ok") ])

        let result =
            HttpApiBuilder.route securedApi [ group ]
            |> HttpRouter.handle (HttpServerRequest.make "GET" "/secure" Headers.empty HttpBody.empty)
            |> Effect.runSync Runtime.defaultRuntime.Context

        match result with
        | Failure cause ->
            toBe called false

            match Cause.failures cause with
            | RequestParseError(_, reason) :: _ -> toBe reason "Missing Authorization header"
            | other -> failwithf "expected RequestParseError, got %A" other
        | Success response -> failwithf "expected security failure, got %A" response)

    test "accepts any declared endpoint security alternative" (fun () ->
        let securedEndpoint =
            HttpApiEndpoint.get "secure" "/secure" HttpApiEndpoint.empty
            |> HttpApiEndpoint.addSecurities
                [ HttpApiSecurity.bearer
                  HttpApiSecurity.apiKeyHeader "x-api-key" ]

        let securedApi =
            HttpApi.make "SecureApi"
            |> HttpApi.add (HttpApiGroup.make "secure" |> HttpApiGroup.add securedEndpoint)

        let group =
            HttpApiBuilder.group
                securedApi
                "secure"
                (Map.ofList [ "secure", fun _ -> Effect.succeed (HttpServerResponse.text "ok") ])

        let response =
            HttpApiBuilder.route securedApi [ group ]
            |> HttpRouter.handle
                (HttpServerRequest.make
                    "GET"
                    "/secure"
                    (Headers.ofList [ "x-api-key", "secret" ])
                    HttpBody.empty)
            |> run

        toBe response.Status 200
        toBe (HttpBody.asText response.Body) (Some "ok"))

    test "renders StreamSse failures as a reserved full-cause event" (fun () ->
        let eventsSchema =
            Schema.object {
                let! event = Schema.field "event" Schema.string (fun e -> fst e)
                and! data = Schema.field "data" Schema.string (fun e -> snd e)
                return event, data
            }

        let endpoint =
            HttpApiEndpoint.get
                "events"
                "/events"
                { HttpApiEndpoint.empty with Success = [ HttpApiSchema.streamSse eventsSchema streamErrorSchema ] }

        let api =
            HttpApi.make "StreamApi"
            |> HttpApi.add (HttpApiGroup.make "test" |> HttpApiGroup.add endpoint)

        let group =
            HttpApiBuilder.group
                api
                "test"
                (Map.ofList
                    [ "events",
                      fun _ ->
                          Stream.fromEffect (Effect.fail (box { Reason = "boom" }))
                          |> HttpServerResponse.streamBytesWith
                              { HttpServerResponseOptions.empty with ContentType = Some "text/event-stream" }
                          |> Effect.succeed ])

        let response =
            HttpApiBuilder.route api [ group ]
            |> HttpRouter.handle (HttpServerRequest.make "GET" "/events" Headers.empty HttpBody.empty)
            |> run

        match HttpClientResponse.bodyStream (HttpServerResponse.toClientResponse (HttpClientRequest.get "/events") response) with
        | Some stream ->
            let rendered =
                stream
                |> Stream.runCollect
                |> Runtime.runSync Runtime.defaultRuntime
                |> List.map Encoding.UTF8.GetString
                |> String.concat ""

            toBe (rendered.StartsWith("event: " + HttpApiSchema.streamFailureEvent + "\ndata: ")) true
            toBe (rendered.EndsWith("\n\n")) true

            let data =
                rendered.Split('\n').[1].Substring("data: ".Length)

            match Json.parse data with
            | Error error -> failwithf "expected encoded cause JSON, got %s" error
            | Ok json ->
                match Schema.decode (Schema.cause streamErrorSchema) json with
                | Ok cause ->
                    match Cause.failures cause with
                    | [ error ] -> toBe error.Reason "boom"
                    | other -> failwithf "expected one failure, got %A" other
                | Error error -> failwithf "expected encoded cause, got %A" error
        | None -> failwith "expected stream body")

    test "emits StreamUint8Array handler responses as streamed bytes with declared content type" (fun () ->
        let endpoint =
            HttpApiEndpoint.get
                "download"
                "/download"
                { HttpApiEndpoint.empty with
                    Success = [ HttpApiSchema.streamUint8ArrayWithContentType "application/custom-bytes" |> HttpApiSchema.status 206 ] }

        let api =
            HttpApi.make "StreamApi"
            |> HttpApi.add (HttpApiGroup.make "test" |> HttpApiGroup.add endpoint)

        let group =
            HttpApiBuilder.group
                api
                "test"
                (Map.ofList
                    [ "download",
                      fun _ ->
                          Stream.fromIterable [ [| 1uy; 2uy |]; [| 3uy |] ]
                          |> HttpServerResponse.streamBytesWith
                              { HttpServerResponseOptions.empty with
                                  Status = Some 206
                                  ContentType = Some "application/custom-bytes" }
                          |> Effect.succeed ])

        let response =
            HttpApiBuilder.route api [ group ]
            |> HttpRouter.handle (HttpServerRequest.make "GET" "/download" Headers.empty HttpBody.empty)
            |> run

        toBe response.Status 206
        toBe (Headers.get "content-type" response.Headers) (Some "application/custom-bytes")

        match HttpClientResponse.bodyStream (HttpServerResponse.toClientResponse (HttpClientRequest.get "/download") response) with
        | Some stream ->
            let chunks = Stream.runCollect stream |> Runtime.runSync Runtime.defaultRuntime
            toEqual (chunks |> List.map Array.toList) [ [ 1uy; 2uy ]; [ 3uy ] ]
        | None -> failwith "expected stream body")

    test "renders successful StreamSse events incrementally with declared content type" (fun () ->
        let eventsSchema =
            Schema.object {
                let! event = Schema.field "event" Schema.string (fun e -> fst e)
                and! data = Schema.field "data" Schema.string (fun e -> snd e)
                return event, data
            }

        let endpoint =
            HttpApiEndpoint.get
                "events"
                "/events"
                { HttpApiEndpoint.empty with
                    Success =
                        [ HttpApiSchema.streamSseWithContentType
                              "text/event-stream; charset=utf-8"
                              eventsSchema
                              streamErrorSchema
                          |> HttpApiSchema.status 202 ] }

        let api =
            HttpApi.make "StreamApi"
            |> HttpApi.add (HttpApiGroup.make "test" |> HttpApiGroup.add endpoint)

        let group =
            HttpApiBuilder.group
                api
                "test"
                (Map.ofList
                    [ "events",
                      fun _ ->
                          Stream.fromIterable
                              [ Sse.encodeEvent (Sse.event "first" "one") |> Encoding.UTF8.GetBytes
                                Sse.encodeEvent (Sse.event "second" "two") |> Encoding.UTF8.GetBytes ]
                          |> HttpServerResponse.streamBytesWith
                              { HttpServerResponseOptions.empty with
                                  Status = Some 202
                                  ContentType = Some "text/event-stream; charset=utf-8" }
                          |> Effect.succeed ])

        let response =
            HttpApiBuilder.route api [ group ]
            |> HttpRouter.handle (HttpServerRequest.make "GET" "/events" Headers.empty HttpBody.empty)
            |> run

        toBe response.Status 202
        toBe (Headers.get "content-type" response.Headers) (Some "text/event-stream; charset=utf-8")

        match HttpClientResponse.bodyStream (HttpServerResponse.toClientResponse (HttpClientRequest.get "/events") response) with
        | Some stream ->
            let rendered =
                stream
                |> Stream.runCollect
                |> Runtime.runSync Runtime.defaultRuntime
                |> List.map Encoding.UTF8.GetString
                |> String.concat ""

            toBe rendered (Sse.encodeEvent (Sse.event "first" "one") + Sse.encodeEvent (Sse.event "second" "two"))
        | None -> failwith "expected stream body")

    test "supports buffered and stream successes with the same status" (fun () ->
        let endpoint =
            HttpApiEndpoint.get
                "mixed"
                "/mixed"
                { HttpApiEndpoint.empty with
                    Success =
                        [ HttpApiSchema.asJson streamMessageSchema
                          HttpApiSchema.streamSseData streamMessageSchema streamErrorSchema ] }

        let api =
            HttpApi.make "StreamApi"
            |> HttpApi.add (HttpApiGroup.make "test" |> HttpApiGroup.add endpoint)

        let group =
            HttpApiBuilder.group
                api
                "test"
                (Map.ofList
                    [ "mixed",
                      fun input ->
                          if UrlParams.getFirst "stream" input.Query = Some "true" then
                              Stream.fromIterable
                                  [ Sse.encodeEvent (Sse.event "message" """{"message":"stream"}""") |> Encoding.UTF8.GetBytes ]
                              |> HttpServerResponse.streamBytesWith
                                  { HttpServerResponseOptions.empty with ContentType = Some "text/event-stream" }
                              |> Effect.succeed
                          else
                              JObject(Map.ofList [ "message", JString "buffered" ])
                              |> HttpServerResponse.json
                              |> Effect.succeed ])

        let router = HttpApiBuilder.route api [ group ]

        let buffered =
            router
            |> HttpRouter.handle (HttpServerRequest.make "GET" "/mixed" Headers.empty HttpBody.empty)
            |> run

        let streamed =
            router
            |> HttpRouter.handle (HttpServerRequest.make "GET" "/mixed?stream=true" Headers.empty HttpBody.empty)
            |> run

        toBe buffered.Status 200
        toBe (Headers.get "content-type" buffered.Headers) (Some "application/json")
        toBe (HttpBody.encodedText buffered.Body) (Some """{"message":"buffered"}""")

        toBe streamed.Status 200
        toBe (Headers.get "content-type" streamed.Headers) (Some "text/event-stream")

        match HttpClientResponse.bodyStream (HttpServerResponse.toClientResponse (HttpClientRequest.get "/mixed?stream=true") streamed) with
        | Some stream ->
            let rendered =
                stream
                |> Stream.runCollect
                |> Runtime.runSync Runtime.defaultRuntime
                |> List.map Encoding.UTF8.GetString
                |> String.concat ""

            toBe rendered (Sse.encodeEvent (Sse.event "message" """{"message":"stream"}"""))
        | None -> failwith "expected stream body")

    test "renders typed buffered handler values through success schemas" (fun () ->
        let endpoint =
            HttpApiEndpoint.get
                "message"
                "/message"
                { HttpApiEndpoint.empty with
                    Success = [ HttpApiSchema.asJson streamMessageSchema |> HttpApiSchema.status 201 ] }

        let api =
            HttpApi.make "TypedApi"
            |> HttpApi.add (HttpApiGroup.make "test" |> HttpApiGroup.add endpoint)

        let group =
            HttpApiBuilder.groupTyped
                api
                "test"
                (Map.ofList [ "message", fun _ -> Effect.succeed (HttpApiHandlerResult.Buffered(box { Message = "typed" })) ])

        let response =
            HttpApiBuilder.route api [ group ]
            |> HttpRouter.handle (HttpServerRequest.make "GET" "/message" Headers.empty HttpBody.empty)
            |> run

        toBe response.Status 201
        toBe (Headers.get "content-type" response.Headers) (Some "application/json")
        toBe (HttpBody.encodedText response.Body) (Some """{"message":"typed"}"""))

    test "renders typed StreamUint8Array handler values through success schemas" (fun () ->
        let endpoint =
            HttpApiEndpoint.get
                "download"
                "/download"
                { HttpApiEndpoint.empty with
                    Success = [ HttpApiSchema.streamUint8ArrayWithContentType "application/custom-bytes" |> HttpApiSchema.status 206 ] }

        let api =
            HttpApi.make "TypedApi"
            |> HttpApi.add (HttpApiGroup.make "test" |> HttpApiGroup.add endpoint)

        let group =
            HttpApiBuilder.groupTyped
                api
                "test"
                (Map.ofList
                    [ "download",
                      fun _ ->
                          Stream.fromIterable [ [| 1uy; 2uy |]; [| 3uy |] ]
                          |> HttpApiHandlerResult.StreamBytes
                          |> Effect.succeed ])

        let response =
            HttpApiBuilder.route api [ group ]
            |> HttpRouter.handle (HttpServerRequest.make "GET" "/download" Headers.empty HttpBody.empty)
            |> run

        toBe response.Status 206
        toBe (Headers.get "content-type" response.Headers) (Some "application/custom-bytes")

        match HttpClientResponse.bodyStream (HttpServerResponse.toClientResponse (HttpClientRequest.get "/download") response) with
        | Some stream ->
            let chunks = Stream.runCollect stream |> Runtime.runSync Runtime.defaultRuntime
            toEqual (chunks |> List.map Array.toList) [ [ 1uy; 2uy ]; [ 3uy ] ]
        | None -> failwith "expected stream body")

    test "renders typed StreamSse data handler values through success schemas" (fun () ->
        let endpoint =
            HttpApiEndpoint.get
                "events"
                "/events"
                { HttpApiEndpoint.empty with
                    Success = [ HttpApiSchema.streamSseData streamMessageSchema streamErrorSchema ] }

        let api =
            HttpApi.make "TypedApi"
            |> HttpApi.add (HttpApiGroup.make "test" |> HttpApiGroup.add endpoint)

        let group =
            HttpApiBuilder.groupTyped
                api
                "test"
                (Map.ofList
                    [ "events",
                      fun _ ->
                          Stream.fromIterable [ box { Message = "hello" }; box { Message = "world" } ]
                          |> HttpApiHandlerResult.StreamSse
                          |> Effect.succeed ])

        let response =
            HttpApiBuilder.route api [ group ]
            |> HttpRouter.handle (HttpServerRequest.make "GET" "/events" Headers.empty HttpBody.empty)
            |> run

        toBe response.Status 200
        toBe (Headers.get "content-type" response.Headers) (Some "text/event-stream")

        match HttpClientResponse.bodyStream (HttpServerResponse.toClientResponse (HttpClientRequest.get "/events") response) with
        | Some stream ->
            let rendered =
                stream
                |> Stream.runCollect
                |> Runtime.runSync Runtime.defaultRuntime
                |> List.map Encoding.UTF8.GetString
                |> String.concat ""

            toBe
                rendered
                (Sse.encodeEvent (Sse.message """{"message":"hello"}""")
                 + Sse.encodeEvent (Sse.message """{"message":"world"}"""))
        | None -> failwith "expected stream body")

    test "renders typed StreamSse failures as reserved full-cause events" (fun () ->
        let endpoint =
            HttpApiEndpoint.get
                "events"
                "/events"
                { HttpApiEndpoint.empty with
                    Success = [ HttpApiSchema.streamSseData streamMessageSchema streamErrorSchema ] }

        let api =
            HttpApi.make "TypedApi"
            |> HttpApi.add (HttpApiGroup.make "test" |> HttpApiGroup.add endpoint)

        let group =
            HttpApiBuilder.groupTyped
                api
                "test"
                (Map.ofList
                    [ "events",
                      fun _ ->
                          (Stream.fromEffect (Effect.fail (box { Reason = "boom" })) : Stream<obj, obj, Context>)
                          |> HttpApiHandlerResult.StreamSse
                          |> Effect.succeed ])

        let response =
            HttpApiBuilder.route api [ group ]
            |> HttpRouter.handle (HttpServerRequest.make "GET" "/events" Headers.empty HttpBody.empty)
            |> run

        match HttpClientResponse.bodyStream (HttpServerResponse.toClientResponse (HttpClientRequest.get "/events") response) with
        | Some stream ->
            let rendered =
                stream
                |> Stream.runCollect
                |> Runtime.runSync Runtime.defaultRuntime
                |> List.map Encoding.UTF8.GetString
                |> String.concat ""

            toBe (rendered.StartsWith("event: " + HttpApiSchema.streamFailureEvent + "\ndata: ")) true

            let data =
                rendered.Split('\n').[1].Substring("data: ".Length)

            match Json.parse data with
            | Error error -> failwithf "expected encoded cause JSON, got %s" error
            | Ok json ->
                match Schema.decode (Schema.cause streamErrorSchema) json with
                | Ok cause ->
                    match Cause.failures cause with
                    | [ error ] -> toBe error.Reason "boom"
                    | other -> failwithf "expected one failure, got %A" other
                | Error error -> failwithf "expected encoded cause, got %A" error
        | None -> failwith "expected stream body")

    test "selects typed buffered or stream success by returned value shape" (fun () ->
        let endpoint =
            HttpApiEndpoint.get
                "mixed"
                "/mixed"
                { HttpApiEndpoint.empty with
                    Success =
                        [ HttpApiSchema.asJson streamMessageSchema
                          HttpApiSchema.streamSseData streamMessageSchema streamErrorSchema ] }

        let api =
            HttpApi.make "TypedApi"
            |> HttpApi.add (HttpApiGroup.make "test" |> HttpApiGroup.add endpoint)

        let group =
            HttpApiBuilder.groupTyped
                api
                "test"
                (Map.ofList
                    [ "mixed",
                      fun input ->
                          if UrlParams.getFirst "stream" input.Query = Some "true" then
                              Stream.fromIterable [ box { Message = "stream" } ]
                              |> HttpApiHandlerResult.StreamSse
                              |> Effect.succeed
                          else
                              Effect.succeed (HttpApiHandlerResult.Buffered(box { Message = "buffered" })) ])

        let router = HttpApiBuilder.route api [ group ]

        let buffered =
            router
            |> HttpRouter.handle (HttpServerRequest.make "GET" "/mixed" Headers.empty HttpBody.empty)
            |> run

        let streamed =
            router
            |> HttpRouter.handle (HttpServerRequest.make "GET" "/mixed?stream=true" Headers.empty HttpBody.empty)
            |> run

        toBe (Headers.get "content-type" buffered.Headers) (Some "application/json")
        toBe (HttpBody.encodedText buffered.Body) (Some """{"message":"buffered"}""")

        toBe (Headers.get "content-type" streamed.Headers) (Some "text/event-stream")

        match HttpClientResponse.bodyStream (HttpServerResponse.toClientResponse (HttpClientRequest.get "/mixed?stream=true") streamed) with
        | Some stream ->
            let rendered =
                stream
                |> Stream.runCollect
                |> Runtime.runSync Runtime.defaultRuntime
                |> List.map Encoding.UTF8.GetString
                |> String.concat ""

            toBe rendered (Sse.encodeEvent (Sse.message """{"message":"stream"}"""))
        | None -> failwith "expected stream body"))
