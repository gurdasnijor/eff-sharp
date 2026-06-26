module HttpApiBuilderSpec

open System.Text
open Effect
open Effect.Vitest

type StreamErrorBody = { Reason: string }
type StreamMessage = { Message: string }

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
        | None -> failwith "expected stream body"))
