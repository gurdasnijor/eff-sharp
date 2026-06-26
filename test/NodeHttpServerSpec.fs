module NodeHttpServerSpec

open Effect
open Effect.Platform.Node
open Effect.Vitest

let private nodeHttpContext =
    Context.merge NodeHttpClient.liveContext Clock.live

let private closeHandle (handle: NodeHttpServerHandle) =
    handle.Close() |> Effect.mapError (fun e -> box e)

describe "NodeHttpServer" (fun () ->
    itEffectIn nodeHttpContext "serves router responses over local HTTP" (fun () ->
        let router =
            HttpRouter.empty
            |> HttpRouter.add "GET" "/hello" (HttpServerResponse.text "world")

        NodeHttpServer.listenLocal 0 router
        |> Effect.mapError (fun e -> box e)
        |> Effect.flatMap (fun handle ->
            let port = handle.Port |> Option.defaultWith (fun () -> failwith "expected allocated port")
            let url = "http://127.0.0.1:" + string port + "/hello"

            HttpClient.get url
            |> Effect.mapError (fun e -> box e)
            |> Effect.flatMap (fun response ->
                closeHandle handle
                |> Effect.map (fun () ->
                    toBe response.Status 200
                    toBe response.Body "world"))))

    itEffectIn nodeHttpContext "turns missing routes into 404 responses" (fun () ->
        let router = HttpRouter.empty

        NodeHttpServer.listenLocal 0 router
        |> Effect.mapError (fun e -> box e)
        |> Effect.flatMap (fun handle ->
            let port = handle.Port |> Option.defaultWith (fun () -> failwith "expected allocated port")
            let url = "http://127.0.0.1:" + string port + "/missing"

            HttpClient.get url
            |> Effect.mapError (fun e -> box e)
            |> Effect.flatMap (fun response ->
                closeHandle handle
                |> Effect.map (fun () ->
                    toBe response.Status 404
                    toBe response.Body "RouteNotFound (GET /missing)"))))

    itEffectIn nodeHttpContext "serves streamed response bodies over local HTTP" (fun () ->
        let router =
            HttpRouter.empty
            |> HttpRouter.add
                "GET"
                "/stream"
                (HttpServerResponse.streamBytesWith
                    { HttpServerResponseOptions.empty with ContentType = Some "application/octet-stream" }
                    (Stream.fromIterable [ [| 1uy; 2uy |]; [| 3uy |] ]))

        NodeHttpServer.listenLocal 0 router
        |> Effect.mapError (fun e -> box e)
        |> Effect.flatMap (fun handle ->
            let port = handle.Port |> Option.defaultWith (fun () -> failwith "expected allocated port")
            let url = "http://127.0.0.1:" + string port + "/stream"

            HttpClientRequest.get url
            |> HttpClient.execute
            |> Effect.mapError (fun e -> box e)
            |> Effect.flatMap (fun response ->
                match HttpClientResponse.bodyStream response with
                | Some stream ->
                    stream
                    |> Stream.runCollect
                    |> Effect.flatMap (fun chunks ->
                        closeHandle handle
                        |> Effect.map (fun () ->
                            toBe response.Status 200
                            toBe (Headers.get "content-type" response.Headers) (Some "application/octet-stream")
                            toEqual (chunks |> List.collect Array.toList) [ 1uy; 2uy; 3uy ]))
                | None ->
                    closeHandle handle
                    |> Effect.flatMap (fun () -> Effect.sync (fun () -> failwith "expected streamed response body"))))))
