module NodeHttpPlatformSpec

open Effect
open Effect.Platform.Node
open Effect.Vitest

describe "NodeHttpPlatform" (fun () ->
    test "creates file responses with content headers" (fun () ->
        let response =
            NodeHttpPlatform.make.FileResponse "/tmp/index.html" 206 (Some "Partial Content") Headers.empty (Some 0) (Some 5) 5

        toBe response.Status 206
        toBe response.StatusText (Some "Partial Content")
        toBe (Headers.get "content-type" response.Headers) (Some "text/html")
        toBe (Headers.get "content-length" response.Headers) (Some "5"))

    test "preserves existing content-type headers" (fun () ->
        let response =
            NodeHttpPlatform.make.FileWebResponse
                "index.html"
                10L
                200
                None
                (Headers.ofList [ "content-type", "application/custom" ])

        toBe (Headers.get "content-type" response.Headers) (Some "application/custom")
        toBe (Headers.get "content-length" response.Headers) (Some "10")))
