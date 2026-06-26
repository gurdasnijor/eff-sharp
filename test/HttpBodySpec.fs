module HttpBodySpec

open Effect
open Effect.Vitest

describe "HttpBody" (fun () ->
    test "tracks text content metadata" (fun () ->
        let body = HttpBody.text "hello"

        toBe (HttpBody.contentType body) (Some "text/plain; charset=utf-8")
        toBe (HttpBody.contentLength body) (Some 5)
        toBe (HttpBody.asText body) (Some "hello"))

    test "tracks json content type" (fun () ->
        let body = HttpBody.json (JObject(Map.ofList [ "ok", JBool true ]))

        toBe (HttpBody.contentType body) (Some "application/json")
        toBe (HttpBody.asText body) None
        toBe (HttpBody.encodedText body) (Some """{"ok":true}"""))

    itEffect "tracks stream body metadata" (fun () ->
        let stream = Stream.fromIterable [ [| 1uy; 2uy |]; [| 3uy |] ]
        let body = HttpBody.streamBytesWithContentType "application/octet-stream" stream

        match HttpBody.asStream body with
        | Some bodyStream ->
            bodyStream
            |> Stream.runCollect
            |> Effect.map (fun chunks ->
                toBe (HttpBody.isStream body) true
                toBe (HttpBody.contentType body) (Some "application/octet-stream")
                toBe (HttpBody.contentLength body) None
                toBe (HttpBody.asText body) None
                toEqual (chunks |> List.map Array.toList) [ [ 1uy; 2uy ]; [ 3uy ] ])
        | None -> Effect.sync (fun () -> failwith "expected stream body")))
