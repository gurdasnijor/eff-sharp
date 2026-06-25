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
        toBe (HttpBody.encodedText body) (Some """{"ok":true}""")))
