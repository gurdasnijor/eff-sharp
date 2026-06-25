module HttpClientRequestSpec

open Effect
open Effect.Vitest

describe "HttpClientRequest" (fun () ->
    test "builds method, params, hash, headers, and body" (fun () ->
        let req =
            HttpClientRequest.post "/items"
            |> HttpClientRequest.prependUrl "https://api.example.com/v1"
            |> HttpClientRequest.appendUrlParam "q" "hello world"
            |> HttpClientRequest.setHash "section"
            |> HttpClientRequest.acceptJson
            |> HttpClientRequest.bodyText "payload"

        toBe req.Method "POST"
        toBe (HttpClientRequest.urlWithParams req) "https://api.example.com/v1/items?q=hello+world#section"
        toBe (Headers.get "accept" req.Headers) (Some "application/json")
        toBe (Headers.get "content-type" req.Headers) (Some "text/plain; charset=utf-8")
        toBe (HttpBody.asText req.Body) (Some "payload"))

    test "normalizes custom methods" (fun () ->
        let req = HttpClientRequest.make "propfind" "/resource"
        toBe req.Method "PROPFIND"))
