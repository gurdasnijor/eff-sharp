module HttpServerRequestSpec

open Effect
open Effect.Vitest

describe "HttpServerRequest" (fun () ->
    test "fromClientRequest preserves metadata and parses cookies/search params" (fun () ->
        let clientRequest =
            HttpClientRequest.post "http://localhost:3000/items?existing=1"
            |> HttpClientRequest.appendUrlParam "page" "2"
            |> HttpClientRequest.setHash "top"
            |> HttpClientRequest.setHeader "cookie" "session=123; theme=dark"
            |> HttpClientRequest.bodyJson (JObject(Map.ofList [ "ok", JBool true ]))

        let request = HttpServerRequest.fromClientRequest clientRequest

        toBe request.Method "POST"
        toBe request.OriginalUrl "http://localhost:3000/items?existing=1&page=2#top"
        toBe request.Url "/items?existing=1&page=2#top"
        toBe (Map.tryFind "session" request.Cookies) (Some "123")
        toEqual (HttpServerRequest.searchParams request) [ "existing", "1"; "page", "2" ])

    test "toClientRequest round-trips url params, hash, headers, and body" (fun () ->
        let serverRequest =
            HttpServerRequest.make
                "POST"
                "http://localhost:3000/todos/1?a=1&a=2#top"
                (Headers.ofList [ "Content-Type", "application/json"; "x-test", "ok" ])
                (HttpBody.textWithContentType "application/json" """{"foo":"bar"}""")

        let clientRequest = HttpServerRequest.toClientRequest serverRequest

        toBe clientRequest.Method "POST"
        toBe clientRequest.Url "http://localhost:3000/todos/1"
        toBe clientRequest.Hash (Some "top")
        toEqual clientRequest.UrlParams [ "a", "1"; "a", "2" ]
        toBe (Headers.get "content-type" clientRequest.Headers) (Some "application/json")
        toBe (HttpBody.asText clientRequest.Body) (Some """{"foo":"bar"}"""))

    test "modify distinguishes unchanged and explicit missing remote address" (fun () ->
        let request =
            HttpServerRequest.make "GET" "http://example.com" Headers.empty HttpBody.empty
            |> HttpServerRequest.modify None None (Some(Some "127.0.0.1"))

        toBe request.RemoteAddress (Some "127.0.0.1")
        toBe (request |> HttpServerRequest.modify None None None).RemoteAddress (Some "127.0.0.1")
        toBe (request |> HttpServerRequest.modify None None (Some None)).RemoteAddress None))
