module HttpClientResponseSpec

open Effect
open Effect.Vitest

describe "HttpClientResponse" (fun () ->
    test "classifies response statuses" (fun () ->
        let req = HttpClientRequest.get "/"

        toBe (HttpClientResponse.isSuccess (HttpClientResponse.text req 204 Headers.empty "")) true
        toBe (HttpClientResponse.isClientError (HttpClientResponse.text req 404 Headers.empty "missing")) true
        toBe (HttpClientResponse.isServerError (HttpClientResponse.text req 503 Headers.empty "down")) true)

    test "uses response content-type for text bodies" (fun () ->
        let req = HttpClientRequest.get "/"
        let headers = Headers.ofList [ "Content-Type", "text/html" ]
        let response = HttpClientResponse.text req 200 headers "<html></html>"

        toBe (HttpBody.contentType response.Body) (Some "text/html")
        toBe (HttpClientResponse.bodyText response) (Some "<html></html>")))
