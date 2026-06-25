module HttpServerResponseSpec

open Effect
open Effect.Vitest

describe "HttpServerResponse" (fun () ->
    test "constructs text, html, json, and redirects" (fun () ->
        let text = HttpServerResponse.text "ok"
        let html = HttpServerResponse.html "<html></html>"
        let json = HttpServerResponse.json (JObject(Map.ofList [ "ok", JBool true ]))
        let redirect = HttpServerResponse.redirect "/login"

        toBe text.Status 200
        toBe (Headers.get "content-type" text.Headers) (Some "text/plain; charset=utf-8")
        toBe (Headers.get "content-type" html.Headers) (Some "text/html")
        toBe (Headers.get "content-type" json.Headers) (Some "application/json")
        toBe redirect.Status 302
        toBe (Headers.get "location" redirect.Headers) (Some "/login"))

    test "converts to and from client responses" (fun () ->
        let request = HttpClientRequest.get "http://localhost:3000/todos/1"

        let serverResponse =
            HttpServerResponse.jsonWith
                { HttpServerResponseOptions.empty with Status = Some 201 }
                (JObject(Map.ofList [ "foo", JString "bar" ]))
            |> HttpServerResponse.setHeader "x-test" "ok"
            |> HttpServerResponse.setCookie "session" "123"

        let clientResponse = HttpServerResponse.toClientResponse request serverResponse
        let roundTrip = HttpServerResponse.fromClientResponse clientResponse

        toBe clientResponse.Status 201
        toBe (Headers.get "x-test" clientResponse.Headers) (Some "ok")
        toBe roundTrip.Status 201
        toBe (Headers.get "content-type" roundTrip.Headers) (Some "application/json")))
