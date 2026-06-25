module HttpClientSpec

open Effect
open Effect.Vitest

describe "HttpClient" (fun () ->
    test "legacy request helpers dispatch to the service" (fun () ->
        let calls = ResizeArray<string * string>()

        let client =
            { Request =
                fun method url ->
                    Effect.sync (fun () ->
                        calls.Add(method, url)
                        { Status = 201
                          Body = method + " " + url })
              Execute =
                fun request ->
                    Effect.succeed (HttpClientResponse.text request 200 Headers.empty "unused") }

        let ctx = Context.make HttpClient.tag client
        let getResponse = Runtime.runSync (Runtime.make ctx) (HttpClient.get "https://example.com/get")
        let postResponse = Runtime.runSync (Runtime.make ctx) (HttpClient.post "https://example.com/post")

        toBe getResponse.Status 201
        toBe getResponse.Body "GET https://example.com/get"
        toBe postResponse.Body "POST https://example.com/post"
        toEqual (List.ofSeq calls) [ "GET", "https://example.com/get"; "POST", "https://example.com/post" ])

    test "execute dispatches structured requests to the service" (fun () ->
        let mutable captured: HttpClientRequest option = None

        let client =
            { Request =
                fun _ _ ->
                    Effect.succeed
                        { Status = 200
                          Body = "" }
              Execute =
                fun request ->
                    captured <- Some request
                    Effect.succeed (HttpClientResponse.text request 202 (Headers.ofList [ "x-test", "ok" ]) "accepted") }

        let request =
            HttpClientRequest.post "https://example.com/items"
            |> HttpClientRequest.setUrlParam "page" "1"
            |> HttpClientRequest.bodyText "payload"

        let response = Runtime.runSync (Runtime.make (Context.make HttpClient.tag client)) (HttpClient.execute request)

        toBe response.Status 202
        toBe (HttpClientResponse.bodyText response) (Some "accepted")
        toBe (Headers.get "x-test" response.Headers) (Some "ok")

        match captured with
        | Some request ->
            toBe request.Method "POST"
            toBe (HttpClientRequest.urlWithParams request) "https://example.com/items?page=1"
            toBe (HttpBody.asText request.Body) (Some "payload")
        | None -> failwith "expected captured request"))
