module HttpApiTestSpec

open Effect
open Effect.Vitest

let private getTodo =
    HttpApiEndpoint.get
        "getTodo"
        "/todos/:id"
        { HttpApiEndpoint.empty with Params = Some(box [ "id" ]) }

let private api =
    HttpApi.make "TodoApi"
    |> HttpApi.add (HttpApiGroup.make "todos" |> HttpApiGroup.add getTodo)

describe "HttpApiTest" (fun () ->
    test "runs HttpApiClient against HttpApiBuilder routes in memory" (fun () ->
        let group =
            HttpApiBuilder.group
                api
                "todos"
                (Map.ofList
                    [ "getTodo",
                      fun input ->
                          Effect.succeed (HttpServerResponse.json (JObject(Map.ofList [ "id", JString input.Params.["id"] ]))) ])

        let testClient = HttpApiTest.make api [ group ] { BaseUrl = "http://test" }

        let response =
            testClient
            |> HttpApiTest.endpoint
                "todos"
                "getTodo"
                { HttpApiClient.emptyEndpointInput with Params = Map.ofList [ "id", "42" ] }
            |> Runtime.runSync Runtime.defaultRuntime

        toBe response.Status 200
        toBe (Headers.get "content-type" response.Headers) (Some "application/json")
        toBe (HttpBody.encodedText response.Body) (Some """{"id":"42"}"""))

    test "turns missing routes into HTTP responses" (fun () ->
        let group =
            HttpApiBuilder.group
                api
                "todos"
                (Map.ofList [ "getTodo", fun _ -> Effect.succeed (HttpServerResponse.text "ok") ])

        let testClient = HttpApiTest.make api [ group ] { BaseUrl = "http://test" }

        let response =
            testClient
            |> HttpApiTest.endpoint
                "todos"
                "getTodo"
                { HttpApiClient.emptyEndpointInput with Params = Map.ofList [ "id", "42" ] }
            |> Runtime.runSync Runtime.defaultRuntime

        toBe response.Status 200))
