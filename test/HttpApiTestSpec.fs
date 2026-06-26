module HttpApiTestSpec

open Effect
open Effect.Vitest

type TestTodo = { Id: string }
type TestTodoParams = { Id: string }

let private getTodo =
    HttpApiEndpoint.get
        "getTodo"
        "/todos/:id"
        { HttpApiEndpoint.empty with Params = Some(box [ "id" ]) }

let private api =
    HttpApi.make "TodoApi"
    |> HttpApi.add (HttpApiGroup.make "todos" |> HttpApiGroup.add getTodo)

let private testTodoSchema: Schema<TestTodo> =
    Schema.object {
        let! id = Schema.field "id" Schema.string (fun todo -> todo.Id)
        return { Id = id }
    }

let private testTodoParamsSchema: Schema<TestTodoParams> =
    Schema.object {
        let! id = Schema.field "id" Schema.string (fun todo -> todo.Id)
        return { Id = id }
    }

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

    test "runs decoded HttpApiClient modes against HttpApiBuilder routes in memory" (fun () ->
        let endpoint =
            HttpApiEndpoint.get
                "getTodo"
                "/todos/:id"
                { HttpApiEndpoint.empty with
                    Params = HttpApiEndpoint.paramsSchema testTodoParamsSchema
                    Success = [ HttpApiSchema.asJson testTodoSchema ] }

        let api =
            HttpApi.make "TodoApi"
            |> HttpApi.add (HttpApiGroup.make "todos" |> HttpApiGroup.add endpoint)

        let group =
            HttpApiBuilder.group
                api
                "todos"
                (Map.ofList
                    [ "getTodo",
                      fun input ->
                          let params_ = input.ParamsValue |> Option.get |> unbox<TestTodoParams>
                          Effect.succeed (HttpServerResponse.json (JObject(Map.ofList [ "id", JString params_.Id ]))) ])

        let testClient = HttpApiTest.make api [ group ] { BaseUrl = "http://test" }

        match
            testClient
            |> HttpApiTest.endpointWithModeTyped
                "todos"
                "getTodo"
                DecodedOnly
                { HttpApiClient.emptyEndpointValueInput with Params = Some(box { Id = "42" }) }
            |> Runtime.runSync Runtime.defaultRuntime
        with
        | Decoded(DecodedBody value) -> toBe (unbox<TestTodo> value).Id "42"
        | other -> failwithf "expected decoded todo, got %A" other)

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
