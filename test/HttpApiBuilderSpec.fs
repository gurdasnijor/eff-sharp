module HttpApiBuilderSpec

open Effect
open Effect.Vitest

let private getTodo =
    HttpApiEndpoint.get
        "getTodo"
        "/todos/:id"
        { HttpApiEndpoint.empty with Params = Some(box [ "id" ]) }

let private createTodo =
    HttpApiEndpoint.post
        "createTodo"
        "/todos"
        { HttpApiEndpoint.empty with Payload = [ HttpApiSchema.asText Schema.string ] }

let private api =
    HttpApi.make "TodoApi"
    |> HttpApi.add (HttpApiGroup.make "todos" |> HttpApiGroup.addMany [ getTodo; createTodo ])

let private run effect = Runtime.runSync Runtime.defaultRuntime effect

describe "HttpApiBuilder" (fun () ->
    test "turns handlers into router routes with params and query" (fun () ->
        let group =
            HttpApiBuilder.group
                api
                "todos"
                (Map.ofList
                    [ "getTodo",
                      fun input ->
                          let id = input.Params.["id"]
                          let include_ = UrlParams.getFirst "include" input.Query |> Option.defaultValue "none"
                          Effect.succeed (HttpServerResponse.text (id + ":" + include_))
                      "createTodo",
                      fun input ->
                          let payload = HttpBody.asText input.Payload |> Option.defaultValue ""
                          Effect.succeed (HttpServerResponse.textWith { HttpServerResponseOptions.empty with Status = Some 201 } payload) ])

        let router = HttpApiBuilder.route api [ group ]

        let getResponse =
            router
            |> HttpRouter.handle (HttpServerRequest.make "GET" "/todos/42?include=comments" Headers.empty HttpBody.empty)
            |> run

        let postResponse =
            router
            |> HttpRouter.handle
                (HttpServerRequest.make
                    "POST"
                    "/todos"
                    (Headers.ofList [ "content-type", "text/plain" ])
                    (HttpBody.text "new todo"))
            |> run

        toBe (HttpBody.asText getResponse.Body) (Some "42:comments")
        toBe postResponse.Status 201
        toBe (HttpBody.asText postResponse.Body) (Some "new todo"))

    test "fails fast when endpoint handlers are missing" (fun () ->
        let group =
            HttpApiBuilder.group
                api
                "todos"
                (Map.ofList [ "getTodo", fun _ -> Effect.succeed (HttpServerResponse.text "ok") ])

        toThrow (fun () -> HttpApiBuilder.route api [ group ] |> ignore)))
