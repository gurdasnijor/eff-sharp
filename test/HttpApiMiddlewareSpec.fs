module HttpApiMiddlewareSpec

open Effect
open Effect.Vitest

let private run effect = Runtime.runSync Runtime.defaultRuntime effect

describe "HttpApiMiddleware" (fun () ->
    test "middleware wraps HttpApiBuilder route effects and contributes error metadata" (fun () ->
        let middleware =
            HttpApiMiddleware.make
                "x-powered-by"
                (fun context effect ->
                    effect
                    |> Effect.map (fun response ->
                        response
                        |> HttpServerResponse.setHeader "x-powered-by" (context.Group + "." + context.Endpoint)))
            |> HttpApiMiddleware.withError HttpApiError.unauthorized

        let endpoint =
            HttpApiEndpoint.get "hello" "/hello" HttpApiEndpoint.empty
            |> HttpApiEndpoint.middleware middleware

        let api = HttpApi.make "Api" |> HttpApi.add (HttpApiGroup.make "greetings" |> HttpApiGroup.add endpoint)

        let group =
            HttpApiBuilder.group
                api
                "greetings"
                (Map.ofList [ "hello", fun _ -> Effect.succeed (HttpServerResponse.text "hi") ])

        let response =
            HttpApiBuilder.route api [ group ]
            |> HttpRouter.handle (HttpServerRequest.make "GET" "/hello" Headers.empty HttpBody.empty)
            |> run

        toBe (Headers.get "x-powered-by" response.Headers) (Some "greetings.hello")
        toEqual api.Groups.["greetings"].Endpoints.["hello"].Options.Error [ HttpApiError.unauthorized ])

    test "group and api middleware apply to every endpoint" (fun () ->
        let middleware =
            HttpApiMiddleware.identity "auth"
            |> HttpApiMiddleware.requiredForClient
            |> HttpApiMiddleware.withError HttpApiError.forbidden

        let api =
            HttpApi.make "Api"
            |> HttpApi.add
                (HttpApiGroup.make "todos"
                 |> HttpApiGroup.addMany
                     [ HttpApiEndpoint.get "list" "/todos" HttpApiEndpoint.empty
                       HttpApiEndpoint.get "get" "/todos/:id" HttpApiEndpoint.empty ])
            |> HttpApi.middleware middleware

        let errors =
            api.Groups.["todos"].Endpoints
            |> Map.toList
            |> List.map (fun (_, endpoint) -> endpoint.Middlewares.Head.RequiredForClient, endpoint.Options.Error.Head.Status)

        toEqual errors [ true, 403; true, 403 ]))
