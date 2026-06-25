module HttpRouterSpec

open Effect
open Effect.Vitest

let private run (effect: Effect<'A, 'E, Context>) = Runtime.runSync Runtime.defaultRuntime effect

let private request method url = HttpServerRequest.make method url Headers.empty HttpBody.empty

describe "HttpRouter" (fun () ->
    test "matches exact routes and path params" (fun () ->
        let router =
            HttpRouter.empty
            |> HttpRouter.addHandler
                "GET"
                "/todos/:id"
                (fun _ params_ -> Effect.succeed (HttpServerResponse.text params_.["id"]))

        let response = run (HttpRouter.handle (request "GET" "/todos/42") router)

        toBe response.Status 200
        toBe (HttpBody.asText response.Body) (Some "42"))

    test "uses GET route for HEAD requests" (fun () ->
        let router = HttpRouter.empty |> HttpRouter.add "GET" "/health" (HttpServerResponse.empty)
        let response = run (HttpRouter.handle (request "HEAD" "/health") router)

        toBe response.Status 204)

    test "supports wildcard and prefix routes" (fun () ->
        let child =
            HttpRouter.empty
            |> HttpRouter.add "GET" "/" (HttpServerResponse.text "root")
            |> HttpRouter.add "GET" "/assets/*" (HttpServerResponse.text "asset")
            |> HttpRouter.prefixed "/app"

        let root = run (HttpRouter.handle (request "GET" "/app") child)
        let asset = run (HttpRouter.handle (request "GET" "/app/assets/main.css") child)

        toBe (HttpBody.asText root.Body) (Some "root")
        toBe (HttpBody.asText asset.Body) (Some "asset"))

    test "returns RouteNotFound for missing routes" (fun () ->
        let router = HttpRouter.empty

        match Runtime.runSyncExit Runtime.defaultRuntime (HttpRouter.handle (request "POST" "/missing") router) with
        | Failure cause ->
            match Cause.failures cause with
            | RouteNotFound req :: _ ->
                toBe req.Method "POST"
                toBe (HttpServerError.status (RouteNotFound req)) 404
            | other -> failwithf "expected RouteNotFound, got %A" other
        | Success response -> failwithf "expected route failure, got %A" response))
