module HttpApiSwaggerSpec

open Effect
open Effect.Vitest

let private run effect = Runtime.runSync Runtime.defaultRuntime effect

describe "HttpApiSwagger" (fun () ->
    test "adds a docs route backed by the generated OpenAPI spec" (fun () ->
        let endpoint = HttpApiEndpoint.get "hello" "/hello" HttpApiEndpoint.empty
        let api = HttpApi.make "DocsApi" |> HttpApi.add (HttpApiGroup.make "docs" |> HttpApiGroup.add endpoint)
        let router = HttpApiSwagger.routeDefault api HttpRouter.empty

        let response =
            router
            |> HttpRouter.handle (HttpServerRequest.make "GET" "/docs" Headers.empty HttpBody.empty)
            |> run

        let body = HttpBody.asText response.Body |> Option.defaultValue ""

        toBe response.Status 200
        toBe (Headers.get "content-type" response.Headers |> Option.exists (fun value -> value.StartsWith("text/html"))) true
        toBe (body.Contains("SwaggerUIBundle")) true
        toBe (body.Contains("DocsApi")) true))
