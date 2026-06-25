module HttpApiScalarSpec

open Effect
open Effect.Vitest

let private run effect = Runtime.runSync Runtime.defaultRuntime effect

describe "HttpApiScalar" (fun () ->
    test "adds a configurable docs route backed by the generated OpenAPI spec" (fun () ->
        let endpoint = HttpApiEndpoint.get "hello" "/hello" HttpApiEndpoint.empty
        let api = HttpApi.make "ScalarApi" |> HttpApi.add (HttpApiGroup.make "docs" |> HttpApiGroup.add endpoint)

        let router =
            HttpRouter.empty
            |> HttpApiScalar.route
                api
                "/reference"
                (Some { ScalarConfig.empty with Theme = Some ScalarThemeId.Moon; ShowOperationId = Some true })

        let response =
            router
            |> HttpRouter.handle (HttpServerRequest.make "GET" "/reference" Headers.empty HttpBody.empty)
            |> run

        let body = HttpBody.asText response.Body |> Option.defaultValue ""

        toBe response.Status 200
        toBe (body.Contains("@scalar/api-reference")) true
        toBe (body.Contains("ScalarApi")) true
        toBe (body.Contains("\"theme\":\"moon\"")) true
        toBe (body.Contains("\"showOperationId\":true")) true))
