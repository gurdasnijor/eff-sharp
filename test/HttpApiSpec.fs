module HttpApiSpec

open Effect
open Effect.Vitest

describe "HttpApi" (fun () ->
    test "make starts empty and add indexes groups by identifier" (fun () ->
        let users = HttpApiGroup.make "users"
        let admin = HttpApiGroup.make "admin"

        let api =
            HttpApi.make "Service"
            |> HttpApi.add users
            |> HttpApi.add admin

        toBe api.Identifier "Service"
        toEqual (api.Groups |> Map.toList |> List.map fst) [ "admin"; "users" ])

    test "addMany preserves the last group for duplicate identifiers" (fun () ->
        let first =
            HttpApiGroup.make "users"
            |> HttpApiGroup.add (HttpApiEndpoint.get "first" "/first" HttpApiEndpoint.empty)

        let second =
            HttpApiGroup.make "users"
            |> HttpApiGroup.add (HttpApiEndpoint.get "second" "/second" HttpApiEndpoint.empty)

        let api = HttpApi.make "Service" |> HttpApi.addMany [ first; second ]

        toBe (api.Groups.["users"].Endpoints |> Map.containsKey "second") true
        toBe (api.Groups.["users"].Endpoints |> Map.containsKey "first") false)

    test "prefix and addErrors apply across every group" (fun () ->
        let api =
            HttpApi.make "Service"
            |> HttpApi.add
                (HttpApiGroup.make "users"
                 |> HttpApiGroup.add (HttpApiEndpoint.get "getUser" "/users/:id" HttpApiEndpoint.empty))
            |> HttpApi.prefix "/v1"
            |> HttpApi.addErrors [ HttpApiError.notFound; HttpApiError.internalServerError ]

        let endpoint = api.Groups.["users"].Endpoints.["getUser"]

        toBe endpoint.Path "/v1/users/:id"
        toEqual (endpoint.Options.Error |> List.map (fun error -> error.Status)) [ 404; 500 ]))
