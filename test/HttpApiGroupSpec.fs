module HttpApiGroupSpec

open Effect
open Effect.Vitest

describe "HttpApiGroup" (fun () ->
    test "make starts empty and add indexes endpoints by name" (fun () ->
        let listTodos = HttpApiEndpoint.get "listTodos" "/todos" HttpApiEndpoint.empty
        let createTodo = HttpApiEndpoint.post "createTodo" "/todos" HttpApiEndpoint.empty

        let group =
            HttpApiGroup.make "todos"
            |> HttpApiGroup.addMany [ listTodos; createTodo ]

        toBe group.Identifier "todos"
        toEqual (group.Endpoints |> Map.toList |> List.map fst) [ "createTodo"; "listTodos" ])

    test "add replaces endpoints with the same name" (fun () ->
        let first = HttpApiEndpoint.get "find" "/v1/find" HttpApiEndpoint.empty
        let second = HttpApiEndpoint.get "find" "/v2/find" HttpApiEndpoint.empty

        let group =
            HttpApiGroup.make "search"
            |> HttpApiGroup.add first
            |> HttpApiGroup.add second

        toBe group.Endpoints.["find"].Path "/v2/find"))
