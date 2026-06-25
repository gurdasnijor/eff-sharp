module HttpApiClientSpec

open Effect
open Effect.Vitest

let private getUser =
    HttpApiEndpoint.get
        "getUser"
        "/users/:id"
        { HttpApiEndpoint.empty with
            Params = Some(box [ "id" ])
            Query = Some(box [ "page"; "tags" ]) }

let private health = HttpApiEndpoint.get "health" "/health" HttpApiEndpoint.empty

let private api =
    HttpApi.make "Api"
    |> HttpApi.add (HttpApiGroup.make "users" |> HttpApiGroup.addMany [ getUser; health ])

describe "HttpApiClient.urlBuilder" (fun () ->
    test "builds urls from endpoint path params and query params" (fun () ->
        let builder = HttpApiClient.urlBuilder api { BaseUrl = "https://api.example.com" }

        let url =
            builder
            |> HttpApiClient.endpointUrl
                "users"
                "getUser"
                { Params = Map.ofList [ "id", "123" ]
                  Query = UrlParams.ofList [ "page", "1"; "tags", "1"; "tags", "2" ] }

        toBe url "https://api.example.com/users/123?page=1&tags=1&tags=2")

    test "encodes path parameters" (fun () ->
        let endpoint =
            HttpApiEndpoint.get
                "listResources"
                "/state/stacks/:stack/stages/:stage/resources"
                { HttpApiEndpoint.empty with Params = Some(box [ "stack"; "stage" ]) }

        let api = HttpApi.make "Api" |> HttpApi.add (HttpApiGroup.make "stacks" |> HttpApiGroup.add endpoint)
        let builder = HttpApiClient.urlBuilder api { BaseUrl = "https://api.example.com/" }

        let url =
            builder
            |> HttpApiClient.endpointUrl
                "stacks"
                "listResources"
                { Params = Map.ofList [ "stack", "a/b"; "stage", "prod/blue" ]
                  Query = UrlParams.empty }

        toBe url "https://api.example.com/state/stacks/a%2Fb/stages/prod%2Fblue/resources")

    test "omits missing optional path parameters" (fun () ->
        let endpoint =
            HttpApiEndpoint.get
                "download"
                "/files/:path?"
                { HttpApiEndpoint.empty with Params = Some(box [ "path" ]) }

        let api = HttpApi.make "Api" |> HttpApi.add (HttpApiGroup.make "files" |> HttpApiGroup.add endpoint)
        let builder = HttpApiClient.urlBuilder api { BaseUrl = "https://api.example.com" }

        let missing =
            builder
            |> HttpApiClient.endpointUrl
                "files"
                "download"
                { Params = Map.empty
                  Query = UrlParams.empty }

        let present =
            builder
            |> HttpApiClient.endpointUrl
                "files"
                "download"
                { Params = Map.ofList [ "path", "a/b" ]
                  Query = UrlParams.empty }

        toBe missing "https://api.example.com/files"
        toBe present "https://api.example.com/files/a%2Fb")

    test "throws for missing required path parameters" (fun () ->
        let builder = HttpApiClient.urlBuilder api { BaseUrl = "https://api.example.com" }

        toThrow (fun () ->
            builder
            |> HttpApiClient.endpointUrl
                "users"
                "getUser"
                { Params = Map.empty
                  Query = UrlParams.empty }
            |> ignore))

    test "executes endpoints through HttpClient" (fun () ->
        let mutable captured: HttpClientRequest option = None

        let httpClient =
            { Request =
                fun method url ->
                    Effect.succeed
                        { Status = 200
                          Body = method + " " + url }
              Execute =
                fun request ->
                    captured <- Some request
                    Effect.succeed (HttpClientResponse.text request 200 Headers.empty "ok") }

        let client = HttpApiClient.make api { BaseUrl = "https://api.example.com" }

        let response =
            client
            |> HttpApiClient.endpoint
                "users"
                "getUser"
                { HttpApiClient.emptyEndpointInput with
                    Params = Map.ofList [ "id", "123" ]
                    Query = UrlParams.ofList [ "page", "1" ]
                    Headers = Headers.ofList [ "x-test", "ok" ] }
            |> Runtime.runSync (Runtime.make (Context.make HttpClient.tag httpClient))

        toBe response.Status 200

        match captured with
        | Some request ->
            toBe request.Method "GET"
            toBe request.Url "https://api.example.com/users/123?page=1"
            toBe (Headers.get "x-test" request.Headers) (Some "ok")
        | None -> failwith "expected request capture"))
