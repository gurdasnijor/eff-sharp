namespace Effect

type HttpApiTestClient =
    { Client: HttpApiClient
      Context: Context
      Router: HttpRouter }

[<RequireQualifiedAccess>]
module HttpApiTest =

    let private serverErrorResponse (error: HttpServerError) =
        HttpServerResponse.textWith
            { HttpServerResponseOptions.empty with Status = Some(HttpServerError.status error) }
            (HttpServerError.message error)

    let private fakeHttpClient (router: HttpRouter) : HttpClient =
        let execute (request: HttpClientRequest) =
            let serverRequest = HttpServerRequest.fromClientRequest request

            HttpRouter.handle serverRequest router
            |> Effect.catchAll (serverErrorResponse >> Effect.succeed)
            |> Effect.map (HttpServerResponse.toClientResponse request)

        { Execute = execute
          Request =
            fun method url ->
                HttpClientRequest.make method url
                |> execute
                |> Effect.map (fun response ->
                    { Status = response.Status
                      Body = HttpClientResponse.bodyText response |> Option.defaultValue "" }) }

    let make (api: HttpApi) (groups: HttpApiGroupHandlers list) (options: HttpApiClientUrlOptions) : HttpApiTestClient =
        let router = HttpApiBuilder.route api groups
        let httpClient = fakeHttpClient router

        { Client = HttpApiClient.make api options
          Context = Context.make HttpClient.tag httpClient
          Router = router }

    let endpoint
        (group: string)
        (endpoint: string)
        (input: HttpApiClientEndpointInput)
        (testClient: HttpApiTestClient)
        : Effect<HttpClientResponse, HttpClientError, Context> =
        testClient.Client
        |> HttpApiClient.endpoint group endpoint input
        |> Effect.provideContext testClient.Context

    let endpointWithMode
        (group: string)
        (endpoint: string)
        (mode: HttpApiClientResponseMode)
        (input: HttpApiClientEndpointInput)
        (testClient: HttpApiTestClient)
        : Effect<HttpApiClientResponse, HttpApiClientError, Context> =
        testClient.Client
        |> HttpApiClient.endpointWithMode group endpoint mode input
        |> Effect.provideContext testClient.Context

    let topLevelEndpointWithMode
        (endpoint: string)
        (mode: HttpApiClientResponseMode)
        (input: HttpApiClientEndpointInput)
        (testClient: HttpApiTestClient)
        : Effect<HttpApiClientResponse, HttpApiClientError, Context> =
        testClient.Client
        |> HttpApiClient.topLevelEndpointWithMode endpoint mode input
        |> Effect.provideContext testClient.Context
