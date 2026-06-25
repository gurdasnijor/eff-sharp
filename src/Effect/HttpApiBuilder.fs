namespace Effect

type HttpApiServerRequest =
    { Request: HttpServerRequest
      Params: Map<string, string>
      Query: UrlParams
      Headers: Headers
      Payload: HttpBody }

type HttpApiEndpointHandler = HttpApiServerRequest -> Effect<HttpServerResponse, HttpServerError, Context>

type HttpApiGroupHandlers =
    { Group: HttpApiGroup
      Handlers: Map<string, HttpApiEndpointHandler> }

type HttpApiHandlers =
    { Api: HttpApi
      Groups: Map<string, HttpApiGroupHandlers> }

[<RequireQualifiedAccess>]
module HttpApiBuilder =

    let private endpointInput (request: HttpServerRequest) (parameters: Map<string, string>) =
        { Request = request
          Params = parameters
          Query = HttpServerRequest.searchParams request
          Headers = request.Headers
          Payload = request.Body }

    let group (api: HttpApi) (groupName: string) (handlers: Map<string, HttpApiEndpointHandler>) : HttpApiGroupHandlers =
        match Map.tryFind groupName api.Groups with
        | None -> invalidArg "groupName" ("Unknown HttpApi group: " + groupName)
        | Some group ->
            for endpointName in handlers |> Map.toSeq |> Seq.map fst do
                if not (Map.containsKey endpointName group.Endpoints) then
                    invalidArg "handlers" ("Unknown HttpApi endpoint: " + groupName + "." + endpointName)

            { Group = group
              Handlers = handlers }

    let handlers (api: HttpApi) (groups: HttpApiGroupHandlers list) : HttpApiHandlers =
        { Api = api
          Groups = groups |> List.map (fun group -> group.Group.Identifier, group) |> Map.ofList }

    let private applyMiddlewares (group: HttpApiGroup) (endpoint: HttpApiEndpoint) (effect: Effect<HttpServerResponse, HttpServerError, Context>) =
        let context =
            { Group = group.Identifier
              Endpoint = endpoint.Name }

        endpoint.Middlewares
        |> List.fold (fun acc middleware -> middleware.Apply context acc) effect

    let private routeFor (group: HttpApiGroup) (endpoint: HttpApiEndpoint) (handler: HttpApiEndpointHandler) : HttpRoute =
        HttpRouter.routeHandler
            (HttpApiEndpoint.methodString endpoint)
            endpoint.Path
            (fun request parameters -> handler (endpointInput request parameters) |> applyMiddlewares group endpoint)

    let toRouter (handlers: HttpApiHandlers) : HttpRouter =
        handlers.Api.Groups
        |> Map.toList
        |> List.fold
            (fun router (groupName, group) ->
                match Map.tryFind groupName handlers.Groups with
                | None -> invalidArg "handlers" ("Missing HttpApi group handlers: " + groupName)
                | Some groupHandlers ->
                    group.Endpoints
                    |> Map.toList
                    |> List.fold
                        (fun router (endpointName, endpoint) ->
                            match Map.tryFind endpointName groupHandlers.Handlers with
                            | None -> invalidArg "handlers" ("Missing HttpApi endpoint handler: " + groupName + "." + endpointName)
                            | Some handler -> HttpRouter.addRoute (routeFor group endpoint handler) router)
                        router)
            HttpRouter.empty

    let route (api: HttpApi) (groups: HttpApiGroupHandlers list) : HttpRouter =
        handlers api groups |> toRouter
