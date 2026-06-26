namespace Effect

type HttpRouteHandler = HttpServerRequest -> Map<string, string> -> Effect<HttpServerResponse, HttpServerError, Context>

type HttpRoute =
    { Method: string
      Path: string
      Handler: HttpRouteHandler }

type HttpRouter = { Routes: HttpRoute list }

[<RequireQualifiedAccess>]
module HttpRouter =

    let tag: Tag<HttpRouter> = Tag.make "Effect.HttpRouter"

    let empty: HttpRouter = { Routes = [] }

    let private normalizeMethod (method: string) = if method = "*" then "*" else method.ToUpperInvariant()

    let private normalizePath (path: string) =
        if path = "" then
            "/"
        elif path = "*" then
            "*"
        elif path.StartsWith("/") then
            path
        else
            "/" + path

    let routeHandler (method: string) (path: string) (handler: HttpRouteHandler) : HttpRoute =
        { Method = normalizeMethod method
          Path = normalizePath path
          Handler = handler }

    let routeEffect
        (method: string)
        (path: string)
        (handler: Effect<HttpServerResponse, HttpServerError, Context>)
        : HttpRoute =
        routeHandler method path (fun _ _ -> handler)

    let route (method: string) (path: string) (response: HttpServerResponse) : HttpRoute =
        routeEffect method path (Effect.succeed response)

    let addRoute (route: HttpRoute) (router: HttpRouter) : HttpRouter =
        { router with Routes = router.Routes @ [ route ] }

    let addHandler (method: string) (path: string) (handler: HttpRouteHandler) (router: HttpRouter) : HttpRouter =
        addRoute (routeHandler method path handler) router

    let add (method: string) (path: string) (response: HttpServerResponse) (router: HttpRouter) : HttpRouter =
        addRoute (route method path response) router

    let addAll (routes: HttpRoute list) (router: HttpRouter) : HttpRouter =
        routes |> List.fold (fun acc route -> addRoute route acc) router

    let private prefixPath (prefix: string) (path: string) =
        let p = normalizePath prefix
        let child = normalizePath path

        if child = "/" then
            p
        elif p = "/" then
            child
        else
            p.TrimEnd('/') + "/" + child.TrimStart('/')

    let prefixed (prefix: string) (router: HttpRouter) : HttpRouter =
        { Routes = router.Routes |> List.map (fun route -> { route with Path = prefixPath prefix route.Path }) }

    let private routeMethodMatches requestMethod routeMethod =
        routeMethod = "*" || routeMethod = requestMethod || (requestMethod = "HEAD" && routeMethod = "GET")

    let private splitPath (path: string) =
        path.Trim('/').Split('/')
        |> Array.toList
        |> List.filter (fun part -> part <> "")

    let private tryMatchPath (routePath: string) (requestPath: string) : Map<string, string> option =
        if routePath = "*" then
            Some Map.empty
        elif routePath.EndsWith("/*") then
            let prefix = routePath.Substring(0, routePath.Length - 2)

            if requestPath = prefix || requestPath.StartsWith(prefix.TrimEnd('/') + "/") then
                Some Map.empty
            else
                None
        else
            let routeParts = splitPath routePath
            let requestParts = splitPath requestPath

            if routeParts.Length <> requestParts.Length then
                None
            else
                let mutable failed = false
                let mutable parameters = Map.empty

                for routePart, requestPart in List.zip routeParts requestParts do
                    if routePart.StartsWith(":") then
                        parameters <- Map.add (routePart.Substring(1)) requestPart parameters
                    elif routePart <> requestPart then
                        failed <- true

                if failed then None else Some parameters

    let find (request: HttpServerRequest) (router: HttpRouter) : (HttpRoute * Map<string, string>) option =
        let requestMethod = request.Method.ToUpperInvariant()
        let requestPath = HttpServerRequest.path request

        router.Routes
        |> List.tryPick (fun route ->
            if routeMethodMatches requestMethod route.Method then
                tryMatchPath route.Path requestPath |> Option.map (fun parameters -> route, parameters)
            else
                None)

    let handle (request: HttpServerRequest) (router: HttpRouter) : Effect<HttpServerResponse, HttpServerError, Context> =
        match find request router with
        | Some(route, parameters) -> route.Handler request parameters
        | None -> Effect.fail (RouteNotFound request)
