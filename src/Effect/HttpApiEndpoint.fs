namespace Effect

type HttpMethod =
    | GET
    | POST
    | PUT
    | PATCH
    | DELETE
    | HEAD
    | OPTIONS

type HttpApiEndpointOptions =
    { Params: obj option
      Query: obj option
      Payload: HttpApiContent list
      Headers: obj option
      Success: HttpApiContent list
      Error: HttpApiContent list
      Security: HttpApiSecurity list }

[<RequireQualifiedAccess>]
type HttpApiInput =
    | Schema of Schema<obj>

type HttpApiEndpoint =
    { Name: string
      Method: HttpMethod
      Path: string
      Options: HttpApiEndpointOptions
      Middlewares: HttpApiMiddleware list }

[<RequireQualifiedAccess>]
module HttpApiEndpoint =

    let empty =
        { Params = None
          Query = None
          Payload = []
          Headers = None
          Success = [ HttpApiSchema.noContent ]
          Error = []
          Security = [] }

    let input (schema: Schema<'T>) : obj option =
        Some(box (HttpApiInput.Schema(Schema.erase schema)))

    let paramsSchema (schema: Schema<'T>) : obj option = input schema
    let querySchema (schema: Schema<'T>) : obj option = input schema
    let headersSchema (schema: Schema<'T>) : obj option = input schema

    let tryInputSchema (value: obj option) : Schema<obj> option =
        match value with
        | Some(:? HttpApiInput as input) ->
            match input with
            | HttpApiInput.Schema schema -> Some schema
        | _ -> None

    let private methodName =
        function
        | GET -> "GET"
        | POST -> "POST"
        | PUT -> "PUT"
        | PATCH -> "PATCH"
        | DELETE -> "DELETE"
        | HEAD -> "HEAD"
        | OPTIONS -> "OPTIONS"

    let private normalizePath (path: string) =
        if path = "" then "/"
        elif path.StartsWith("/") then path
        else "/" + path

    let private prefixPath (prefix: string) (path: string) =
        let p = normalizePath prefix
        let child = normalizePath path

        if child = "/" then p
        elif p = "/" then child
        else p.TrimEnd('/') + "/" + child.TrimStart('/')

    let private validateSuccesses method_ (successes: HttpApiContent list) =
        if method_ = HEAD && successes |> List.exists HttpApiSchema.isStream then
            invalidArg "success" "HEAD endpoints cannot use streaming success schemas"

        for schema in successes do
            if HttpApiSchema.hasReservedFailureEvent schema then
                invalidArg "success" "SSE events cannot use the reserved HttpApi stream failure event name"

        let byStatus =
            successes
            |> List.groupBy (fun schema -> schema.Status)

        for status, schemas in byStatus do
            let streams = schemas |> List.filter HttpApiSchema.isStream
            let noContent = schemas |> List.exists HttpApiSchema.isNoContent
            let buffered = schemas |> List.filter (fun schema -> not (HttpApiSchema.isStream schema) && not (HttpApiSchema.isNoContent schema))

            if List.length streams > 1 then
                invalidArg "success" (sprintf "Only one streaming success schema is allowed for status %d" status)

            if not (List.isEmpty streams) && noContent then
                invalidArg "success" (sprintf "Streaming and no-content successes cannot share status %d" status)

            match streams with
            | stream :: _ ->
                let streamType = HttpApiSchema.baseContentType stream.ContentType

                for schema in buffered do
                    if HttpApiSchema.baseContentType schema.ContentType = streamType then
                        invalidArg "success" (sprintf "Streaming and buffered successes cannot share content type %s at status %d" streamType status)
            | [] -> ()

    let private make method_ name path options =
        if options.Error |> List.exists HttpApiSchema.isStream then
            invalidArg "error" "Streaming schemas are only supported as successes"

        validateSuccesses method_ options.Success

        { Name = name
          Method = method_
          Path = path
          Options = options
          Middlewares = [] }

    let get name path options = make GET name path options
    let post name path options = make POST name path options
    let put name path options = make PUT name path options
    let patch name path options = make PATCH name path options
    let delete name path options = make DELETE name path options
    let head name path options = make HEAD name path options
    let options name path options = make OPTIONS name path options

    let methodString endpoint = methodName endpoint.Method

    let prefix (prefix: string) (endpoint: HttpApiEndpoint) : HttpApiEndpoint =
        { endpoint with Path = prefixPath prefix endpoint.Path }

    let addError (error: HttpApiContent) (endpoint: HttpApiEndpoint) : HttpApiEndpoint =
        if HttpApiSchema.isStream error then
            invalidArg "error" "Streaming schemas are only supported as successes"

        { endpoint with Options = { endpoint.Options with Error = endpoint.Options.Error @ [ error ] } }

    let addErrors (errors: HttpApiContent list) (endpoint: HttpApiEndpoint) : HttpApiEndpoint =
        errors |> List.fold (fun acc error -> addError error acc) endpoint

    let middleware (middleware: HttpApiMiddleware) (endpoint: HttpApiEndpoint) : HttpApiEndpoint =
        { endpoint with
            Middlewares = endpoint.Middlewares @ [ middleware ]
            Options = { endpoint.Options with Error = endpoint.Options.Error @ middleware.Error } }

    let addSecurity (security: HttpApiSecurity) (endpoint: HttpApiEndpoint) : HttpApiEndpoint =
        { endpoint with Options = { endpoint.Options with Security = endpoint.Options.Security @ [ security ] } }

    let addSecurities (securities: HttpApiSecurity list) (endpoint: HttpApiEndpoint) : HttpApiEndpoint =
        securities |> List.fold (fun acc security -> addSecurity security acc) endpoint
