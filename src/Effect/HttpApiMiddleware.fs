namespace Effect

type HttpApiMiddlewareContext =
    { Group: string
      Endpoint: string }

type HttpApiMiddleware =
    { Identifier: string
      Error: HttpApiContent list
      RequiredForClient: bool
      Apply:
        HttpApiMiddlewareContext ->
            Effect<HttpServerResponse, HttpServerError, Context> ->
                Effect<HttpServerResponse, HttpServerError, Context> }

[<RequireQualifiedAccess>]
module HttpApiMiddleware =

    let make
        (identifier: string)
        (apply:
            HttpApiMiddlewareContext ->
                Effect<HttpServerResponse, HttpServerError, Context> ->
                    Effect<HttpServerResponse, HttpServerError, Context>)
        : HttpApiMiddleware =
        { Identifier = identifier
          Error = []
          RequiredForClient = false
          Apply = apply }

    let withError (error: HttpApiContent) (middleware: HttpApiMiddleware) =
        if HttpApiSchema.isStream error then
            invalidArg "error" "Streaming schemas are only supported as successes"

        { middleware with Error = middleware.Error @ [ error ] }

    let withErrors (errors: HttpApiContent list) (middleware: HttpApiMiddleware) =
        errors |> List.fold (fun acc error -> withError error acc) middleware

    let requiredForClient (middleware: HttpApiMiddleware) =
        { middleware with RequiredForClient = true }

    let identity identifier =
        make identifier (fun _ effect -> effect)
