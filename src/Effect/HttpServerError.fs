namespace Effect

/// Server-side HTTP failures produced before a response is available.
type HttpServerError =
    | RouteNotFound of request: HttpServerRequest
    | RequestParseError of request: HttpServerRequest * reason: string
    | InternalServerError of reason: string

[<RequireQualifiedAccess>]
module HttpServerError =

    let status (error: HttpServerError) : int =
        match error with
        | RouteNotFound _ -> 404
        | RequestParseError _ -> 400
        | InternalServerError _ -> 500

    let message (error: HttpServerError) : string =
        match error with
        | RouteNotFound request -> "RouteNotFound (" + request.Method + " " + request.Url + ")"
        | RequestParseError(_, reason) -> "RequestParseError (" + reason + ")"
        | InternalServerError reason -> "InternalServerError (" + reason + ")"
