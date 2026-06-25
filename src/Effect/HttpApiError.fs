namespace Effect

/// Built-in HTTP API failures for common status responses.
[<RequireQualifiedAccess>]
type HttpApiError =
    | BadRequest
    | Unauthorized
    | Forbidden
    | NotFound
    | MethodNotAllowed
    | NotAcceptable
    | RequestTimeout
    | Conflict
    | Gone
    | InternalServerError
    | NotImplemented
    | ServiceUnavailable
    | SchemaError of reason: string

[<RequireQualifiedAccess>]
module HttpApiError =

    let tag =
        function
        | HttpApiError.BadRequest -> "BadRequest"
        | HttpApiError.Unauthorized -> "Unauthorized"
        | HttpApiError.Forbidden -> "Forbidden"
        | HttpApiError.NotFound -> "NotFound"
        | HttpApiError.MethodNotAllowed -> "MethodNotAllowed"
        | HttpApiError.NotAcceptable -> "NotAcceptable"
        | HttpApiError.RequestTimeout -> "RequestTimeout"
        | HttpApiError.Conflict -> "Conflict"
        | HttpApiError.Gone -> "Gone"
        | HttpApiError.InternalServerError -> "InternalServerError"
        | HttpApiError.NotImplemented -> "NotImplemented"
        | HttpApiError.ServiceUnavailable -> "ServiceUnavailable"
        | HttpApiError.SchemaError _ -> "HttpApiSchemaError"

    let status =
        function
        | HttpApiError.BadRequest
        | HttpApiError.SchemaError _ -> 400
        | HttpApiError.Unauthorized -> 401
        | HttpApiError.Forbidden -> 403
        | HttpApiError.NotFound -> 404
        | HttpApiError.MethodNotAllowed -> 405
        | HttpApiError.NotAcceptable -> 406
        | HttpApiError.RequestTimeout -> 408
        | HttpApiError.Conflict -> 409
        | HttpApiError.Gone -> 410
        | HttpApiError.InternalServerError -> 500
        | HttpApiError.NotImplemented -> 501
        | HttpApiError.ServiceUnavailable -> 503

    let message =
        function
        | HttpApiError.SchemaError reason -> reason
        | error -> tag error

    let toResponse (error: HttpApiError) : HttpServerResponse =
        HttpServerResponse.emptyWith
            { HttpServerResponseOptions.empty with
                Status = Some(status error) }

    let schema (error: HttpApiError) : HttpApiContent =
        HttpApiSchema.empty (status error)

    let badRequest = schema HttpApiError.BadRequest
    let unauthorized = schema HttpApiError.Unauthorized
    let forbidden = schema HttpApiError.Forbidden
    let notFound = schema HttpApiError.NotFound
    let methodNotAllowed = schema HttpApiError.MethodNotAllowed
    let notAcceptable = schema HttpApiError.NotAcceptable
    let requestTimeout = schema HttpApiError.RequestTimeout
    let conflict = schema HttpApiError.Conflict
    let gone = schema HttpApiError.Gone
    let internalServerError = schema HttpApiError.InternalServerError
    let notImplemented = schema HttpApiError.NotImplemented
    let serviceUnavailable = schema HttpApiError.ServiceUnavailable
