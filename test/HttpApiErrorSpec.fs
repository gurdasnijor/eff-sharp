module HttpApiErrorSpec

open Effect
open Effect.Vitest

describe "HttpApiError" (fun () ->
    test "maps built-in errors to status codes and no-content schemas" (fun () ->
        toBe (HttpApiError.status HttpApiError.BadRequest) 400
        toBe (HttpApiError.status HttpApiError.Unauthorized) 401
        toBe (HttpApiError.status HttpApiError.Forbidden) 403
        toBe (HttpApiError.status HttpApiError.NotFound) 404
        toBe (HttpApiError.status HttpApiError.MethodNotAllowed) 405
        toBe (HttpApiError.status HttpApiError.NotAcceptable) 406
        toBe (HttpApiError.status HttpApiError.RequestTimeout) 408
        toBe (HttpApiError.status HttpApiError.Conflict) 409
        toBe (HttpApiError.status HttpApiError.Gone) 410
        toBe (HttpApiError.status HttpApiError.InternalServerError) 500
        toBe (HttpApiError.status HttpApiError.NotImplemented) 501
        toBe (HttpApiError.status HttpApiError.ServiceUnavailable) 503
        toBe HttpApiError.notFound.Status 404
        toBe (HttpApiSchema.isNoContent HttpApiError.notFound) true)

    test "schema errors render as bad requests with the provided message" (fun () ->
        let error = HttpApiError.SchemaError "invalid payload"
        let response = HttpApiError.toResponse error

        toBe (HttpApiError.tag error) "HttpApiSchemaError"
        toBe (HttpApiError.message error) "invalid payload"
        toBe response.Status 400))
