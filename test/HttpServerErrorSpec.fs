module HttpServerErrorSpec

open Effect
open Effect.Vitest

let private request method url = HttpServerRequest.make method url Headers.empty HttpBody.empty

describe "HttpServerError" (fun () ->
    test "maps server errors to status codes" (fun () ->
        toBe (HttpServerError.status (RouteNotFound(request "GET" "/missing"))) 404
        toBe (HttpServerError.status (RequestParseError(request "POST" "/bad", "invalid body"))) 400
        toBe (HttpServerError.status (InternalServerError "boom")) 500)

    test "formats server error messages" (fun () ->
        toBe (HttpServerError.message (RouteNotFound(request "POST" "/missing"))) "RouteNotFound (POST /missing)"
        toBe (HttpServerError.message (RequestParseError(request "POST" "/bad", "invalid body"))) "RequestParseError (invalid body)"
        toBe (HttpServerError.message (InternalServerError "boom")) "InternalServerError (boom)"))
