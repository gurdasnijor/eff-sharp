module HttpApiSecuritySpec

open Effect
open Effect.Vitest

describe "HttpApiSecurity" (fun () ->
    test "defines bearer as an HTTP security scheme" (fun () ->
        toBe (HttpApiSecurity.scheme HttpApiSecurity.bearer) (Some "Bearer"))

    test "stores custom HTTP schemes" (fun () ->
        toBe (HttpApiSecurity.scheme (HttpApiSecurity.http "Token")) (Some "Token")))
