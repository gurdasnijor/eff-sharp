module HttpApiSecuritySpec

open Effect
open Effect.Vitest

describe "HttpApiSecurity" (fun () ->
    test "defines bearer as an HTTP security scheme" (fun () ->
        toBe (HttpApiSecurity.scheme HttpApiSecurity.bearer) (Some "Bearer"))

    test "stores custom HTTP schemes" (fun () ->
        toBe (HttpApiSecurity.scheme (HttpApiSecurity.http "Token")) (Some "Token"))

    test "defines basic auth and api key locations" (fun () ->
        toEqual HttpApiSecurity.basic Basic
        toEqual (HttpApiSecurity.apiKeyName (HttpApiSecurity.apiKeyHeader "x-api-key")) (Some "x-api-key")
        toEqual (HttpApiSecurity.apiKeyLocation (HttpApiSecurity.apiKeyHeader "x-api-key")) (Some Header)
        toEqual (HttpApiSecurity.apiKeyLocation (HttpApiSecurity.apiKeyQuery "api_key")) (Some Query)
        toEqual (HttpApiSecurity.apiKeyLocation (HttpApiSecurity.apiKeyCookie "session")) (Some Cookie)))
