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
        toEqual (HttpApiSecurity.apiKeyLocation (HttpApiSecurity.apiKeyCookie "session")) (Some Cookie))

    test "decodes bearer tokens from authorization headers" (fun () ->
        let request =
            HttpServerRequest.make
                "GET"
                "/"
                (Headers.ofList [ "authorization", "Bearer abc123" ])
                HttpBody.empty

        let credential =
            HttpApiBuilder.securityDecode HttpApiSecurity.bearer request
            |> Runtime.runSync Runtime.defaultRuntime

        match credential with
        | HttpCredential token -> toBe (Redacted.value token) "abc123"
        | _ -> failwith "expected HTTP credential")

    test "decodes custom HTTP schemes without preserving the leading separator" (fun () ->
        let request =
            HttpServerRequest.make
                "GET"
                "/"
                (Headers.ofList [ "authorization", "Token abc123" ])
                HttpBody.empty

        let credential =
            HttpApiBuilder.securityDecode (HttpApiSecurity.http "Token") request
            |> Runtime.runSync Runtime.defaultRuntime

        match credential with
        | HttpCredential token -> toBe (Redacted.value token) "abc123"
        | _ -> failwith "expected HTTP credential")

    test "decodes basic auth credentials" (fun () ->
        let encoded = Encoding.encodeBase64String "user:pass"

        let request =
            HttpServerRequest.make
                "GET"
                "/"
                (Headers.ofList [ "authorization", "Basic " + encoded ])
                HttpBody.empty

        let credential =
            HttpApiBuilder.securityDecode HttpApiSecurity.basic request
            |> Runtime.runSync Runtime.defaultRuntime

        match credential with
        | BasicCredentials credentials ->
            toBe credentials.Username "user"
            toBe (Redacted.value credentials.Password) "pass"
        | _ -> failwith "expected basic credentials")

    test "decodes API keys from header, query, and cookie locations" (fun () ->
        let headerRequest =
            HttpServerRequest.make
                "GET"
                "/"
                (Headers.ofList [ "x-api-key", "header-secret" ])
                HttpBody.empty

        let queryRequest =
            HttpServerRequest.make "GET" "/?api_key=query-secret" Headers.empty HttpBody.empty

        let cookieRequest =
            HttpServerRequest.make
                "GET"
                "/"
                (Headers.ofList [ "cookie", "session=cookie-secret" ])
                HttpBody.empty

        let decode security request =
            HttpApiBuilder.securityDecode security request
            |> Runtime.runSync Runtime.defaultRuntime
            |> HttpApiSecurity.credentialValue

        toBe (decode (HttpApiSecurity.apiKeyHeader "x-api-key") headerRequest) "header-secret"
        toBe (decode (HttpApiSecurity.apiKeyQuery "api_key") queryRequest) "query-secret"
        toBe (decode (HttpApiSecurity.apiKeyCookie "session") cookieRequest) "cookie-secret")

    test "fails security decode when credentials are absent" (fun () ->
        let result =
            HttpApiBuilder.securityDecode HttpApiSecurity.bearer (HttpServerRequest.make "GET" "/" Headers.empty HttpBody.empty)
            |> Effect.runSync Runtime.defaultRuntime.Context

        match result with
        | Failure cause ->
            match Cause.failures cause with
            | RequestParseError(_, reason) :: _ -> toBe reason "Missing Authorization header"
            | _ -> failwith "expected RequestParseError"
        | Success _ -> failwith "expected security failure"))
