namespace Effect

/// `HttpClient` — an HTTP client service (abstract).
///
/// The service still exposes the legacy string request function used by current
/// callers, while the module also provides structured request/response helpers
/// over the unstable/http value substrate.
///
/// Concrete implementations live in the platform package (`NodeHttpClient`),
/// backed by Node `fetch`.
/// A response: HTTP status code + the body read as text.
type HttpResponse = { Status: int; Body: string }

/// A transport-level failure (the request never produced a response — DNS,
/// connection refused, timeout). A response with a non-2xx status is NOT an error
/// here; inspect `HttpResponse.Status`. (HttpClientError)
type HttpClientError =
    { Method: string
      Url: string
      Reason: string }

/// The HTTP client service: execute a structured request. The legacy `Request`
/// member is retained for current callers and is implemented by platform layers
/// in terms of `Execute`.
type HttpClient =
    { Request: string -> string -> Effect<HttpResponse, HttpClientError, Context>
      Execute: HttpClientRequest -> Effect<HttpClientResponse, HttpClientError, Context> }

[<RequireQualifiedAccess>]
module HttpClient =

    /// The `Tag` under which the `HttpClient` service is stored. Implementations
    /// (`NodeHttpClient`) register under it; accessors read it back.
    let tag: Tag<HttpClient> = Tag.make<HttpClient> "effect/http/HttpClient"

    /// Perform `method` against `url` using the provided `HttpClient`. Requires the
    /// service in `Context` (supply `NodeHttpClient.layer`/`layerFetch`).
    let request (method: string) (url: string) : Effect<HttpResponse, HttpClientError, Context> =
        Effect.service tag |> Effect.flatMap (fun c -> c.Request method url)

    /// A GET request. (HttpClient.get)
    let get (url: string) : Effect<HttpResponse, HttpClientError, Context> = request "GET" url

    /// A POST request. (HttpClient.post)
    let post (url: string) : Effect<HttpResponse, HttpClientError, Context> = request "POST" url

    /// Execute a structured request through the current service.
    let execute (req: HttpClientRequest) : Effect<HttpClientResponse, HttpClientError, Context> =
        Effect.service tag |> Effect.flatMap (fun c -> c.Execute req)
