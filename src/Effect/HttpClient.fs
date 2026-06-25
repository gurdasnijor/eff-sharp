namespace Effect

/// `HttpClient` — a minimal HTTP client service (abstract).
///
/// Port of the slice of effect-smol's `effect/unstable/http/HttpClient` that the
/// cutover drivers use: a `request`/`get` that performs an HTTP call and yields
/// the response, failing (typed) on a transport error (e.g. connection refused).
/// fluent-firegrid's `verification` uses exactly this — `HttpClient.get url` in a
/// retry loop (`Effect.exit`) to poll a service for readiness. The full request/
/// response/streaming/`HttpApi` surface is deferred (effect-s2, Phase D).
///
/// Concrete implementations live in the platform package (`NodeHttpClient`):
/// `System.Net.Http` on .NET, `fetch` under Fable.
/// A response: HTTP status code + the body read as text. (HttpClientResponse,
/// minimal form.)
type HttpResponse = { Status: int; Body: string }

/// A transport-level failure (the request never produced a response — DNS,
/// connection refused, timeout). A response with a non-2xx status is NOT an error
/// here; inspect `HttpResponse.Status`. (HttpClientError)
type HttpClientError =
    { Method: string
      Url: string
      Reason: string }

/// The HTTP client service: perform `method` against `url`, yielding the response.
type HttpClient =
    { Request: string -> string -> Effect<HttpResponse, HttpClientError, Context> }

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
