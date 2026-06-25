namespace Effect.Platform.Node

open Effect

/// `NodeHttpClient` — the dual-backed implementation of the core `HttpClient`
/// service. Mirror of effect-smol's `platform-node` `NodeHttpClient`.
///
///   * `#if !FABLE_COMPILER` — `System.Net.Http.HttpClient` (the non-Fable BCL fallback).
///   * `#if FABLE_COMPILER` — the global `fetch` (Node 18+).
///
/// Built on the public `Effect` API. A transport failure maps to the typed
/// `HttpClientError`; any HTTP response (incl. 4xx/5xx) succeeds with its status.
[<RequireQualifiedAccess>]
module NodeHttpClient =

    let private failWith (method: string) (url: string) (reason: string) : HttpClientError =
        { Method = method
          Url = url
          Reason = reason }

#if !FABLE_COMPILER
    // One shared HttpClient (connection pooling), as recommended by the BCL.
    let private client = new System.Net.Http.HttpClient()

    let private impl: HttpClient =
        { Request =
            fun method url ->
                Effect.promise (fun () ->
                    task {
                        try
                            use req =
                                new System.Net.Http.HttpRequestMessage(System.Net.Http.HttpMethod method, url)

                            let! resp = client.SendAsync req
                            let! body = resp.Content.ReadAsStringAsync()

                            return
                                Ok
                                    { Status = int resp.StatusCode
                                      Body = body }
                        with ex ->
                            return Error ex.Message
                    })
                |> Effect.flatMap (function
                    | Ok r -> Effect.succeed r
                    | Error reason -> Effect.fail (failWith method url reason)) }
#else
    open Fable.Core

    // fetch(url, { method }) -> Promise resolving to { status, body } (body read as text).
    [<Emit("fetch($0, { method: $1 }).then(function(r){ return r.text().then(function(b){ return { status: r.status, body: b }; }); })")>]
    let private fetchJs (url: string) (method: string) : JS.Promise<obj> = jsNative

    [<Emit("$0.status")>]
    let private respStatus (o: obj) : int = jsNative

    [<Emit("$0.body")>]
    let private respBody (o: obj) : string = jsNative

    let private impl: HttpClient =
        { Request =
            fun method url ->
                Effect.promise (fun () ->
                    async {
                        try
                            let! r = Async.AwaitPromise(fetchJs url method)

                            return
                                Ok
                                    { Status = respStatus r
                                      Body = respBody r }
                        with ex ->
                            return Error ex.Message
                    })
                |> Effect.flatMap (function
                    | Ok r -> Effect.succeed r
                    | Error reason -> Effect.fail (failWith method url reason)) }
#endif

    /// The platform `HttpClient` layer. (NodeHttpClient.layer)
    let layer<'E, 'RIn> : Layer<'E, 'RIn> = Layer.succeed HttpClient.tag impl

    /// Alias matching effect's `NodeHttpClient.layerFetch` (the fetch-backed client).
    let layerFetch<'E, 'RIn> : Layer<'E, 'RIn> = layer

    /// A `Context` carrying the platform HTTP client.
    let liveContext: Context = Context.make HttpClient.tag impl
