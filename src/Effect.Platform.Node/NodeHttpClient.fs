namespace Effect.Platform.Node

open Effect

/// `NodeHttpClient` — the Node implementation of the core `HttpClient` service.
/// Mirror of effect-smol's `platform-node` `NodeHttpClient`, currently backed by
/// the global `fetch` available in Node 18+.
///
/// Built on the public `Effect` API. A transport failure maps to the typed
/// `HttpClientError`; any HTTP response (incl. 4xx/5xx) succeeds with its status.
[<RequireQualifiedAccess>]
module NodeHttpClient =

    let private failWith (method: string) (url: string) (reason: string) : HttpClientError =
        { Method = method
          Url = url
          Reason = reason }

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

    /// The platform `HttpClient` layer. (NodeHttpClient.layer)
    let layer<'E, 'RIn> : Layer<'E, 'RIn> = Layer.succeed HttpClient.tag impl

    /// Alias matching effect's `NodeHttpClient.layerFetch` (the fetch-backed client).
    let layerFetch<'E, 'RIn> : Layer<'E, 'RIn> = layer

    /// A `Context` carrying the platform HTTP client.
    let liveContext: Context = Context.make HttpClient.tag impl
