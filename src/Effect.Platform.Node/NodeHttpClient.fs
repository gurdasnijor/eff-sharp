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

    [<Emit("fetch($0, { method: $1, headers: Object.fromEntries($2), body: $3 == null ? undefined : $3 })")>]
    let private fetchJs (url: string) (method: string) (headers: (string * string) array) (body: obj) : JS.Promise<obj> =
        jsNative

    [<Emit("$0.status")>]
    let private respStatus (o: obj) : int = jsNative

    [<Emit("$0.text()")>]
    let private respText (o: obj) : JS.Promise<string> = jsNative

    [<Emit("$0.body")>]
    let private respBodyStream (o: obj) : obj = jsNative

    [<Emit("Array.from($0.headers.entries())")>]
    let private respHeaders (o: obj) : (string * string) array = jsNative

    let private bodyPayload (body: HttpBody) : obj =
        match body with
        | EmptyBody -> null
        | TextBody(value, _) -> box value
        | JsonBody value -> box (HttpBody.toJsonString value)
        | BytesBody(value, _) -> box value
        | StreamBody _ -> invalidOp "Streaming request bodies are not supported by NodeHttpClient yet"
        | FormDataBody formData -> formData

    let private baseContentType (contentType: string) =
        match contentType.IndexOf ';' with
        | -1 -> contentType.Trim().ToLowerInvariant()
        | i -> contentType.Substring(0, i).Trim().ToLowerInvariant()

    let private shouldStreamResponse (headers: Headers) =
        match Headers.get "content-type" headers with
        | Some contentType ->
            match baseContentType contentType with
            | "text/event-stream"
            | "application/octet-stream" -> true
            | _ -> false
        | None -> false

    let private execute (req: HttpClientRequest) : Effect<HttpClientResponse, HttpClientError, Context> =
        let url = HttpClientRequest.urlWithParams req
        let headers = req.Headers |> Headers.toSeq |> Seq.toArray
        let body = bodyPayload req.Body

        Effect.promise (fun () ->
            async {
                try
                    let! r = Async.AwaitPromise(fetchJs url req.Method headers body)
                    let responseHeaders = r |> respHeaders |> Headers.ofSeq
                    let status = respStatus r

                    if shouldStreamResponse responseHeaders then
                        let contentType =
                            Headers.get "content-type" responseHeaders
                            |> Option.defaultValue "application/octet-stream"

                        let stream =
                            Stream.fromReadableStreamJS (respBodyStream r) box

                        return Ok(HttpClientResponse.make req status responseHeaders (HttpBody.streamBytesWithContentType contentType stream))
                    else
                        let! body = Async.AwaitPromise(respText r)
                        return Ok(HttpClientResponse.text req status responseHeaders body)
                with ex ->
                    return Error ex.Message
            })
        |> Effect.flatMap (function
            | Ok r -> Effect.succeed r
            | Error reason -> Effect.fail (failWith req.Method url reason))

    let private request (method: string) (url: string) : Effect<HttpResponse, HttpClientError, Context> =
        HttpClientRequest.make method url
        |> execute
        |> Effect.map (fun response ->
            { Status = response.Status
              Body = response |> HttpClientResponse.bodyText |> Option.defaultValue "" })

    let private impl: HttpClient =
        { Request = request
          Execute = execute }

    /// The platform `HttpClient` layer. (NodeHttpClient.layer)
    let layer<'E, 'RIn> : Layer<'E, 'RIn> = Layer.succeed HttpClient.tag impl

    /// Alias matching effect's `NodeHttpClient.layerFetch` (the fetch-backed client).
    let layerFetch<'E, 'RIn> : Layer<'E, 'RIn> = layer

    /// A `Context` carrying the platform HTTP client.
    let liveContext: Context = Context.make HttpClient.tag impl
