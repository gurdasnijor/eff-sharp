namespace Effect.Platform.Node

open Effect
open Fable.Core

/// Listen options for the Node HTTP server adapter.
type NodeHttpListenOptions =
    { Port: int
      Host: string option }

/// A transport-level failure while starting or closing a Node HTTP server.
type NodeHttpServerError = { Reason: string }

/// A running Node HTTP server.
type NodeHttpServerHandle =
    { Address: string
      Port: int option
      Close: unit -> Effect<unit, NodeHttpServerError, Context> }

/// Minimal Node `http.Server` adapter for the pure `HttpRouter` substrate.
[<RequireQualifiedAccess>]
module NodeHttpServer =

    [<Import("createServer", "node:http")>]
    let private createServerJs (handler: System.Func<obj, obj, unit>) : obj = jsNative

    [<Emit("new Promise(function(resolve, reject) { const onError = function(e) { $0.off('error', onError); reject(e); }; $0.once('error', onError); $0.listen({ port: $1, host: $2 == null ? undefined : $2 }, function() { $0.off('error', onError); resolve(undefined); }); })")>]
    let private listenJs (server: obj) (port: int) (host: obj) : JS.Promise<unit> = jsNative

    [<Emit("new Promise(function(resolve, reject) { $0.close(function(err) { if (err) reject(err); else resolve(undefined); }); })")>]
    let private closeJs (server: obj) : JS.Promise<unit> = jsNative

    [<Emit("$0 && $0.message ? $0.message : String($0)")>]
    let private errorMessage (error: obj) : string = jsNative

    [<Emit("$0.method || 'GET'")>]
    let private requestMethod (request: obj) : string = jsNative

    [<Emit("$0.url || '/'")>]
    let private requestUrl (request: obj) : string = jsNative

    [<Emit("Object.entries($0.headers || {}).flatMap(function(entry) { const k = entry[0]; const v = entry[1]; return Array.isArray(v) ? [[k, v.join(', ')]] : (v == null ? [] : [[k, String(v)]]); })")>]
    let private requestHeaders (request: obj) : (string * string) array = jsNative

    [<Emit("($0.socket && $0.socket.remoteAddress) ? $0.socket.remoteAddress : ''")>]
    let private requestRemoteAddress (request: obj) : string = jsNative

    [<Emit("new Promise(function(resolve, reject) { const chunks = []; $0.on('data', function(chunk) { chunks.push(chunk); }); $0.on('end', function() { resolve(Buffer.concat(chunks).toString('utf8')); }); $0.on('error', reject); })")>]
    let private readBodyTextJs (request: obj) : JS.Promise<string> = jsNative

    [<Emit("(() => { const a = $0.address(); return typeof a === 'string' ? a : (a ? a.address : ''); })()")>]
    let private serverAddress (server: obj) : string = jsNative

    [<Emit("(() => { const a = $0.address(); return a && typeof a === 'object' && a.port != null ? a.port : 0; })()")>]
    let private serverPort (server: obj) : int = jsNative

    [<Emit("$0.statusCode = $1; if ($2 != null) $0.statusMessage = $2; $3.forEach(function(pair) { $0.setHeader(pair[0], pair[1]); }); if ($4.length > 0) $0.setHeader('Set-Cookie', $4.map(function(pair) { return pair[0] + '=' + pair[1]; })); $0.end($5 == null ? undefined : $5);")>]
    let private writeResponseJs
        (response: obj)
        (status: int)
        (statusText: obj)
        (headers: (string * string) array)
        (cookies: (string * string) array)
        (body: obj)
        : unit =
        jsNative

    let private toError (ex: exn) : NodeHttpServerError = { Reason = ex.Message }

    let private hostObj (host: string option) : obj =
        match host with
        | Some value -> box value
        | None -> null

    let private statusTextObj (statusText: string option) : obj =
        match statusText with
        | Some value -> box value
        | None -> null

    let private bodyPayload (body: HttpBody) : obj =
        match body with
        | EmptyBody -> null
        | TextBody(value, _) -> box value
        | JsonBody value -> box (HttpBody.toJsonString value)
        | BytesBody(value, _) -> box value

    let private requestBody (headers: Headers) (bodyText: string) : HttpBody =
        if bodyText = "" then
            HttpBody.empty
        else
            match Headers.get "content-type" headers with
            | Some contentType -> HttpBody.textWithContentType contentType bodyText
            | None -> HttpBody.text bodyText

    let private toServerRequest (source: obj) (bodyText: string) : HttpServerRequest =
        let headers = requestHeaders source |> Headers.ofSeq
        let remoteAddress = requestRemoteAddress source

        { HttpServerRequest.make (requestMethod source) (requestUrl source) headers (requestBody headers bodyText) with
            Source = Some source
            RemoteAddress = if remoteAddress = "" then None else Some remoteAddress }

    let private errorResponse (error: HttpServerError) : HttpServerResponse =
        HttpServerResponse.textWith
            { HttpServerResponseOptions.empty with Status = Some(HttpServerError.status error) }
            (HttpServerError.message error)

    let private defectResponse (cause: Cause<HttpServerError>) : HttpServerResponse =
        HttpServerResponse.textWith
            { HttpServerResponseOptions.empty with Status = Some 500 }
            (Cause.render cause)

    let private writeServerResponse (nodeResponse: obj) (request: HttpServerRequest) (response: HttpServerResponse) : unit =
        let body =
            if request.Method = "HEAD" then
                null
            else
                bodyPayload response.Body

        writeResponseJs
            nodeResponse
            response.Status
            (statusTextObj response.StatusText)
            (response.Headers |> Headers.toSeq |> Seq.toArray)
            (response.Cookies |> Map.toSeq |> Seq.toArray)
            body

    let private makeHandler (runtime: Runtime) (router: HttpRouter) : System.Func<obj, obj, unit> =
        System.Func<obj, obj, unit>(fun nodeRequest nodeResponse ->
            Async.StartImmediate(
                async {
                    try
                        let! bodyText = Async.AwaitPromise(readBodyTextJs nodeRequest)
                        let request = toServerRequest nodeRequest bodyText
                        let! exit = Runtime.runExit runtime (HttpRouter.handle request router)

                        let response =
                            match exit with
                            | Success response -> response
                            | Failure cause ->
                                match Cause.failures cause |> List.tryHead with
                                | Some error -> errorResponse error
                                | None -> defectResponse cause

                        writeServerResponse nodeResponse request response
                    with ex ->
                        let response =
                            HttpServerResponse.textWith
                                { HttpServerResponseOptions.empty with Status = Some 500 }
                                ex.Message

                        writeResponseJs
                            nodeResponse
                            response.Status
                            null
                            (response.Headers |> Headers.toSeq |> Seq.toArray)
                            [||]
                            (bodyPayload response.Body)
                }
            ))

    let private closeEffect (server: obj) : Effect<unit, NodeHttpServerError, Context> =
        Effect.promise (fun () ->
            async {
                try
                    do! Async.AwaitPromise(closeJs server)
                    return Ok()
                with ex ->
                    return Error(toError ex)
            })
        |> Effect.flatMap (function
            | Ok() -> Effect.succeed ()
            | Error error -> Effect.fail error)

    /// Start a Node HTTP server that serves a pure `HttpRouter`.
    let serve (options: NodeHttpListenOptions) (router: HttpRouter) : Effect<NodeHttpServerHandle, NodeHttpServerError, Context> =
        Effect.environment<Context, NodeHttpServerError>
        |> Effect.flatMap (fun context ->
            Effect.promise (fun () ->
                async {
                    try
                        let server = createServerJs (makeHandler (Runtime.make context) router)
                        do! Async.AwaitPromise(listenJs server options.Port (hostObj options.Host))

                        let port = serverPort server

                        return
                            Ok
                                { Address = serverAddress server
                                  Port = if port = 0 then None else Some port
                                  Close = fun () -> closeEffect server }
                    with ex ->
                        return Error(toError ex)
                })
            |> Effect.flatMap (function
                | Ok handle -> Effect.succeed handle
                | Error error -> Effect.fail error))

    /// Convenience constructor for `127.0.0.1:<port>`.
    let listenLocal (port: int) (router: HttpRouter) : Effect<NodeHttpServerHandle, NodeHttpServerError, Context> =
        serve { Port = port; Host = Some "127.0.0.1" } router
