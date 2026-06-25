namespace Effect.Platform.Node

open Effect

[<RequireQualifiedAccess>]
module NodeHttpServerRequest =

    /// Return the underlying Node `IncomingMessage` stored on a server request.
    let toIncomingMessage (request: HttpServerRequest) : obj =
        match request.Source with
        | Some source -> source
        | None -> invalidArg "request" "HttpServerRequest does not carry a Node IncomingMessage source"

    /// Effect's upstream Node adapter can recover the active ServerResponse from
    /// request internals. eff-sharp's request value stores only the incoming
    /// message today, so this remains explicit instead of returning a fake value.
    let toServerResponse (_request: HttpServerRequest) : obj =
        invalidOp "NodeHttpServerRequest.toServerResponse is not available on eff-sharp HttpServerRequest values yet"
