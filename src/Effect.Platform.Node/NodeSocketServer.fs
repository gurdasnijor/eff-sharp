namespace Effect.Platform.Node

open Effect
open Fable.Core

/// TCP listen options for `NodeSocketServer.make`.
type NodeListenOptions =
    { Host: string option
      Port: int }

type NodeTcpAddress = { Hostname: string; Port: int }

type NodeSocketAddress =
    | TcpAddress of NodeTcpAddress
    | UnixAddress of string

type NodeSocketServer =
    { Address: NodeSocketAddress
      Close: Effect<unit, PlatformError, Context> }

[<RequireQualifiedAccess>]
module NodeSocketServer =

    let tag: Tag<NodeSocketServer> =
        Tag.make<NodeSocketServer> "@effect/platform-node/NodeSocketServer"

    let private toError (method: string) (ex: exn) : PlatformError =
        PlatformError.systemError
            { Tag = Unknown
              Module = "NodeSocketServer"
              Method = method
              Description = Some ex.Message
              Syscall = None
              PathOrDescriptor = None
              Cause = Some(box ex) }

    [<Import("createServer", "node:net")>]
    let private createServer (options: obj) : obj = jsNative

    [<Emit("({})")>]
    let private emptyOptions () : obj = jsNative

    [<Emit("($0 == null ? { port: $1 } : { host: $0, port: $1 })")>]
    let private listenOptions (host: obj) (port: int) : obj = jsNative

    [<Emit("""new Promise((resolve, reject) => {
  const cleanup = () => { $0.off("error", onError); };
  const onError = (err) => { cleanup(); reject(err); };
  $0.once("error", onError);
  $0.listen($1, () => { cleanup(); resolve(undefined); });
})""")>]
    let private listenPromise (server: obj) (options: obj) : JS.Promise<unit> = jsNative

    [<Emit("""new Promise((resolve, reject) => {
  if (!$0.listening) {
    resolve(undefined);
  } else {
    $0.close((err) => err ? reject(err) : resolve(undefined));
  }
})""")>]
    let private closePromise (server: obj) : JS.Promise<unit> = jsNative

    [<Emit("$0.address()")>]
    let private addressRaw (server: obj) : obj = jsNative

    [<Emit("typeof $0 === 'string'")>]
    let private isString (value: obj) : bool = jsNative

    [<Emit("$0.address")>]
    let private addressHostname (value: obj) : string = jsNative

    [<Emit("$0.port")>]
    let private addressPort (value: obj) : int = jsNative

    let private closeEffect (server: obj) : Effect<unit, PlatformError, Context> =
        Effect.promise (fun () ->
            async {
                try
                    do! Async.AwaitPromise(closePromise server)
                    return Ok()
                with ex ->
                    return Error(toError "close" ex)
            })
        |> Effect.flatMap (function
            | Ok() -> Effect.succeed ()
            | Error error -> Effect.fail error)

    /// Create and bind a Node TCP server. The returned handle exposes the bound
    /// address and an explicit `Close` effect. This covers the common port-0
    /// allocation path and gives callers deterministic cleanup without requiring
    /// an ambient scope representation.
    let make (options: NodeListenOptions) : Effect<NodeSocketServer, PlatformError, Context> =
        Effect.promise (fun () ->
            async {
                let server = createServer (emptyOptions ())
                let host = options.Host |> Option.defaultValue null

                try
                    do! Async.AwaitPromise(listenPromise server (listenOptions (box host) options.Port))
                    let raw = addressRaw server

                    let address =
                        if isString raw then
                            UnixAddress(unbox<string> raw)
                        else
                            TcpAddress
                                { Hostname = addressHostname raw
                                  Port = addressPort raw }

                    return Ok { Address = address; Close = closeEffect server }
                with ex ->
                    try
                        do! Async.AwaitPromise(closePromise server)
                    with _ ->
                        ()

                    return Error(toError "make" ex)
            })
        |> Effect.flatMap (function
            | Ok server -> Effect.succeed server
            | Error error -> Effect.fail error)

    let layer (options: NodeListenOptions) : Layer<PlatformError, Context> =
        Layer.effect tag (make options)
