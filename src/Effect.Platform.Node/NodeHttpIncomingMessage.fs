namespace Effect.Platform.Node

open Effect
open Fable.Core

type NodeHttpIncomingMessage =
    { Source: obj
      Headers: Headers
      RemoteAddress: string option
      Text: Effect<string, PlatformError, Context>
      Body: Stream<byte[], PlatformError, Context> }

[<RequireQualifiedAccess>]
module NodeHttpIncomingMessage =

    [<Emit("Object.entries($0.headers || {}).flatMap(function(entry) { const k = entry[0]; const v = entry[1]; return Array.isArray(v) ? [[k, v.join(', ')]] : (v == null ? [] : [[k, String(v)]]); })")>]
    let private headersJs (_source: obj) : (string * string) array = jsNative

    [<Emit("($0.socket && $0.socket.remoteAddress) ? $0.socket.remoteAddress : ''")>]
    let private remoteAddressJs (_source: obj) : string = jsNative

    [<Emit("new Promise(function(resolve, reject) { const chunks = []; $0.on('data', function(chunk) { chunks.push(chunk); }); $0.on('end', function() { resolve(Buffer.concat(chunks).toString('utf8')); }); $0.on('error', reject); })")>]
    let private readTextJs (_source: obj) : JS.Promise<string> = jsNative

    let private toError methodName (ex: exn) =
        PlatformError.systemError
            { Tag = SystemErrorTag.Unknown
              Module = "NodeHttpIncomingMessage"
              Method = methodName
              Description = Some ex.Message
              Syscall = None
              PathOrDescriptor = None
              Cause = Some(box ex) }

    let make (source: obj) : NodeHttpIncomingMessage =
        let text =
            Effect.tryPromiseJS (fun () -> readTextJs source) (toError "text")

        { Source = source
          Headers = headersJs source |> Headers.ofSeq
          RemoteAddress =
            match remoteAddressJs source with
            | "" -> None
            | value -> Some value
          Text = text
          Body = NodeStream.fromReadable (fun () -> source) }

    let toServerRequest (method: string) (url: string) (body: HttpBody) (message: NodeHttpIncomingMessage) : HttpServerRequest =
        { HttpServerRequest.make method url message.Headers body with
            Source = Some message.Source
            RemoteAddress = message.RemoteAddress }
