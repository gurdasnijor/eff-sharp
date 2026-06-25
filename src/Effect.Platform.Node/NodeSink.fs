namespace Effect.Platform.Node

open Effect

/// `NodeSink` — bridge a host writable into an Effect `Sink<byte[], unit, …>`.
/// Mirror of effect-smol's `platform-node-shared/NodeSink.ts`. The reusable
/// `Writable → Sink` adapter (ChildProcess stdin, Stdio, Socket build on it):
/// write each chunk, then `close` the writer on completion (so a child sees EOF).
///
/// Uses a Node `Writable` (`.write()` + `.end()`).
///
/// Built on the public `Sink.fromWrite` enabler — no internal `Sink` construction.
[<RequireQualifiedAccess>]
module NodeSink =

    let private failWrite (ex: exn) : PlatformError =
        PlatformError.systemError
            { Tag = SystemErrorTag.Unknown
              Module = "Sink"
              Method = "write"
              Description = Some ex.Message
              Syscall = None
              PathOrDescriptor = None
              Cause = Some(box ex) }

    open Fable.Core

    [<Emit("$0.write(new Uint8Array($1))")>]
    let private writeChunk (writable: obj) (chunk: byte[]) : unit = jsNative

    [<Emit("$0.end()")>]
    let private endWritable (writable: obj) : unit = jsNative

    /// Write byte chunks to a Node `Writable`, ending it when the input ends.
    let fromWritable (getWritable: unit -> obj) : Sink<byte[], unit, PlatformError, Context> =
        Sink.fromWrite (fun (chunk: byte[]) -> Effect.sync (fun () -> writeChunk (getWritable ()) chunk)) (fun () ->
            Effect.sync (fun () -> endWritable (getWritable ())))
