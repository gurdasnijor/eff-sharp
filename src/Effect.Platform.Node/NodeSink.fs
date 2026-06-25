namespace Effect.Platform.Node

open Effect

/// `NodeSink` — bridge a host writable into an Effect `Sink<byte[], unit, …>`.
/// Mirror of effect-smol's `platform-node-shared/NodeSink.ts`. The reusable
/// `Writable → Sink` adapter (ChildProcess stdin, Stdio, Socket build on it):
/// write each chunk, then `close` the writer on completion (so a child sees EOF).
///
///   * `#if !FABLE_COMPILER` — `System.IO.Stream` (`WriteAsync` + `Close`).
///   * `#if FABLE_COMPILER` — a Node `Writable` (`.write()` + `.end()`).
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

#if !FABLE_COMPILER
    /// Write byte chunks to a .NET stream, closing it when the input ends.
    let fromWritable (getStream: unit -> System.IO.Stream) : Sink<byte[], unit, PlatformError, Context> =
        Sink.fromWrite
            (fun (chunk: byte[]) ->
                Effect.promise (fun () ->
                    task {
                        try
                            let stream = getStream ()
                            do! stream.WriteAsync(chunk, 0, chunk.Length)
                            do! stream.FlushAsync()
                            return Ok()
                        with ex ->
                            return Error ex
                    })
                |> Effect.flatMap (function
                    | Ok() -> Effect.succeed ()
                    | Error ex -> Effect.fail (failWrite ex)))
            (fun () ->
                Effect.sync (fun () ->
                    try
                        (getStream ()).Close()
                    with _ ->
                        ()))
#else
    open Fable.Core

    [<Emit("$0.write(new Uint8Array($1))")>]
    let private writeChunk (writable: obj) (chunk: byte[]) : unit = jsNative

    [<Emit("$0.end()")>]
    let private endWritable (writable: obj) : unit = jsNative

    /// Write byte chunks to a Node `Writable`, ending it when the input ends.
    let fromWritable (getWritable: unit -> obj) : Sink<byte[], unit, PlatformError, Context> =
        Sink.fromWrite (fun (chunk: byte[]) -> Effect.sync (fun () -> writeChunk (getWritable ()) chunk)) (fun () ->
            Effect.sync (fun () -> endWritable (getWritable ())))
#endif
