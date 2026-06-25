namespace Effect.Platform.Node

open Effect

/// `NodeStream` — bridge a host readable into an Effect `Stream<byte[], …>`.
/// Mirror of effect-smol's `platform-node-shared/NodeStream.ts`. The reusable
/// `Readable → Stream` adapter every Node platform module (ChildProcess stdout,
/// Stdio, Socket) builds on.
///
///   * `#if !FABLE_COMPILER` — `System.IO.Stream`, read **incrementally**
///     (chunk-by-chunk via `Stream.repeatEffectOption` over `ReadAsync`).
///   * `#if FABLE_COMPILER` — a Node `Readable`, **buffered v1**: collect `data`
///     chunks, emit them once the stream `end`s. True incremental Node reads (pull
///     via `readable.read()` + `await 'readable'`) are the documented next step.
///
/// Built on the public `Stream.repeatEffectOption` enabler — no internal `Stream`
/// construction.
[<RequireQualifiedAccess>]
module NodeStream =

    let private failRead (ex: exn) : PlatformError =
        PlatformError.systemError
            { Tag = SystemErrorTag.Unknown
              Module = "Stream"
              Method = "read"
              Description = Some ex.Message
              Syscall = None
              PathOrDescriptor = None
              Cause = Some(box ex) }

#if !FABLE_COMPILER
    /// Incrementally emit chunks read from a .NET stream. `getStream` returns the
    /// same underlying stream on each pull (e.g. a process's redirected pipe).
    let fromReadable (getStream: unit -> System.IO.Stream) : Stream<byte[], PlatformError, Context> =
        let pull: Effect<byte[] option, PlatformError, Context> =
            Effect.promise (fun () ->
                task {
                    try
                        let stream = getStream ()
                        let buffer = Array.zeroCreate<byte> 8192
                        let! n = stream.ReadAsync(buffer, 0, buffer.Length)
                        return Ok(if n = 0 then None else Some(Array.sub buffer 0 n))
                    with ex ->
                        return Error ex
                })
            |> Effect.flatMap (function
                | Ok r -> Effect.succeed r
                | Error ex -> Effect.fail (failRead ex))

        Stream.repeatEffectOption pull
#else
    open Fable.Core

    /// Attach `data`/`end`/`error` listeners and resolve the captured chunk array
    /// once the readable ends (rejects on error). Buffered v1.
    [<Emit("new Promise(function(res,rej){ var out=[]; $0.on('data',function(c){out.push(new Uint8Array(c));}); $0.on('end',function(){res(out);}); $0.on('error',function(e){rej(e);}); })")>]
    let private collectReadable (readable: obj) : JS.Promise<byte[][]> = jsNative

    /// Emit chunks from a Node `Readable`, buffered: await `end`, then emit all.
    let fromReadable (getReadable: unit -> obj) : Stream<byte[], PlatformError, Context> =
        let pull: Effect<byte[][] option, PlatformError, Context> =
            // One-shot pull: the whole buffered array, then None.
            Effect.promise (fun () ->
                async {
                    try
                        let! chunks = Async.AwaitPromise(collectReadable (getReadable ()))
                        return Ok(Some chunks)
                    with ex ->
                        return Error ex
                })
            |> Effect.flatMap (function
                | Ok r -> Effect.succeed r
                | Error ex -> Effect.fail (failRead ex))

        // Emit the captured array's chunks in order: pull once (Some chunks), flatten.
        Stream.fromEffect pull
        |> Stream.flatMap (function
            | Some chunks -> Stream.fromIterable chunks
            | None -> Stream.empty)
#endif
