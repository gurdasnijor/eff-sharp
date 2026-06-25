namespace Effect.Platform.Node

open Effect

/// `NodeChildProcessSpawner` — the dual-backed implementation of the core
/// `ChildProcessSpawner` service. Mirror of effect-smol's
/// `platform-node-shared/NodeChildProcessSpawner.ts`.
///
///   * `#if !FABLE_COMPILER` — `System.Diagnostics.Process` (the xUnit surface).
///     Streams stdout/stderr **incrementally** (chunk-by-chunk via
///     `Stream.repeatEffectOption` over `Stream.ReadAsync`) and supports live
///     `Stdin` writes (`Sink.fromWrite`) — genuine bidirectional streaming.
///   * `#if FABLE_COMPILER` — Node's `node:child_process` via `Fable.Core` interop.
///     **Buffered v1**: attaches `data` listeners at spawn and resolves captured
///     chunks once the process `close`s, so `Stdout`/`Stderr` emit after exit.
///     Sufficient for spawn→capture→exit and batch commands; live incremental
///     streaming on Node (for long-running bidirectional JSON-RPC) is the next
///     step, and lands once `Stream.repeatEffectOption` is wired to a Node
///     `Readable` push-source.
///
/// Built entirely on the PUBLIC `Effect`/`Stream`/`Sink` surface — the internal
/// constructors are inaccessible from this assembly, which is correct: platform
/// packages compose via combinators.
[<RequireQualifiedAccess>]
module NodeChildProcessSpawner =

    /// Build a `PlatformError` for a child-process system failure.
    let private failSystem
        (errorTag: SystemErrorTag)
        (method: string)
        (description: string)
        (cause: obj option)
        : PlatformError =
        PlatformError.systemError
            { Tag = errorTag
              Module = "ChildProcess"
              Method = method
              Description = Some description
              Syscall = Some "spawn"
              PathOrDescriptor = None
              Cause = cause }

#if !FABLE_COMPILER
    // ------------------------------------------------------------------------
    // .NET layer — System.Diagnostics.Process (the xUnit test surface).
    // ------------------------------------------------------------------------

    /// Run an async host operation, mapping a throw into the `PlatformError`
    /// channel. The async catches into a `Result`, so no exception escapes the
    /// public `Effect.promise` (which would otherwise surface as a defect). On
    /// .NET, `Effect.promise` takes a `Task`, so the workflow is started as one.
    let private attempt (method: string) (work: unit -> Async<'A>) : Effect<'A, PlatformError, Context> =
        Effect.promise (fun () ->
            (async {
                try
                    let! v = work ()
                    return Ok v
                with ex ->
                    return Error ex
            })
            |> Async.StartAsTask)
        |> Effect.flatMap (function
            | Ok v -> Effect.succeed v
            | Error ex -> Effect.fail (failSystem SystemErrorTag.Unknown method ex.Message (Some(box ex))))

    /// Run a synchronous host thunk, mapping a throw into `PlatformError`.
    let private attemptSync (method: string) (thunk: unit -> 'A) : Effect<'A, PlatformError, Context> =
        Effect.suspend (fun () ->
            try
                Effect.succeed (thunk ())
            with ex ->
                Effect.fail (failSystem SystemErrorTag.Unknown method ex.Message (Some(box ex))))

    /// Read a redirected process stream incrementally, emitting each chunk.
    /// `enabled` is `false` for non-piped streams, which yield nothing.
    let private readStream
        (enabled: bool)
        (getStream: unit -> System.IO.Stream)
        : Stream<byte[], PlatformError, Context> =
        if not enabled then
            Stream.empty
        else
            let pull: Effect<byte[] option, PlatformError, Context> =
                attempt "stdout" (fun () ->
                    async {
                        let stream = getStream ()
                        let buffer = Array.zeroCreate<byte> 8192
                        let! n = stream.AsyncRead(buffer, 0, buffer.Length)
                        return (if n = 0 then None else Some(Array.sub buffer 0 n))
                    })

            Stream.repeatEffectOption pull

    /// A sink writing byte chunks to the process's stdin, closing it on completion.
    let private writeSink
        (enabled: bool)
        (proc: System.Diagnostics.Process)
        : Sink<byte[], unit, PlatformError, Context> =
        if not enabled then
            Sink.drain
        else
            Sink.fromWrite
                (fun (chunk: byte[]) ->
                    attempt "stdin" (fun () ->
                        async {
                            do! proc.StandardInput.BaseStream.AsyncWrite(chunk, 0, chunk.Length)
                            do! proc.StandardInput.BaseStream.FlushAsync() |> Async.AwaitTask
                        }))
                (fun () ->
                    Effect.sync (fun () ->
                        try
                            proc.StandardInput.Close()
                        with _ ->
                            ()))

    let private spawnDotnet
        (command: Command)
        (scope: Scope<PlatformError, Context>)
        : Effect<ChildProcessHandle, PlatformError, Context> =
        let (Command.Standard sc) = command
        let o = sc.Options

        Effect.suspend (fun () ->
            try
                let psi = System.Diagnostics.ProcessStartInfo()
                psi.FileName <- sc.Command

                for a in sc.Args do
                    psi.ArgumentList.Add a

                o.Cwd |> Option.iter (fun c -> psi.WorkingDirectory <- c)
                psi.UseShellExecute <- false
                psi.RedirectStandardOutput <- (o.Stdout = Stdio.Pipe)
                psi.RedirectStandardError <- (o.Stderr = Stdio.Pipe)
                psi.RedirectStandardInput <- (o.Stdin = Stdio.Pipe)

                if not o.ExtendEnv then
                    psi.Environment.Clear()

                for KeyValue(k, v) in o.Env do
                    match v with
                    | Some value -> psi.Environment.[k] <- value
                    | None -> psi.Environment.Remove k |> ignore

                let proc = new System.Diagnostics.Process()
                proc.StartInfo <- psi
                proc.Start() |> ignore
                let pid = proc.Id

                let handle =
                    { Pid = pid
                      Stdout = readStream (o.Stdout = Stdio.Pipe) (fun () -> proc.StandardOutput.BaseStream)
                      Stderr = readStream (o.Stderr = Stdio.Pipe) (fun () -> proc.StandardError.BaseStream)
                      Stdin = writeSink (o.Stdin = Stdio.Pipe) proc
                      ExitCode =
                        attempt "exitCode" (fun () ->
                            async {
                                do! proc.WaitForExitAsync() |> Async.AwaitTask
                                return proc.ExitCode
                            })
                      IsRunning = Effect.sync (fun () -> not proc.HasExited)
                      Kill =
                        attemptSync "kill" (fun () ->
                            if not proc.HasExited then
                                proc.Kill true) }

                // Tear down the process when the scope closes (best-effort).
                let cleanup =
                    Effect.sync (fun () ->
                        (try
                            if not proc.HasExited then
                                proc.Kill true
                         with _ ->
                             ())

                        (try
                            proc.Dispose()
                         with _ ->
                             ()))

                Scope.addFinalizer scope cleanup |> Effect.map (fun () -> handle)
            with ex ->
                let errorTag =
                    match ex with
                    | :? System.ComponentModel.Win32Exception -> SystemErrorTag.NotFound
                    | :? System.IO.FileNotFoundException -> SystemErrorTag.NotFound
                    | _ -> SystemErrorTag.Unknown

                Effect.fail (failSystem errorTag "spawn" ex.Message (Some(box ex))))

    let private spawner: ChildProcessSpawner = { Spawn = spawnDotnet }

#else
    // ------------------------------------------------------------------------
    // Node layer — node:child_process via Fable interop (the runtime target).
    // ------------------------------------------------------------------------
    open Fable.Core
    open Fable.Core.JsInterop

    // import { spawn } from "node:child_process"
    [<Import("spawn", "node:child_process")>]
    let private nodeSpawn (command: string) (args: string[]) (options: obj) : obj = jsNative

    /// Build the spawn options object. `cwd` is `null` to inherit the parent's.
    [<Emit("{ cwd: ($0 === null ? undefined : $0), stdio: [$1, $2, $3], env: $4 }")>]
    let private spawnOpts (cwd: string) (stdin: string) (stdout: string) (stderr: string) (env: obj) : obj = jsNative

    /// Build the environment object: optionally clone `process.env`, then apply
    /// overrides (a `null` value deletes the variable, mirroring `key: undefined`).
    [<Emit("(function(){ var e = $2 ? Object.assign({}, process.env) : {}; for (var i=0;i<$0.length;i++){ if ($1[i] === null) { delete e[$0[i]]; } else { e[$0[i]] = $1[i]; } } return e; })()")>]
    let private buildEnv (keys: string[]) (values: string[]) (extendEnv: bool) : obj = jsNative

    /// Attach `data`/`close`/`error` listeners immediately and resolve the
    /// captured chunks + exit code once the process closes. Called once per spawn.
    [<Emit("(function(p){ var out=[]; var err=[]; if(p.stdout)p.stdout.on('data',function(c){out.push(new Uint8Array(c));}); if(p.stderr)p.stderr.on('data',function(c){err.push(new Uint8Array(c));}); return new Promise(function(res){ p.on('error',function(e){res({stdout:out,stderr:err,code:-1});}); p.on('close',function(code){res({stdout:out,stderr:err,code:(code==null?-1:code)});}); }); })($0)")>]
    let private collectOutput (child: obj) : JS.Promise<obj> = jsNative

    [<Emit("$0.kill()")>]
    let private killChild (child: obj) : unit = jsNative

    [<Emit("($0.exitCode === null) && ($0.signalCode === null)")>]
    let private childRunning (child: obj) : bool = jsNative

    [<Emit("{ if ($0.stdin) { $0.stdin.write(new Uint8Array($1)); } }")>]
    let private writeStdin (child: obj) (chunk: byte[]) : unit = jsNative

    [<Emit("{ if ($0.stdin) { $0.stdin.end(); } }")>]
    let private endStdin (child: obj) : unit = jsNative

    /// Read a property by (possibly computed) key. Avoids the dynamic `?` operator
    /// for string-variable keys.
    [<Emit("$0[$1]")>]
    let private getField (o: obj) (key: string) : obj = jsNative

    let private stdioString (s: Stdio) : string =
        match s with
        | Stdio.Pipe -> "pipe"
        | Stdio.Inherit -> "inherit"
        | Stdio.Ignore -> "ignore"

    /// Emit the chunks captured by `collectOutput` from a resolved result object.
    /// Buffered: await the close-promise once, then emit the captured chunk array.
    let private emitCaptured (resultP: JS.Promise<obj>) (field: string) : Stream<byte[], PlatformError, Context> =
        Stream.fromEffect (
            Effect.promise (fun () ->
                async {
                    let! result = Async.AwaitPromise resultP
                    return (unbox (getField result field): byte[][])
                })
        )
        |> Stream.flatMap (fun chunks -> Stream.fromIterable chunks)

    let private spawnNode
        (command: Command)
        (scope: Scope<PlatformError, Context>)
        : Effect<ChildProcessHandle, PlatformError, Context> =
        let (Command.Standard sc) = command
        let o = sc.Options

        Effect.suspend (fun () ->
            try
                let keys = o.Env |> Map.toList |> List.map fst |> List.toArray

                let values =
                    o.Env
                    |> Map.toList
                    |> List.map (fun (_, v) ->
                        match v with
                        | Some s -> s
                        | None -> null)
                    |> List.toArray

                let env = buildEnv keys values o.ExtendEnv

                let cwd =
                    match o.Cwd with
                    | Some c -> c
                    | None -> null

                let opts =
                    spawnOpts cwd (stdioString o.Stdin) (stdioString o.Stdout) (stdioString o.Stderr) env

                let child = nodeSpawn sc.Command (List.toArray sc.Args) opts
                // Attach listeners ONCE; both output streams and ExitCode share this.
                let resultP = collectOutput child
                let pid: int = unbox (getField child "pid")

                let handle =
                    { Pid = pid
                      Stdout = emitCaptured resultP "stdout"
                      Stderr = emitCaptured resultP "stderr"
                      Stdin =
                        Sink.fromWrite
                            (fun (chunk: byte[]) -> Effect.sync (fun () -> writeStdin child chunk))
                            (fun () -> Effect.sync (fun () -> endStdin child))
                      ExitCode =
                        Effect.promise (fun () ->
                            async {
                                let! result = Async.AwaitPromise resultP
                                return (unbox (getField result "code"): int)
                            })
                      IsRunning = Effect.sync (fun () -> childRunning child)
                      Kill = Effect.sync (fun () -> killChild child) }

                let cleanup = Effect.sync (fun () -> killChild child)
                Scope.addFinalizer scope cleanup |> Effect.map (fun () -> handle)
            with ex ->
                Effect.fail (failSystem SystemErrorTag.Unknown "spawn" ex.Message None))

    let private spawner: ChildProcessSpawner = { Spawn = spawnNode }
#endif

    /// The platform `ChildProcessSpawner` layer: `System.Diagnostics.Process` on
    /// .NET (the xUnit test surface), Node's `child_process` under Fable.
    let layer<'E, 'RIn> : Layer<'E, 'RIn> =
        Layer.succeed ChildProcessSpawner.tag spawner

    /// A `Context` carrying the platform spawner, keyed under `ChildProcessSpawner.tag`.
    let liveContext: Context = Context.make ChildProcessSpawner.tag spawner
