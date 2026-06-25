namespace Effect

/// `ChildProcessSpawner` — the service that runs a `Command` and yields a handle
/// to its streams and lifecycle.
///
/// Net-new module (first end-to-end package cutover: `fluent-acp-process`). It
/// follows eff-sharp's established **intermediate-abstraction + dual-Layer**
/// platform pattern (`Path`/`Terminal`/`Console`): ONE abstract service
/// (`ChildProcessSpawner`, a `Tag` + record), backed TWICE —
///   * `#if !FABLE_COMPILER` — `System.Diagnostics.Process` (the **xUnit test
///     surface**; not throwaway).
///   * `#if FABLE_COMPILER`  — Node's `node:child_process` via Fable interop
///     (`Import`/`Emit` of `spawn`), the runtime target.
/// Consumers depend on the abstract service via `ChildProcessSpawner.tag` /
/// `ChildProcessSpawner.spawn`; the right `layer` is provided per runtime.
///
/// A spawned process is exposed as a `ChildProcessHandle`: `Stdout`/`Stderr` as
/// `Stream<byte[], …>` (chunks), `Stdin` as a `Sink<byte[], …>`, an awaitable
/// `ExitCode`, `IsRunning`, `Kill`, and the `Pid`. Errors surface as
/// `PlatformError` (the platform error channel, shared with `Path`/`Terminal`).
///
/// Streaming fidelity (reported, not buried): the **.NET** layer streams
/// stdout/stderr **incrementally** (chunk-by-chunk off the redirected pipe) and
/// supports live `Stdin` writes — genuine bidirectional streaming. The **Node**
/// layer is **buffered v1**: it attaches `data` listeners at spawn and resolves
/// the captured chunks once the process `close`s, so `Stdout`/`Stderr` emit after
/// exit. That is sufficient for the cutover's spawn→capture→exit proof and for
/// batch commands; live incremental streaming on Node (needed for
/// `fluent-acp-process`'s long-running bidirectional JSON-RPC) is the documented
/// next step. Deferred everywhere: `KillOptions` (signal/force-kill-after) and
/// command pipelines (see `ChildProcess.fs`).
/// A handle to a spawned child process. Mirrors the subset of effect's
/// `ChildProcessHandle` the cutover needs. (`ChildProcessHandle`)
type ChildProcessHandle =
    {
        /// The OS process id. (`pid`)
        Pid: int
        /// Standard output as a stream of byte chunks (empty unless stdout is
        /// piped). (`stdout`)
        Stdout: Stream<byte[], PlatformError, Context>
        /// Standard error as a stream of byte chunks (empty unless stderr is
        /// piped). (`stderr`)
        Stderr: Stream<byte[], PlatformError, Context>
        /// A sink that writes byte chunks to the process's stdin, closing it when
        /// the feeding stream completes (empty unless stdin is piped). (`stdin`)
        Stdin: Sink<byte[], unit, PlatformError, Context>
        /// Completes with the process exit code once it terminates. (`exitCode`)
        ExitCode: Effect<int, PlatformError, Context>
        /// Whether the process is still running. (`isRunning`)
        IsRunning: Effect<bool, PlatformError, Context>
        /// Terminate the process (whole tree on .NET; `child.kill()` on Node).
        /// (`kill`)
        Kill: Effect<unit, PlatformError, Context>
    }

/// The abstract spawner service: build a handle from a `Command`, registering its
/// teardown on the supplied `Scope`. Backed by the .NET or Node `layer`.
/// (`ChildProcessSpawner`)
type ChildProcessSpawner =
    { Spawn: Command -> Scope<PlatformError, Context> -> Effect<ChildProcessHandle, PlatformError, Context> }

[<RequireQualifiedAccess>]
module ChildProcessSpawner =

    /// The `Tag` under which the spawner service is stored.
    let tag: Tag<ChildProcessSpawner> =
        Tag.make<ChildProcessSpawner> "effect/platform/ChildProcessSpawner"

    /// Build a `PlatformError` for a child-process system failure. Shared by both
    /// layers (PlatformError is platform-agnostic / Fable-clean).
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

    /// Spawn `command` using the active `ChildProcessSpawner`, registering its
    /// teardown on `scope`. (ChildProcessSpawner accessor)
    let spawn
        (scope: Scope<PlatformError, Context>)
        (command: Command)
        : Effect<ChildProcessHandle, PlatformError, Context> =
        Effect.service tag |> Effect.flatMap (fun s -> s.Spawn command scope)

    /// Spawn `command`, drain its stdout to completion, await exit, and return the
    /// captured bytes and exit code. Reads stdout *before* awaiting exit, so it
    /// never deadlocks on a full pipe. Convenience for tests and simple consumers;
    /// the raw `Stream`/`ExitCode` surface remains available via `spawn`.
    /// (mirrors effect's `ChildProcessSpawner.string`, byte form)
    let bytesExit (command: Command) : Effect<byte[] * int, PlatformError, Context> =
        Scope.scoped (fun scope ->
            spawn scope command
            |> Effect.flatMap (fun handle ->
                Stream.runCollect handle.Stdout
                |> Effect.flatMap (fun chunks ->
                    handle.ExitCode
                    |> Effect.map (fun code -> (chunks |> List.toArray |> Array.collect id), code))))

#if !FABLE_COMPILER
    // ------------------------------------------------------------------------
    // .NET layer — System.Diagnostics.Process (the xUnit test surface).
    // ------------------------------------------------------------------------

    /// Read a redirected process stream incrementally, emitting each chunk.
    /// `enabled` is `false` for non-piped streams, which yield nothing.
    let private readStream
        (enabled: bool)
        (getStream: unit -> System.IO.Stream)
        : Stream<byte[], PlatformError, Context> =
        if not enabled then
            Stream.empty
        else
            { Run =
                fun emit ->
                    Effect(fun fib r ->
                        async {
                            let stream = getStream ()
                            let buffer = Array.zeroCreate<byte> 8192
                            let mutable outcome: Exit<unit, PlatformError> = Success()
                            let mutable go = true

                            while go do
                                let! read =
                                    async {
                                        try
                                            let! n = stream.AsyncRead(buffer, 0, buffer.Length)
                                            return Ok n
                                        with ex ->
                                            return Error ex
                                    }

                                match read with
                                | Error ex ->
                                    outcome <-
                                        Exit.fail (
                                            failSystem SystemErrorTag.Unknown "stdout" ex.Message (Some(box ex))
                                        )

                                    go <- false
                                | Ok 0 -> go <- false
                                | Ok n ->
                                    let chunk = Array.sub buffer 0 n
                                    let (Effect runEmit) = emit chunk
                                    let! emitExit = runEmit fib r

                                    match emitExit with
                                    | Success() -> ()
                                    | Failure c ->
                                        outcome <- Failure c
                                        go <- false

                            return outcome
                        }) }

    /// A sink writing byte chunks to the process's stdin, closing it on completion.
    let private writeSink
        (enabled: bool)
        (proc: System.Diagnostics.Process)
        : Sink<byte[], unit, PlatformError, Context> =
        if not enabled then
            Sink.drain
        else
            { Build =
                fun () ->
                    let emit (chunk: byte[]) =
                        Effect(fun _ _ ->
                            async {
                                try
                                    do! proc.StandardInput.BaseStream.AsyncWrite(chunk, 0, chunk.Length)
                                    do! proc.StandardInput.BaseStream.FlushAsync() |> Async.AwaitTask
                                    return Success()
                                with ex ->
                                    return
                                        Exit.fail (failSystem SystemErrorTag.Unknown "stdin" ex.Message (Some(box ex)))
                            })

                    let extract () =
                        Effect(fun _ _ ->
                            async {
                                try
                                    proc.StandardInput.Close()
                                with _ ->
                                    ()

                                return Success()
                            })

                    emit, extract }

    let private spawnDotnet
        (command: Command)
        (scope: Scope<PlatformError, Context>)
        : Effect<ChildProcessHandle, PlatformError, Context> =
        let (Command.Standard sc) = command
        let o = sc.Options

        Effect(fun fib r ->
            async {
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
                            Effect(fun _ _ ->
                                async {
                                    try
                                        do! proc.WaitForExitAsync() |> Async.AwaitTask
                                        return Success proc.ExitCode
                                    with ex ->
                                        return
                                            Exit.fail (
                                                failSystem SystemErrorTag.Unknown "exitCode" ex.Message (Some(box ex))
                                            )
                                })
                          IsRunning = Effect.sync (fun () -> not proc.HasExited)
                          Kill =
                            Effect(fun _ _ ->
                                async {
                                    try
                                        if not proc.HasExited then
                                            proc.Kill true

                                        return Success()
                                    with ex ->
                                        return
                                            Exit.fail (
                                                failSystem SystemErrorTag.Unknown "kill" ex.Message (Some(box ex))
                                            )
                                }) }

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

                    let (Effect runFin) = Scope.addFinalizer scope cleanup
                    let! _ = runFin fib r
                    return Success handle
                with ex ->
                    let errorTag =
                        match ex with
                        | :? System.ComponentModel.Win32Exception -> SystemErrorTag.NotFound
                        | :? System.IO.FileNotFoundException -> SystemErrorTag.NotFound
                        | _ -> SystemErrorTag.Unknown

                    return Exit.fail (failSystem errorTag "spawn" ex.Message (Some(box ex)))
            })

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
    let private emitCaptured (resultP: JS.Promise<obj>) (field: string) : Stream<byte[], PlatformError, Context> =
        { Run =
            fun emit ->
                Effect(fun fib r ->
                    async {
                        let! result = Async.AwaitPromise resultP
                        let chunks: byte[][] = unbox (getField result field)
                        let mutable outcome: Exit<unit, PlatformError> = Success()
                        let mutable i = 0

                        while i < chunks.Length
                              && (match outcome with
                                  | Success() -> true
                                  | _ -> false) do
                            let (Effect runEmit) = emit chunks.[i]
                            let! emitExit = runEmit fib r

                            match emitExit with
                            | Success() -> ()
                            | Failure c -> outcome <- Failure c

                            i <- i + 1

                        return outcome
                    }) }

    let private spawnNode
        (command: Command)
        (scope: Scope<PlatformError, Context>)
        : Effect<ChildProcessHandle, PlatformError, Context> =
        let (Command.Standard sc) = command
        let o = sc.Options

        Effect(fun fib r ->
            async {
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
                            { Build =
                                fun () ->
                                    (fun (chunk: byte[]) -> Effect.sync (fun () -> writeStdin child chunk)),
                                    (fun () -> Effect.sync (fun () -> endStdin child)) }
                          ExitCode =
                            Effect(fun _ _ ->
                                async {
                                    let! result = Async.AwaitPromise resultP
                                    return Success(unbox (getField result "code"): int)
                                })
                          IsRunning = Effect.sync (fun () -> childRunning child)
                          Kill = Effect.sync (fun () -> killChild child) }

                    let cleanup = Effect.sync (fun () -> killChild child)
                    let (Effect runFin) = Scope.addFinalizer scope cleanup
                    let! _ = runFin fib r
                    return Success handle
                with ex ->
                    return Exit.fail (failSystem SystemErrorTag.Unknown "spawn" ex.Message None)
            })

    let private spawner: ChildProcessSpawner = { Spawn = spawnNode }
#endif

    /// The platform `ChildProcessSpawner` layer: `System.Diagnostics.Process` on
    /// .NET (the xUnit test surface), Node's `child_process` under Fable.
    let layer<'E, 'RIn> : Layer<'E, 'RIn> = Layer.succeed tag spawner
