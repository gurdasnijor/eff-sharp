namespace Effect.Platform.Node

open Effect

/// `NodeChildProcessSpawner` — the Node implementation of the core
/// `ChildProcessSpawner` service. Mirror of effect-smol's
/// `platform-node-shared/NodeChildProcessSpawner.ts`.
///
/// Uses Node's `node:child_process` via `Fable.Core` interop. Buffered v1:
/// attaches `data` listeners at spawn and resolves captured chunks once the
/// process `close`s, so `Stdout`/`Stderr` emit after exit. Sufficient for
/// spawn→capture→exit and batch commands; live incremental streaming on Node is
/// the next step.
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

    // ------------------------------------------------------------------------
    // Node layer — node:child_process via Fable interop.
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

    /// Resolve the exit code once the process closes (or `-1` on error). Attached
    /// ONCE at spawn — `close` fires only once, so a late listener would hang.
    /// stdout/stderr are read separately via `NodeStream.fromReadable`.
    [<Emit("new Promise(function(res){ $0.on('error',function(){res(-1);}); $0.on('close',function(code){res(code==null?-1:code);}); })")>]
    let private awaitExit (child: obj) : JS.Promise<int> = jsNative

    [<Emit("$0.kill()")>]
    let private killChild (child: obj) : unit = jsNative

    [<Emit("($0.exitCode === null) && ($0.signalCode === null)")>]
    let private childRunning (child: obj) : bool = jsNative

    /// Read a property by (possibly computed) key. Avoids the dynamic `?` operator
    /// for string-variable keys.
    [<Emit("$0[$1]")>]
    let private getField (o: obj) (key: string) : obj = jsNative

    let private stdioString (s: Stdio) : string =
        match s with
        | Stdio.Pipe -> "pipe"
        | Stdio.Inherit -> "inherit"
        | Stdio.Ignore -> "ignore"

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
                // Attach the exit listener ONCE, at spawn; stdout/stderr read lazily
                // via NodeStream (Node paused-mode buffers until consumed).
                let exitP = awaitExit child
                let pid: int = unbox (getField child "pid")

                let handle =
                    { Pid = pid
                      Stdout = NodeStream.fromReadable (fun () -> getField child "stdout")
                      Stderr = NodeStream.fromReadable (fun () -> getField child "stderr")
                      Stdin = NodeSink.fromWritable (fun () -> getField child "stdin")
                      ExitCode = Effect.promise (fun () -> async { return! Async.AwaitPromise exitP })
                      IsRunning = Effect.sync (fun () -> childRunning child)
                      Kill = Effect.sync (fun () -> killChild child) }

                let cleanup = Effect.sync (fun () -> killChild child)
                Scope.addFinalizer scope cleanup |> Effect.map (fun () -> handle)
            with ex ->
                Effect.fail (failSystem SystemErrorTag.Unknown "spawn" ex.Message None))

    let private spawner: ChildProcessSpawner = { Spawn = spawnNode }

    /// The platform `ChildProcessSpawner` layer.
    let layer<'E, 'RIn> : Layer<'E, 'RIn> =
        Layer.succeed ChildProcessSpawner.tag spawner

    /// A `Context` carrying the platform spawner, keyed under `ChildProcessSpawner.tag`.
    let liveContext: Context = Context.make ChildProcessSpawner.tag spawner
