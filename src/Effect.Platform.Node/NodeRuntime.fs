namespace Effect.Platform.Node

open Effect

/// `NodeRuntime` — the Node platform entry point. Mirror of effect-smol's
/// `platform-node` NodeRuntime.
///
/// `layer` merges every Node platform service a program typically needs
/// (FileSystem, ChildProcessSpawner, Clock, Console) so a consumer provides ONE
/// layer at the program edge instead of wiring each service. `runMain` runs an
/// effect as the program's main: provide that layer, run to its `Exit`, set the
/// process exit code, and report a failure's cause.
[<RequireQualifiedAccess>]
module NodeRuntime =

    /// The merged Node platform layer: FileSystem + ChildProcessSpawner + Clock +
    /// Console. Provide this once: `Layer.provide NodeRuntime.layer program`.
    let layer<'E, 'RIn> : Layer<'E, 'RIn> =
        Layer.mergeAll
            [ NodeFileSystem.layer
              NodeChildProcessSpawner.layer
              Layer.succeed Clock.tag (Clock.make ())
              Layer.succeed Console.tag Console.live ]

    [<Fable.Core.Emit("(globalThis.process.exitCode = $0)")>]
    let private setExitCode (code: int) : unit = ()

    /// Run `effect` as the program's main: provide the Node platform `layer`, run to
    /// its `Exit`, set the process exit code (0 success / 1 failure), and report a
    /// failure's cause to stderr. Returns immediately; the effect drives the Node
    /// event loop. (NodeRuntime.runMain)
    let runMain (effect: Effect<'A, 'E, Context>) : unit =
        let program = Layer.provide layer effect

        Async.StartImmediate(
            async {
                let! exit = Effect.runExit () program

                match exit with
                | Success _ -> setExitCode 0
                | Failure cause ->
                    eprintfn "%s" (Cause.render cause)
                    setExitCode 1
            }
        )
