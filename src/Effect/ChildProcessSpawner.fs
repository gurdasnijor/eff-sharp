namespace Effect

/// `ChildProcessSpawner` — the abstract service that runs a `Command` and yields a
/// handle to its streams and lifecycle.
///
/// Port of effect-smol's `effect/unstable/process/ChildProcessSpawner.ts` (the
/// abstract service). The concrete implementations live in the platform package,
/// as upstream splits the service from `platform-node-shared/NodeChildProcessSpawner`:
///   * `Effect.Platform.Node`'s `NodeChildProcessSpawner` — dual-backed,
///     `System.Diagnostics.Process` on .NET (the xUnit surface) and Node's
///     `node:child_process` under Fable, provided via `NodeChildProcessSpawner.layer`.
///
/// Consumers depend on the abstract service via `ChildProcessSpawner.tag` /
/// `ChildProcessSpawner.spawn`; the platform `layer` is provided at the edge.
///
/// A spawned process is exposed as a `ChildProcessHandle`: `Stdout`/`Stderr` as
/// `Stream<byte[], …>` (chunks), `Stdin` as a `Sink<byte[], …>`, an awaitable
/// `ExitCode`, `IsRunning`, `Kill`, and the `Pid`. Errors surface as
/// `PlatformError` (the platform error channel, shared with `FileSystem`/`Path`).
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
/// teardown on the supplied `Scope`. Backed by the platform `layer`.
/// (`ChildProcessSpawner`)
type ChildProcessSpawner =
    { Spawn: Command -> Scope<PlatformError, Context> -> Effect<ChildProcessHandle, PlatformError, Context> }

[<RequireQualifiedAccess>]
module ChildProcessSpawner =

    /// The `Tag` under which the spawner service is stored. Implementations
    /// (`NodeChildProcessSpawner`) register under this tag; accessors read it back.
    let tag: Tag<ChildProcessSpawner> =
        Tag.make<ChildProcessSpawner> "effect/platform/ChildProcessSpawner"

    /// Spawn `command` using the `ChildProcessSpawner` provided in `Context`,
    /// registering its teardown on `scope`. The service is a required dependency —
    /// supply `NodeChildProcessSpawner.layer`. (ChildProcessSpawner accessor)
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
