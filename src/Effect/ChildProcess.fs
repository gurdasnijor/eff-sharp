namespace Effect

/// `ChildProcess` — a command builder for spawning OS processes.
///
/// Net-new module (no upstream-parity slice predates it). Port of the
/// `StandardCommand` subset of effect's `unstable/process/ChildProcess.ts`,
/// scoped to what the first package cutover (`fluent-acp-process`) needs: build a
/// command (executable + args), set `cwd`/`env`/`extendEnv`, and configure the
/// three standard streams. The command is **plain immutable data**; running it is
/// the `ChildProcessSpawner` service's job (src/Effect/ChildProcessSpawner.fs),
/// mirroring the abstract-service + dual-Layer pattern of `Path`/`Terminal`.
///
/// Scope ported vs deferred (per the cutover brief, "minimum to run
/// fluent-acp-process"):
///   * Ported: `make` (executable + args), `setCwd`, `setEnv` (with `None` =
///     "unset this variable", mirroring upstream's `key: undefined`),
///     `setExtendEnv`, and `setStdin`/`setStdout`/`setStderr` over a three-way
///     `Stdio` disposition (`pipe`/`inherit`/`ignore`).
///   * Deferred (not used by acp-process; noted, not silently dropped):
///     `PipedCommand` / `pipeTo` / `prefix` (command pipelines), the
///     template-literal `make` form, custom `Sink`/`Stream` stdio targets,
///     additional file descriptors (`fdN`), `shell`/`detached`, and the
///     `KillOptions` (`killSignal`/`forceKillAfter`). `Command` is a one-case DU
///     so `PipedCommand` can be added without breaking callers.
/// Disposition of a child process's standard stream. Mirrors the effect string
/// literals `"pipe" | "inherit" | "ignore"`. `Pipe` captures the stream so it can
/// be read/written through the handle; `Inherit` shares the parent's stream;
/// `Ignore` discards it. (`CommandInput`/`CommandOutput`)
[<RequireQualifiedAccess>]
type Stdio =
    | Pipe
    | Inherit
    | Ignore

/// Options controlling how a command is spawned. (`CommandOptions`, subset)
type CommandOptions =
    {
        /// Working directory; `None` keeps the parent's. (`cwd`)
        Cwd: string option
        /// Environment overrides applied over the (optionally inherited) base
        /// environment. `Some v` sets the variable; `None` removes it (upstream's
        /// `key: undefined`). (`env`)
        Env: Map<string, string option>
        /// When `true`, overrides are merged over the parent process environment;
        /// when `false`, only `Env` is passed. (`extendEnv`)
        ExtendEnv: bool
        Stdin: Stdio
        Stdout: Stdio
        Stderr: Stdio
    }

/// An executable invocation. Only `StandardCommand` is ported (see module doc).
type StandardCommand =
    { Command: string
      Args: string list
      Options: CommandOptions }

/// A command to spawn. A one-case DU: `PipedCommand` is deferred but can be added
/// here without breaking the `ChildProcess.make`/spawner surface. (`Command`)
[<RequireQualifiedAccess>]
type Command = Standard of StandardCommand

[<RequireQualifiedAccess>]
module ChildProcess =

    /// The default options: inherit the parent environment, pipe all three
    /// standard streams. (mirrors effect's "pipe" stdio defaults + env inherit)
    let defaultOptions: CommandOptions =
        { Cwd = None
          Env = Map.empty
          ExtendEnv = true
          Stdin = Stdio.Pipe
          Stdout = Stdio.Pipe
          Stderr = Stdio.Pipe }

    /// Build a command from an executable and its arguments. (ChildProcess.make)
    let make (command: string) (args: string list) : Command =
        Command.Standard
            { Command = command
              Args = args
              Options = defaultOptions }

    /// Build a command with explicit options. (ChildProcess.make, options form)
    let makeWith (command: string) (args: string list) (options: CommandOptions) : Command =
        Command.Standard
            { Command = command
              Args = args
              Options = options }

    let private mapOptions (f: CommandOptions -> CommandOptions) (cmd: Command) : Command =
        match cmd with
        | Command.Standard sc -> Command.Standard { sc with Options = f sc.Options }

    /// Set the working directory. (ChildProcess.setCwd)
    let setCwd (cwd: string) (cmd: Command) : Command =
        mapOptions (fun o -> { o with Cwd = Some cwd }) cmd

    /// Merge environment overrides over the command's existing overrides. A
    /// `None` value removes the variable from the spawned environment.
    /// (ChildProcess.setEnv)
    let setEnv (env: Map<string, string option>) (cmd: Command) : Command =
        mapOptions
            (fun o ->
                { o with
                    Env = Map.fold (fun acc k v -> Map.add k v acc) o.Env env })
            cmd

    /// Whether to merge overrides over the parent environment. (extendEnv)
    let setExtendEnv (extend: bool) (cmd: Command) : Command =
        mapOptions (fun o -> { o with ExtendEnv = extend }) cmd

    /// Configure the stdin disposition. (stdin)
    let setStdin (stdin: Stdio) (cmd: Command) : Command =
        mapOptions (fun o -> { o with Stdin = stdin }) cmd

    /// Configure the stdout disposition. (stdout)
    let setStdout (stdout: Stdio) (cmd: Command) : Command =
        mapOptions (fun o -> { o with Stdout = stdout }) cmd

    /// Configure the stderr disposition. (stderr)
    let setStderr (stderr: Stdio) (cmd: Command) : Command =
        mapOptions (fun o -> { o with Stderr = stderr }) cmd

    /// The executable name of a command. (StandardCommand.command)
    let command (cmd: Command) : string =
        match cmd with
        | Command.Standard sc -> sc.Command

    /// The arguments of a command. (StandardCommand.args)
    let args (cmd: Command) : string list =
        match cmd with
        | Command.Standard sc -> sc.Args

    /// The options of a command. (StandardCommand.options)
    let options (cmd: Command) : CommandOptions =
        match cmd with
        | Command.Standard sc -> sc.Options
