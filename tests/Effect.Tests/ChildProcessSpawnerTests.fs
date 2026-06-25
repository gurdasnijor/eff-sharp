module Effect.Tests.ChildProcessSpawnerTests

open Xunit
open Effect
open Effect.Platform.Node

// Net-new module — no upstream *.test.ts. These Facts exercise the ported surface
// against the REAL .NET layer (System.Diagnostics.Process): they spawn actual
// processes and assert captured stdout + exit codes, plus env/stdin wiring and
// spawn-failure mapping. This is the .NET test surface of the dual-Layer design
// (the Node layer is proven separately by the Fable probe / Node smoke test).

// Spawn a portable shell snippet: cmd.exe on Windows, /bin/sh elsewhere. The CI
// gate and local dev are Unix; Windows is covered for completeness.
let private isWindows =
    System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(System.Runtime.InteropServices.OSPlatform.Windows)

let private shell (script: string) : Command =
    if isWindows then
        ChildProcess.make "cmd" [ "/c"; script ]
    else
        ChildProcess.make "/bin/sh" [ "-c"; script ]

let private decode (chunks: byte[] list) : string =
    chunks
    |> List.toArray
    |> Array.collect id
    |> System.Text.Encoding.UTF8.GetString

let private provide (program: Effect<'A, PlatformError, Context>) : Effect<'A, PlatformError, unit> =
    Layer.provide NodeChildProcessSpawner.layer program

let private runExit (program: Effect<'A, PlatformError, Context>) : Exit<'A, PlatformError> =
    Effect.runSync () (provide program)

let private run (program: Effect<'A, PlatformError, Context>) : 'A =
    match runExit program with
    | Success a -> a
    | Failure c -> failwithf "unexpected failure: %s" (Cause.render c)

// ----------------------------------------------------------------------------

[<Fact>]
let ``spawn captures stdout and a zero exit code`` () =
    let bytes, code = run (ChildProcessSpawner.bytesExit (shell "printf 'hello world'"))

    Assert.Equal("hello world", System.Text.Encoding.UTF8.GetString(bytes))
    Assert.Equal(0, code)

[<Fact>]
let ``a non-zero exit code is reported`` () =
    let bytes, code = run (ChildProcessSpawner.bytesExit (shell "exit 3"))
    Assert.Equal(3, code)
    Assert.Empty(bytes)

[<Fact>]
let ``environment overrides are visible to the child`` () =
    let command =
        shell (
            if isWindows then
                "echo %GREETING%"
            else
                "printf '%s' \"$GREETING\""
        )
        |> ChildProcess.setEnv (Map.ofList [ "GREETING", Some "ahoy" ])

    let bytes, code = run (ChildProcessSpawner.bytesExit command)
    Assert.Equal("ahoy", System.Text.Encoding.UTF8.GetString(bytes).Trim())
    Assert.Equal(0, code)

[<Fact>]
let ``stdin is written to the child and echoed back on stdout`` () =
    // `cat` echoes stdin to stdout. Feed it, close stdin, then read stdout.
    let command = shell "cat"
    let input = Stream.fromIterable [ System.Text.Encoding.UTF8.GetBytes "piped-in" ]

    let program =
        Scope.scoped (fun scope ->
            effect {
                let! handle = ChildProcessSpawner.spawn scope command
                do! Stream.run handle.Stdin input
                let! outChunks = Stream.runCollect handle.Stdout
                let! code = handle.ExitCode
                return (decode outChunks, code)
            })

    if isWindows then
        () // `cat` is not a cmd.exe builtin; covered on Unix.
    else
        let text, code = run program
        Assert.Equal("piped-in", text)
        Assert.Equal(0, code)

[<Fact>]
let ``stderr is captured when piped`` () =
    // Write to fd 2; with stderr piped (the default) it should be captured.
    let command = shell (if isWindows then "echo oops 1>&2" else "printf 'oops' 1>&2")

    let program =
        Scope.scoped (fun scope ->
            effect {
                let! handle = ChildProcessSpawner.spawn scope command
                let! errChunks = Stream.runCollect handle.Stderr
                let! code = handle.ExitCode
                return (decode errChunks, code)
            })

    let text, code = run program
    Assert.Equal("oops", text.Trim())
    Assert.Equal(0, code)

[<Fact>]
let ``isRunning reports false after the process exits`` () =
    let program =
        Scope.scoped (fun scope ->
            effect {
                let! handle = ChildProcessSpawner.spawn scope (shell "exit 0")
                let! _ = handle.ExitCode
                return! handle.IsRunning
            })

    Assert.False(run program)

[<Fact>]
let ``spawning a missing executable fails with a PlatformError`` () =
    let program =
        ChildProcessSpawner.bytesExit (ChildProcess.make "eff-sharp-no-such-binary-xyz" [])

    match runExit program with
    | Success _ -> Assert.Fail "expected a spawn failure"
    | Failure c ->
        Assert.True(
            Cause.failures c
            |> List.exists (fun (e: PlatformError) -> e.Tag = "PlatformError"),
            "expected a PlatformError in the failure cause"
        )

[<Fact>]
let ``the layer provides a working spawner via the accessor`` () =
    let bytes, code = run (ChildProcessSpawner.bytesExit (shell "printf 'ok'"))
    Assert.Equal("ok", System.Text.Encoding.UTF8.GetString(bytes))
    Assert.Equal(0, code)
