module Program

open Effect
open Fable.Core

// ChildProcess Node smoke test — the REAL proof of the dual-Layer design's Node
// side. Compiled by Fable to JS and run on Node; spawns actual child processes
// through ChildProcessSpawner.layer (the node:child_process-backed Layer) and
// asserts captured stdout/stderr + exit codes + env overrides.

let mutable passed = 0
let mutable failed = 0

let private pass (name: string) (detail: string) =
    passed <- passed + 1
    printfn "  PASS  %s  ->  %s" name detail

let private fail (name: string) (detail: string) =
    failed <- failed + 1
    printfn "  FAIL  %s  ->  %s" name detail

[<Emit("Buffer.from($0).toString('utf8')")>]
let private decode (bytes: byte[]) : string = jsNative

[<Emit("process.exitCode = $0")>]
let private setExitCode (code: int) : unit = jsNative

let private bytesExit (cmd: Command) : Async<Exit<byte[] * int, PlatformError>> =
    ChildProcessSpawner.bytesExit cmd
    |> Layer.provide ChildProcessSpawner.layer
    |> Effect.runExit ()

let private smoke: Async<unit> =
    async {
        // 1) capture stdout + zero exit
        let! e1 = bytesExit (ChildProcess.make "node" [ "-e"; "process.stdout.write('hi from node')" ])

        match e1 with
        | Success(bytes, code) ->
            let text = decode bytes

            if text = "hi from node" then
                pass "stdout capture" text
            else
                fail "stdout capture" (sprintf "got '%s'" text)

            if code = 0 then
                pass "zero exit code" "0"
            else
                fail "zero exit code" (string code)
        | Failure c -> fail "stdout capture" (Cause.render c)

        // 2) non-zero exit code
        let! e2 = bytesExit (ChildProcess.make "node" [ "-e"; "process.exit(7)" ])

        match e2 with
        | Success(_, code) ->
            if code = 7 then
                pass "non-zero exit code" "7"
            else
                fail "non-zero exit code" (string code)
        | Failure c -> fail "non-zero exit code" (Cause.render c)

        // 3) environment override is visible to the child
        let envCmd =
            ChildProcess.make "node" [ "-e"; "process.stdout.write(process.env.GREETING || 'none')" ]
            |> ChildProcess.setEnv (Map.ofList [ "GREETING", Some "ahoy-node" ])

        let! e3 = bytesExit envCmd

        match e3 with
        | Success(bytes, _) ->
            let text = decode bytes

            if text = "ahoy-node" then
                pass "env override" text
            else
                fail "env override" (sprintf "got '%s'" text)
        | Failure c -> fail "env override" (Cause.render c)

        printfn ""
        printfn "==== NODE SMOKE: %d passed, %d failed ====" passed failed

        if failed > 0 then
            setExitCode 1
    }

[<EntryPoint>]
let main _ =
    printfn "==== eff-sharp ChildProcess Node smoke ===="
    Async.StartImmediate smoke
    0
