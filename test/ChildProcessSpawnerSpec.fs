module ChildProcessSpawnerSpec

open Effect
open Effect.Vitest

let private fakeHandle =
    { Pid = 123
      Stdout = Stream.fromIterable [ [| 1uy; 2uy |]; [| 3uy |] ]
      Stderr = Stream.empty
      Stdin = Sink.drain
      ExitCode = Effect.succeed 7
      IsRunning = Effect.succeed false
      Kill = Effect.succeed () }

describe "ChildProcessSpawner" (fun () ->
    itEffectIn
        (Context.make
            ChildProcessSpawner.tag
            { Spawn = fun _ _ -> Effect.succeed fakeHandle })
        "spawn returns the provided handle" (fun () ->
            Scope.scoped (fun scope ->
                ChildProcess.make "node" [ "--version" ]
                |> ChildProcessSpawner.spawn scope
                |> Effect.flatMap (fun handle ->
                    handle.IsRunning
                    |> Effect.map (fun running ->
                        toBe handle.Pid 123
                        toBe running false))))

    itEffectIn
        (Context.make
            ChildProcessSpawner.tag
            { Spawn = fun _ _ -> Effect.succeed fakeHandle })
        "bytesExit drains stdout before returning the exit code" (fun () ->
            ChildProcess.make "node" [ "-e"; "process.stdout.write('ok')" ]
            |> ChildProcessSpawner.bytesExit
            |> Effect.map (fun (bytes, code) ->
                toEqual bytes [| 1uy; 2uy; 3uy |]
                toBe code 7)))
