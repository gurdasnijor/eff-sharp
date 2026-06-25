module NodeChildProcessSpawnerSpec

open System.Text
open Effect
open Effect.Platform.Node
open Effect.Vitest

let private childProcessContext =
    Context.merge NodeChildProcessSpawner.liveContext Clock.live

let private utf8 (bytes: byte[]) = Encoding.UTF8.GetString bytes

describe "NodeChildProcessSpawner" (fun () ->
    itEffectIn childProcessContext "captures stdout and exit code" (fun () ->
        ChildProcess.make "node" [ "-e"; "process.stdout.write('ok')" ]
        |> ChildProcessSpawner.bytesExit
        |> Effect.map (fun (bytes, code) ->
            toBe code 0
            toBe (utf8 bytes) "ok"))

    itEffectIn childProcessContext "applies environment overrides" (fun () ->
        ChildProcess.make "node" [ "-e"; "process.stdout.write(process.env.EFF_SHARP_CHILD || '')" ]
        |> ChildProcess.setEnv (Map.ofList [ "EFF_SHARP_CHILD", Some "set" ])
        |> ChildProcessSpawner.bytesExit
        |> Effect.map (fun (bytes, code) ->
            toBe code 0
            toBe (utf8 bytes) "set")))
