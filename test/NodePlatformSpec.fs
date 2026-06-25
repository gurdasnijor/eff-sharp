module NodePlatformSpec

open Effect
open Effect.Platform.Node
open Effect.Vitest

describe "NodePlatform" (fun () ->
    test "runtime identifies the node target" (fun () ->
        toBe (Runtime.runSync Runtime.defaultRuntime NodePlatform.runtime) "node")

    itEffectIn Clock.live "layer provides the default Node HTTP client" (fun () ->
        HttpClient.get "data:text/plain,node-platform"
        |> Layer.provide NodePlatform.layer
        |> Effect.map (fun response ->
            toBe response.Status 200
            toBe response.Body "node-platform"))

    itEffectIn Clock.live "layer provides Node path and crypto services" (fun () ->
        effect {
            let! path = Path.path
            let! crypto = Crypto.service
            let! bytes = crypto.RandomBytes 4

            toBe (path.Basename "/tmp/file.txt" None) "file.txt"
            toBe bytes.Length 4
        }
        |> Layer.provide NodePlatform.layer))
