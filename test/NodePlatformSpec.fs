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
            toBe response.Body "node-platform")))
