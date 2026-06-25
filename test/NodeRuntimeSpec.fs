module NodeRuntimeSpec

open Effect
open Effect.Platform.Node
open Effect.Vitest

describe "NodeRuntime" (fun () ->
    itEffect "layer provides the default Node platform services" (fun () ->
        Layer.build NodeRuntime.layer
        |> Effect.map (fun ctx ->
            toBe (Context.contains FileSystem.tag ctx) true
            toBe (Context.contains ChildProcessSpawner.tag ctx) true
            toBe (Context.contains HttpClient.tag ctx) true
            toBe (Context.contains Clock.tag ctx) true
            toBe (Context.contains Console.tag ctx) true))

    itEffect "layer can run HTTP client effects at the edge" (fun () ->
        HttpClient.get "data:text/plain,node-runtime"
        |> Layer.provide NodeRuntime.layer
        |> Effect.map (fun response ->
            toBe response.Status 200
            toBe response.Body "node-runtime")))
