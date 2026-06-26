module NodeServicesSpec

open Effect
open Effect.Platform.Node
open Effect.Vitest

describe "NodeServices" (fun () ->
    itEffect "layer provides the standard Node service set" (fun () ->
        Layer.build NodeServices.layer
        |> Effect.map (fun ctx ->
            toBe (Context.contains ChildProcessSpawner.tag ctx) true
            toBe (Context.contains FileSystem.tag ctx) true
            toBe (Context.contains Path.tag ctx) true
            toBe (Context.contains Crypto.tag ctx) true
            toBe (Context.contains Terminal.tag ctx) true
            toBe (Context.contains Clock.tag ctx) true
            toBe (Context.contains Console.tag ctx) true)))
