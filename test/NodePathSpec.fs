module NodePathSpec

open Effect
open Effect.Platform.Node
open Effect.Vitest

describe "NodePath" (fun () ->
    itEffectIn Clock.live "provides the Path service" (fun () ->
        effect {
            let! path = Path.path
            toBe (path.Join [ "/tmp"; "eff"; ".."; "file.txt" ]) "/tmp/file.txt"
        }
        |> Layer.provide NodePath.layer))
