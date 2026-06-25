module LayerMapSpec

open Effect
open Effect.Vitest

let private run (eff: Effect<'A, 'E, unit>) : 'A =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure cause -> failwithf "unexpected failure: %s" (Cause.render cause)

let private dummyTag = Tag.make<string> "LayerMapTest/dummy"

describe "LayerMap" (fun () ->
    test "a key is built once and shared by concurrent borrowers" (fun () ->
        let acquired = System.Collections.Generic.List<string>()
        let released = System.Collections.Generic.List<string>()

        let layer key =
            Layer.scoped dummyTag (fun scope ->
                Scope.acquireRelease
                    scope
                    (Effect.sync (fun () ->
                        acquired.Add key
                        key))
                    (fun value _ -> Effect.sync (fun () -> released.Add value)))

        run (
            effect {
                let mapScope = Scope.make<string, unit> ()
                let! layerMap = LayerMap.makeDefault mapScope layer

                do!
                    Scope.scoped (fun outer ->
                        effect {
                            let! ctx1 = LayerMap.contextEffect layerMap "k" outer
                            let! ctx2 = LayerMap.contextEffect layerMap "k" outer
                            toBe (Context.get dummyTag ctx1) "k"
                            toBe (Context.get dummyTag ctx2) "k"
                            toBe (List.ofSeq acquired = [ "k" ]) true
                            toBe (List.ofSeq released = []) true
                        })

                toBe (List.ofSeq released = [ "k" ]) true
                do! Scope.close mapScope (Success(box ()))
            }
        ))

    test "fromRecord builds configured layer and invalidate releases idle entry" (fun () ->
        let acquired = System.Collections.Generic.List<string>()
        let released = System.Collections.Generic.List<string>()

        let layer key =
            Layer.scoped dummyTag (fun scope ->
                Scope.acquireRelease
                    scope
                    (Effect.sync (fun () ->
                        acquired.Add key
                        key))
                    (fun value _ -> Effect.sync (fun () -> released.Add value)))

        run (
            effect {
                let mapScope = Scope.make<string, unit> ()

                let layers =
                    Map.ofList
                        [ "short", layer "short"
                          "long", layer "long" ]

                let! layerMap = LayerMap.fromRecord mapScope (fun _ -> Duration.zero) layers
                do! Scope.scoped (fun s -> LayerMap.contextEffect layerMap "short" s |> Effect.map ignore)
                toBe (List.ofSeq acquired = [ "short" ]) true
                toBe (List.ofSeq released = [ "short" ]) true

                do! Scope.scoped (fun s -> LayerMap.contextEffect layerMap "long" s |> Effect.map ignore)
                toBe (List.ofSeq acquired = [ "short"; "long" ]) true
                toBe (List.ofSeq released = [ "short"; "long" ]) true

                do! LayerMap.invalidate layerMap "long"
                do! Scope.close mapScope (Success(box ()))
            }
        )))
