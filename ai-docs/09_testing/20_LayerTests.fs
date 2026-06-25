/// @title Testing services with swappable layers
///
/// The payoff of dependency injection is testability: to test a service in
/// isolation, provide *stub* layers for its dependencies. Because a `Layer` is a
/// value, swapping the real implementation for a deterministic one is a one-line
/// change — and the type system guarantees the stub satisfies the same `Tag`.
///
/// Mirrors upstream `09_testing/20_layer-tests` (the `TodoRepoTestRef` pattern).
module EffSharp.AiDocs.LayerTests

open Effect
open EffSharp.AiDocs.Shared

// A low-level dependency.
type IdGen = { Next: Effect<int, string, Context> }
let idTag: Tag<IdGen> = Tag.make<IdGen> "app/IdGen"

// The service under test depends on `IdGen`.
type Widgets = { Make: string -> Effect<int * string, string, Context> }
let widgetTag: Tag<Widgets> = Tag.make<Widgets> "app/Widgets"

let widgetLayer: Layer<string, Context> =
    Layer.effect
        widgetTag
        (effect {
            let! gen = Effect.service idTag

            return
                { Make =
                    fun name ->
                        effect {
                            let! id = gen.Next
                            return id, name
                        } }
        })

// The REAL IdGen — stateful, returns 1, 2, 3, …
let idLive: Layer<string, unit> =
    Layer.effect
        idTag
        (effect {
            let! counter = Ref.make 0
            return { Next = Ref.updateAndGet counter (fun n -> n + 1) }
        })

// A STUB IdGen for tests — deterministic, always 42.
let idStub: Layer<string, unit> =
    Layer.succeed idTag { Next = Effect.succeed 42 }

let private program: Effect<int * string, string, Context> =
    effect {
        let! widgets = Effect.service widgetTag
        return! widgets.Make "gear"
    }

let private check2 label cond = check label cond

let demo () =
    banner "09 · Testing — swappable dependency layers"

    // Test the Widgets service against the STUB IdGen: fully deterministic.
    let wiredStub = program |> Layer.provide widgetLayer |> Layer.provide idStub

    match Effect.runSync () wiredStub with
    | Success(id, name) -> check2 "widget uses the stubbed IdGen (42)" (id = 42 && name = "gear")
    | Failure cause -> check2 (sprintf "stub run failed: %s" (Cause.render cause)) false

    // Same program, REAL IdGen: the only change is which layer we provide.
    let wiredLive = program |> Layer.provide widgetLayer |> Layer.provide idLive

    match Effect.runSync () wiredLive with
    | Success(id, _) -> check2 "live IdGen starts at 1" (id = 1)
    | Failure cause -> check2 (sprintf "live run failed: %s" (Cause.render cause)) false
