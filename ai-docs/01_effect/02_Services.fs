/// @title Services & dependency injection (Tag · Context · Layer)
///
/// A *service* is a value (usually a record of functions) stored in the `Context`
/// under a typed `Tag`. An effect that reads a service has `Context` in its `'R`
/// channel; you discharge that requirement by *providing* the service — directly
/// with `Effect.provideService`, or compositionally with a `Layer`.
///
/// Mirrors upstream `01_effect/02_services`. eff-sharp models the service as a
/// plain F# record + a `Tag`, so there is no class/`Context.Service` ceremony —
/// just data and functions.
module EffSharp.AiDocs.Services

open Effect
open EffSharp.AiDocs.Shared

// ---------------------------------------------------------------------------
// 1. Declare services as records of effects, keyed by a typed Tag.
// ---------------------------------------------------------------------------

/// A low-level capability. Its single method is itself an effect.
type Clock = { CurrentMillis: Effect<int64, string, Context> }
let clockTag: Tag<Clock> = Tag.make<Clock> "app/Clock"

/// A higher-level service that DEPENDS on `Clock`.
type Greeter = { Greet: string -> Effect<string, string, Context> }
let greeterTag: Tag<Greeter> = Tag.make<Greeter> "app/Greeter"

// ---------------------------------------------------------------------------
// 2. Read a service inside an effect with `Effect.service tag`.
// ---------------------------------------------------------------------------

/// Note the `Context` in the requirement channel — this effect is honest about
/// needing a `Greeter` to run.
let program: Effect<string, string, Context> =
    effect {
        let! greeter = Effect.service greeterTag
        return! greeter.Greet "Ada"
    }

// ---------------------------------------------------------------------------
// 3. Build implementations as Layers. A Layer is a recipe for a Context, and it
//    can itself require other services (here `greeterLayer` requires `Clock`).
// ---------------------------------------------------------------------------

/// A deterministic clock (great for tests — see 09_testing).
let clockLive: Layer<string, 'RIn> =
    Layer.succeed clockTag { CurrentMillis = Effect.sync (fun () -> 123L) }

/// `Layer.effect` builds the service *in an effect*, so it can pull in its own
/// dependencies via `Effect.service`. This layer's requirement is `Clock`.
let greeterLayer: Layer<string, Context> =
    Layer.effect
        greeterTag
        (effect {
            let! clock = Effect.service clockTag
            return
                { Greet =
                    fun name ->
                        effect {
                            let! t = clock.CurrentMillis
                            return sprintf "Hello, %s! (clock=%dms)" name t
                        } }
        })

// ---------------------------------------------------------------------------
// 4. Provide the layers. `Layer.provide` discharges a service requirement; chain
//    it to wire a dependency graph. After both, `'R` collapses to `unit`.
// ---------------------------------------------------------------------------

let wired: Effect<string, string, unit> =
    program
    |> Layer.provide greeterLayer // supplies Greeter (still needs Clock)
    |> Layer.provide clockLive //    supplies Clock     (now needs nothing)

// ---------------------------------------------------------------------------
// 5. For a single service you can skip Layers entirely with `provideService`.
// ---------------------------------------------------------------------------

let justTheClock: Effect<int64, string, unit> =
    effect {
        let! clock = Effect.service clockTag
        return! clock.CurrentMillis
    }
    |> Effect.provideService clockTag { CurrentMillis = Effect.succeed 999L }

let demo () =
    banner "02 · Services — Tag / Context / Layer"
    printfn "  wired greeting  = %s" (run wired)
    printfn "  provideService  = %dms" (run justTheClock)
