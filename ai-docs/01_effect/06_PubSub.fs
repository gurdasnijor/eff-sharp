/// @title PubSub — broadcasting to many subscribers
///
/// A `PubSub` is a broadcast hub: every current subscriber receives every value
/// published after they subscribed. Subscriptions are *scoped* — you get one
/// inside a `Scope`, and it is cleaned up when the scope closes.
///
/// Mirrors upstream `01_effect/06_pubsub`. Backpressure strategy is chosen at
/// construction: `bounded` (suspend when full), `dropping`, `sliding`, or
/// `unbounded`.
module EffSharp.AiDocs.PubSubDocs

open Effect
open EffSharp.AiDocs.Shared

// Two subscribers both see the same broadcast — that's the point of a hub vs a
// queue (where each message goes to exactly one consumer).
let fanOut: Effect<string list, string, unit> =
    effect {
        let! hub = PubSub.bounded 16

        // Subscriptions live for the lifetime of this scope.
        return!
            Scope.scoped (fun scope ->
                effect {
                    let! sub1 = PubSub.subscribe scope hub
                    let! sub2 = PubSub.subscribe scope hub

                    // Publish AFTER subscribing so both subscribers receive it.
                    let! _ = PubSub.publish hub "broadcast"

                    let! a = PubSub.take sub1
                    let! b = PubSub.take sub2
                    return [ a; b ] // ["broadcast"; "broadcast"]
                })
    }

// A single subscriber draining a batch published with `publishAll`.
let drainBatch: Effect<string list, string, unit> =
    effect {
        let! hub = PubSub.bounded 16

        return!
            Scope.scoped (fun scope ->
                effect {
                    let! sub = PubSub.subscribe scope hub
                    let! _ = PubSub.publishAll hub [ "a"; "b"; "c" ]
                    let! x = PubSub.take sub
                    let! y = PubSub.take sub
                    let! z = PubSub.take sub
                    return [ x; y; z ]
                })
    }

let demo () =
    banner "06 · PubSub — broadcast fan-out"
    printfn "  fanOut     => %A" (run fanOut)
    printfn "  drainBatch => %A" (run drainBatch)
