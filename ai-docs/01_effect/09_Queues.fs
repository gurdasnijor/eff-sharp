/// @title Queues — work distribution (vs PubSub broadcast)
///
/// A `Queue` is a concurrent buffer: each value is taken by exactly ONE consumer
/// (work distribution), in contrast to a `PubSub` where every subscriber sees
/// every value (broadcast). Construction picks the backpressure strategy:
/// `bounded` (offer suspends when full), `dropping`, `sliding`, or `unbounded`.
///
/// Mirrors upstream queue docs. Note the error channel is `obj` — a queue can be
/// *ended*/*shut down*, which surfaces as a sentinel in that channel.
module EffSharp.AiDocs.Queues

open Effect
open EffSharp.AiDocs.Shared

// Offer a batch, then drain it. With `bounded` capacity, `offer` would suspend a
// producer fiber once the buffer is full — that's backpressure for free.
let produceConsume: Effect<int list, obj, unit> =
    effect {
        let! queue = Queue.bounded 8
        let! _leftovers = Queue.offerAll queue [ 1; 2; 3; 4; 5 ]
        // `takeN` pulls exactly n items (suspending until they're available).
        return! Queue.takeN queue 5
    }

// Each value goes to ONE taker. Two takers split the work between them.
let workSharing: Effect<int * int, obj, unit> =
    effect {
        let! queue = Queue.unbounded ()
        let! _ = Queue.offerAll queue [ 10; 20 ]
        let! first = Queue.take queue // taker A gets 10
        let! second = Queue.take queue // taker B gets 20 (NOT a copy of 10)
        return first, second
    }

let demo () =
    banner "09 · Queues — work distribution"
    printfn "  produce/consume => %A" (run produceConsume)
    printfn "  work sharing    => %A" (run workSharing)
