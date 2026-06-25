/// @title Shared state — Ref and SynchronizedRef
///
/// `Ref` is an atomic mutable cell you read/write inside effects. `SynchronizedRef`
/// adds an **effectful, serialized** critical section (so a read-modify-write that
/// itself runs an effect stays race-free).
///
/// Mirrors upstream state-management docs. The power demo below: fork 50 fibers
/// that each increment a `SynchronizedRef` — the serialized update means the
/// final total is exactly 50, no lost updates.
module EffSharp.AiDocs.State

open Effect
open EffSharp.AiDocs.Shared

// ---------------------------------------------------------------------------
// Ref — plain atomic cell. `modify` returns a value AND the next state.
// ---------------------------------------------------------------------------

let counterRef: Effect<int * int, string, unit> =
    effect {
        let! r = Ref.make 0
        do! Ref.update r (fun n -> n + 10)
        let! previous = Ref.getAndUpdate r (fun n -> n + 5) // returns old, stores new
        let! current = Ref.get r
        return previous, current // (10, 15)
    }

// ---------------------------------------------------------------------------
// SynchronizedRef — race-free under concurrency.  ⭐
// 50 fibers increment the same cell; `update` serializes them via a permit, so
// no increment is lost. (Try this with a naive get-then-set and you'd lose some.)
// ---------------------------------------------------------------------------

let concurrentIncrements: Effect<int, string, unit> =
    effect {
        let! cell = SynchronizedRef.make 0

        // Fork 50 incrementers...
        let! fibers = Effect.forEach [ 1..50 ] (fun _ -> Effect.fork (SynchronizedRef.update cell (fun n -> n + 1)))

        // ...then join them all so we observe the settled value.
        do! Effect.forEach fibers Effect.join |> Effect.map ignore

        return! SynchronizedRef.get cell // exactly 50
    }

// `SynchronizedRef.modifyEffect` runs an EFFECT inside the critical section —
// the update is atomic even though it does effectful work mid-flight.
let atomicEffectfulSwap: Effect<string, string, unit> =
    effect {
        let! cell = SynchronizedRef.make "initial"

        return!
            SynchronizedRef.modifyEffect cell (fun current ->
                effect {
                    // imagine an async fetch here; the cell is locked until we return
                    let next = current + " -> updated"
                    return current, next
                })
    }

let demo () =
    banner "08 · State — Ref / SynchronizedRef"
    printfn "  Ref modify          => %A" (run counterRef)
    printfn "  50 fibers increment => %d" (run concurrentIncrements)
    printfn "  atomic effectful    => %s" (run atomicEffectfulSwap)
