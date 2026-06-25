module Workloads

// eff-sharp's generated-JS counterpart to js-bench/bench.mjs (stock Effect). The
// four workloads and their N values are kept identical to bench.mjs and to
// benchmarks/Effect.Benchmarks/RuntimeBenchmarks.fs so the comparison is
// apples-to-apples on the workload. Compiled to JS via Fable; compare.mjs imports
// the exported run* functions and times them with tinybench on the same Node as
// stock Effect — a fair same-runtime comparison (unlike the .NET-vs-JS table in
// the old RESULTS.md).

open Effect
open Fable.Core // Async.StartAsPromise extension (Fable.Core.Extensions)

// Keep identical to bench.mjs / RuntimeBenchmarks.fs.
let bindN = 10000
let mapN = 10000
let refN = 10000
let forkN = 1000

// --- build programs once at module load (matches the F# GlobalSetup) ---

// bind throughput: a left-nested chain of N flatMaps.
let private bindProgram: Effect<int, string, unit> =
    let mutable e: Effect<int, string, unit> = Effect.succeed 0

    for _ in 1..bindN do
        e <- e |> Effect.flatMap (fun x -> Effect.succeed (x + 1))

    e

// succeed/map: N maps over a succeed.
let private mapProgram: Effect<int, string, unit> =
    let mutable e: Effect<int, string, unit> = Effect.succeed 0

    for _ in 1..mapN do
        e <- e |> Effect.map ((+) 1)

    e

// Ref: N update/get pairs in one effect.
let private refProgram: Effect<int, string, unit> =
    effect {
        let! r = Ref.make 0

        for _ in 1..refN do
            do! Ref.update r ((+) 1)
            let! _ = Ref.get r
            return ()

        return! Ref.get r
    }

// fork/join: fork + join a trivial fiber, N times.
let private forkProgram: Effect<unit, string, unit> =
    effect {
        for _ in 1..forkN do
            let! fib = Effect.fork (Effect.succeed 1: Effect<int, string, unit>)
            let! _ = Effect.join fib
            return ()
    }

// --- exported runners (Fable emits these as ES module functions) ---
//
// All four go through the async/Promise runner. Under Fable, `runSync` only
// completes *shallow* synchronous effects: F#'s Async trampolines deep (10k-long)
// flatMap/map chains with an async hop to stay stack-safe, which `runSync` (unable
// to block JS's event loop) reports as "suspended". The Promise path runs them to
// completion. compare.mjs drives stock Effect through `runPromise` likewise, so
// both sides are measured through their async runner — apples-to-apples.

/// bind throughput — resolves to the final accumulator (expect bindN).
let runBindAsync () =
    Effect.runAsync () bindProgram |> Async.StartAsPromise

/// succeed/map — resolves to the final accumulator (expect mapN).
let runMapAsync () =
    Effect.runAsync () mapProgram |> Async.StartAsPromise

/// Ref update/get — resolves to the final counter (expect refN).
let runRefAsync () =
    Effect.runAsync () refProgram |> Async.StartAsPromise

/// fork/join — fork + join a trivial fiber forkN times.
let runForkAsync () =
    Effect.runAsync () forkProgram |> Async.StartAsPromise
