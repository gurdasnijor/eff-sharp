/// @title Streams — the `stream { }` computation expression
///
/// A `Stream<'A,'E,'R>` is a *pull-of-many* effectful values: like an `Effect`
/// that can succeed with zero, one, or many elements over time. eff-sharp's
/// headline ergonomic win over Effect's TypeScript is the **`stream { }`
/// computation expression**: you author a producer with native `yield` /
/// `yield!` / `for … do`, exactly like an F# `seq { }`, and it lowers onto the
/// same stack-safe `Effect` core.
///
/// Mirrors upstream ai-docs `02_stream`. Element-at-a-time (chunking / Sink /
/// concurrency are a later slice), which is plenty to teach the model.
module EffSharp.AiDocs.Stream02

open Effect
open EffSharp.AiDocs.Demo

// ───────────────────────────────────────────────────────────────────────────
// 1. The CE vs. the combinator chain — the same stream, two ways
// ───────────────────────────────────────────────────────────────────────────

/// Built with combinators. This is the ONLY style Effect's TS offers: every
/// step is a function call you thread together.
let combinatorStyle : Stream<int, string, unit> =
    Stream.range 1 3                         // 1,2,3
    |> Stream.concat (Stream.fromIterable [ 10; 20 ])   // … then 10,20
    |> Stream.map (fun x -> x * x)           // square each

/// The SAME stream, authored with the `stream { }` CE. `yield` emits one
/// element; `yield!` splices in another whole stream; `for … do yield` maps over
/// a collection. Reads top-to-bottom like a generator — no pipe threading.
let ceStyle : Stream<int, string, unit> =
    stream {
        for x in [ 1; 2; 3 ] do
            yield x * x          // yield squares of 1..3
        yield! stream { for y in [ 10; 20 ] do yield y * y } // splice another stream
    }

// ───────────────────────────────────────────────────────────────────────────
// 2. Transformations: map / filter / take / flatMap
// ───────────────────────────────────────────────────────────────────────────

/// `flatMap` expands each element into a sub-stream and flattens — here every n
/// becomes the pair (n, n*10). `take` stops the producer early (important for
/// infinite producers: it raises an internal stop signal caught at the boundary).
let pipeline : Stream<int, string, unit> =
    Stream.range 1 100
    |> Stream.filter (fun n -> n % 2 = 0)            // keep evens
    |> Stream.flatMap (fun n -> Stream.fromIterable [ n; n * 10 ])
    |> Stream.take 6                                 // 2,20,4,40,6,60

// ───────────────────────────────────────────────────────────────────────────
// 3. Effectful streams: `mapEffect` and `tap` run an Effect per element
// ───────────────────────────────────────────────────────────────────────────

/// `tap` runs an effect for each element and passes the element through
/// unchanged — the idiomatic place for logging/metrics inside a stream.
/// `mapEffect` runs an effect per element and emits its *result*.
let effectful : Stream<string, string, unit> =
    Stream.range 1 3
    |> Stream.tap (fun n -> Effect.sync (fun () -> printfn "    processing %d" n))
    |> Stream.mapEffect (fun n -> Effect.succeed (sprintf "item-%d" n))

let run () : unit =
    section "02 Stream — stream { } CE"

    // `runCollect` drains the stream into a list inside an Effect; `runUnit`
    // executes that effect (env = unit, no services needed).
    printfn "combinator style : %A" (Stream.runCollect combinatorStyle |> runUnit)
    printfn "stream{} CE      : %A" (Stream.runCollect ceStyle |> runUnit)
    printfn "filter/flatMap/take: %A" (Stream.runCollect pipeline |> runUnit)

    printfn "effectful stream (tap logs, mapEffect transforms):"
    let collected = Stream.runCollect effectful |> runUnit
    printfn "  -> %A" collected
