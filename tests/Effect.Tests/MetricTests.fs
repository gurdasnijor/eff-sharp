module Effect.Tests.MetricTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/Metric.test.ts.
//
// `Effect.track*` live on the `Metric` module in this port (scope: Metric files
// only). Unique ids per test isolate state in the process-global registry; the
// `dump` test clears it first (mirroring upstream's fresh `MetricRegistry` Map).
// Deferred (need TestClock / fiber-runtime hooks / JS BigInt): bigint counters &
// gauges, `timer`/`trackDuration`, and `FiberRuntimeMetrics`.

let private attrs = [ "x", "a"; "y", "b" ]

let private run (eff: Effect<'A, string, unit>) : 'A =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure c -> failwithf "unexpected failure: %s" (Cause.render c)

/// Run an effect that may fail, discarding its outcome (the metric side effect
/// has already happened).
let private drain (eff: Effect<'A, 'E, unit>) : unit = Effect.runSync () eff |> ignore

// --- referential transparency ---

[<Fact>]
let ``counters with the same id and attributes share state`` () =
    let counter1 =
        Metric.withConstantInput (Metric.withAttributes (Metric.counter "rt") attrs) 1.0

    let counter2 =
        Metric.withConstantInput (Metric.withAttributes (Metric.counter "rt") attrs) 1.0

    let counter3 =
        Metric.withConstantInput (Metric.withAttributes (Metric.counter "rt") [ "z", "c" ]) 1.0

    run (Metric.track (Effect.succeed 0.0) counter1)
    run (Metric.track (Effect.succeed 0.0) counter2)
    run (Metric.track (Effect.succeed 0.0) counter3)

    Assert.Equal({ Count = 2.0; Incremental = false }, run (Metric.value counter1))
    Assert.Equal({ Count = 2.0; Incremental = false }, run (Metric.value counter2))
    Assert.Equal({ Count = 1.0; Incremental = false }, run (Metric.value counter3))

// --- dump ---

[<Fact>]
let ``dump renders the registry`` () =
    Metric.resetUnsafe ()

    let counter1 =
        Metric.withConstantInput (Metric.withAttributes (Metric.counter "counter") attrs) 1.0

    let counter2 =
        Metric.withConstantInput (Metric.withAttributes (Metric.counter "counter") attrs) 1.0

    let counter3 =
        Metric.withConstantInput (Metric.withAttributes (Metric.counter "counter") [ "z", "c" ]) 1.0

    run (Metric.track (Effect.succeed 0.0) counter1)
    run (Metric.track (Effect.succeed 0.0) counter2)
    run (Metric.track (Effect.succeed 0.0) counter3)

    let expected =
        "name=counter  description=  type=Counter  attributes=[x: a, y: b]  state=[count: [2]]\n"
        + "name=counter  description=  type=Counter  attributes=[z: c]        state=[count: [1]]"

    Assert.Equal(expected, run (Metric.dump ()))

// --- Counter ---

[<Fact>]
let ``counter increments with value`` () =
    let counter = Metric.withAttributes (Metric.counter "c-iv") attrs
    run (Metric.trackSuccesses (Effect.succeed 1.0) counter)
    run (Metric.trackSuccesses (Effect.succeed 2.0) counter)
    Assert.Equal({ Count = 3.0; Incremental = false }, run (Metric.value counter))

[<Fact>]
let ``counter increments with constant`` () =
    let counter =
        Metric.withConstantInput (Metric.withAttributes (Metric.counter "c-ic") attrs) 1.0

    run (Metric.trackSuccesses (Effect.succeed 1.0) counter)
    run (Metric.trackSuccesses (Effect.succeed 2.0) counter)
    Assert.Equal({ Count = 2.0; Incremental = false }, run (Metric.value counter))

[<Fact>]
let ``counter decrements with value`` () =
    let counter = Metric.withAttributes (Metric.counter "c-dv") attrs
    run (Metric.trackSuccesses (Effect.succeed -1.0) counter)
    run (Metric.trackSuccesses (Effect.succeed -2.0) counter)
    Assert.Equal({ Count = -3.0; Incremental = false }, run (Metric.value counter))

[<Fact>]
let ``counter decrements with constant`` () =
    let counter =
        Metric.withConstantInput (Metric.withAttributes (Metric.counter "c-dc") attrs) -1.0

    run (Metric.trackSuccesses (Effect.succeed -1.0) counter)
    run (Metric.trackSuccesses (Effect.succeed -2.0) counter)
    Assert.Equal({ Count = -2.0; Incremental = false }, run (Metric.value counter))

[<Fact>]
let ``incremental counter ignores decrements`` () =
    let counter =
        Metric.withConstantInput (Metric.withAttributes (Metric.counterIncremental "c-inc") attrs) -1.0

    run (Metric.trackSuccesses (Effect.succeed -1.0) counter)
    run (Metric.trackSuccesses (Effect.succeed -2.0) counter)
    Assert.Equal({ Count = 0.0; Incremental = true }, run (Metric.value counter))

// --- Gauge ---

[<Fact>]
let ``gauge sets with value`` () =
    let gauge = Metric.withAttributes (Metric.gauge "g-v") attrs
    run (Metric.trackSuccesses (Effect.succeed 1.0) gauge)
    run (Metric.trackSuccesses (Effect.succeed 2.0) gauge)
    Assert.Equal({ GaugeState.Value = 2.0 }, run (Metric.value gauge))

[<Fact>]
let ``gauge sets with constant`` () =
    let gauge =
        Metric.withConstantInput (Metric.withAttributes (Metric.gauge "g-c") attrs) 1.0

    run (Metric.trackSuccesses (Effect.succeed 1.0) gauge)
    run (Metric.trackSuccesses (Effect.succeed 2.0) gauge)
    Assert.Equal({ GaugeState.Value = 1.0 }, run (Metric.value gauge))

[<Fact>]
let ``gauge sets with negative value`` () =
    let gauge = Metric.withAttributes (Metric.gauge "g-nv") attrs
    run (Metric.trackSuccesses (Effect.succeed -1.0) gauge)
    run (Metric.trackSuccesses (Effect.succeed -2.0) gauge)
    Assert.Equal({ GaugeState.Value = -2.0 }, run (Metric.value gauge))

// --- Frequency ---

[<Fact>]
let ``frequency counts occurrences`` () =
    let frequency = Metric.withAttributes (Metric.frequency "f-v") attrs
    run (Metric.trackSuccesses (Effect.succeed "foo") frequency)
    run (Metric.trackSuccesses (Effect.succeed "bar") frequency)
    Assert.Equal({ Occurrences = Map.ofList [ "foo", 1; "bar", 1 ] }, run (Metric.value frequency))

[<Fact>]
let ``frequency with constant input`` () =
    let frequency =
        Metric.withConstantInput (Metric.withAttributes (Metric.frequency "f-c") attrs) "constant"

    run (Metric.trackSuccesses (Effect.succeed "foo") frequency)
    run (Metric.trackSuccesses (Effect.succeed "bar") frequency)
    Assert.Equal({ Occurrences = Map.ofList [ "constant", 2 ] }, run (Metric.value frequency))

// --- Histogram ---

let private makeBuckets (boundaries: float list) (values: float list) : (float * int) list =
    let vs = values |> List.sort |> List.toArray
    let mutable count = 0
    let mutable idx = 0

    [ for b in boundaries do
          while idx < vs.Length && vs.[idx] <= b do
              count <- count + 1
              idx <- idx + 1

          yield (b, count) ]

[<Fact>]
let ``histogram observes with value`` () =
    let boundaries = Metric.linearBoundaries 0.0 1.0 10
    let histogram = Metric.withAttributes (Metric.histogram "h-v" boundaries) attrs
    run (Metric.trackSuccesses (Effect.succeed 1.0) histogram)
    run (Metric.trackSuccesses (Effect.succeed 3.0) histogram)
    let result = run (Metric.value histogram)
    Assert.Equal<(float * int) list>(makeBuckets boundaries [ 1.0; 3.0 ], result.Buckets)
    Assert.Equal(2, result.Count)
    Assert.Equal(4.0, result.Sum)
    Assert.Equal(1.0, result.Min)
    Assert.Equal(3.0, result.Max)

[<Fact>]
let ``histogram observes with constant`` () =
    let boundaries = Metric.linearBoundaries 0.0 1.0 10

    let histogram =
        Metric.withConstantInput (Metric.withAttributes (Metric.histogram "h-c" boundaries) attrs) 1.0

    run (Metric.trackSuccesses (Effect.succeed 1.0) histogram)
    run (Metric.trackSuccesses (Effect.succeed 3.0) histogram)
    let result = run (Metric.value histogram)
    Assert.Equal<(float * int) list>(makeBuckets boundaries [ 1.0; 1.0 ], result.Buckets)
    Assert.Equal(2, result.Count)
    Assert.Equal(2.0, result.Sum)

[<Fact>]
let ``histogram preserves precision of boundary values`` () =
    let boundaries = [ 0.005; 0.01; 0.025; 0.05; 0.075; 0.1 ]
    let histogram = Metric.histogram "h-precision" boundaries
    let result = run (Metric.value histogram)
    boundaries |> List.iteri (fun i b -> Assert.Equal(b, fst result.Buckets.[i]))

// --- Summary ---

[<Fact>]
let ``summary observes with value`` () =
    let summary =
        Metric.withAttributes (Metric.summary "s-v" 60000.0 10 [ 0.0; 0.1; 0.9 ]) attrs

    run (Metric.trackSuccesses (Effect.succeed 1.0) summary)
    run (Metric.trackSuccesses (Effect.succeed 3.0) summary)
    let result = run (Metric.value summary)
    Assert.Equal<(float * float option) list>([ (0.0, Some 1.0); (0.1, Some 1.0); (0.9, Some 3.0) ], result.Quantiles)
    Assert.Equal(2, result.Count)
    Assert.Equal(4.0, result.Sum)
    Assert.Equal(1.0, result.Min)
    Assert.Equal(3.0, result.Max)

[<Fact>]
let ``summary quantile when the first chunk overshoots`` () =
    let summary = Metric.summary "s-overshoot" 60000.0 15 [ 0.5 ]
    let samples = [ 10.0; 10.0; 10.0; 10.0; 10.0; 10.0; 20.0; 30.0; 40.0; 50.0 ]
    run (Effect.forEach samples (fun s -> Metric.update summary s) |> Effect.map ignore)
    let result = run (Metric.value summary)
    Assert.Equal<(float * float option) list>([ (0.5, Some 10.0) ], result.Quantiles)
    Assert.Equal(10, result.Count)
    Assert.Equal(10.0, result.Min)
    Assert.Equal(50.0, result.Max)
    Assert.Equal(200.0, result.Sum)

// --- track / trackErrors / trackDefects ---

[<Fact>]
let ``track updates on success, failure and defect`` () =
    let onSuccess = Metric.withConstantInput (Metric.counter "t-s") 1.0
    run (Metric.track (Effect.succeed 1.0) onSuccess)
    Assert.Equal({ Count = 1.0; Incremental = false }, run (Metric.value onSuccess))

    let onFailure = Metric.withConstantInput (Metric.counter "t-f") 1.0
    drain (Metric.track (Effect.fail 1) onFailure)
    Assert.Equal({ Count = 1.0; Incremental = false }, run (Metric.value onFailure))

    let onDefect = Metric.withConstantInput (Metric.counter "t-d") 1.0
    drain (Metric.track (Effect.failCause (Cause.die (box 1))) onDefect)
    Assert.Equal({ Count = 1.0; Incremental = false }, run (Metric.value onDefect))

[<Fact>]
let ``trackErrors only updates on typed failure`` () =
    let onSuccess = Metric.withConstantInput (Metric.counter "te-s") 1.0
    run (Metric.trackErrors (Effect.succeed 1.0) onSuccess)
    Assert.Equal({ Count = 0.0; Incremental = false }, run (Metric.value onSuccess))

    let onFailure = Metric.withConstantInput (Metric.counter "te-f") 1.0
    drain (Metric.trackErrors (Effect.fail 1) onFailure)
    Assert.Equal({ Count = 1.0; Incremental = false }, run (Metric.value onFailure))

    let onDefect = Metric.withConstantInput (Metric.counter "te-d") 1.0
    drain (Metric.trackErrors (Effect.failCause (Cause.die (box 1))) onDefect)
    Assert.Equal({ Count = 0.0; Incremental = false }, run (Metric.value onDefect))

[<Fact>]
let ``trackDefects only updates on defect`` () =
    let onSuccess = Metric.withConstantInput (Metric.counter "td-s") 1.0
    run (Metric.trackDefects (Effect.succeed 1.0) onSuccess)
    Assert.Equal({ Count = 0.0; Incremental = false }, run (Metric.value onSuccess))

    let onFailure = Metric.withConstantInput (Metric.counter "td-f") 1.0
    drain (Metric.trackDefects (Effect.fail 1) onFailure)
    Assert.Equal({ Count = 0.0; Incremental = false }, run (Metric.value onFailure))

    let onDefect = Metric.withConstantInput (Metric.counter "td-d") 1.0
    drain (Metric.trackDefects (Effect.failCause (Cause.die (box 1))) onDefect)
    Assert.Equal({ Count = 1.0; Incremental = false }, run (Metric.value onDefect))
