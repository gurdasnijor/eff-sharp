module MetricSpec

open Effect
open Effect.Vitest

let private attrs = [ "x", "a"; "y", "b" ]

let private same actual expected = toBe (actual = expected) true

let private run (eff: Effect<'A, string, unit>) : 'A =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure c -> failwithf "unexpected failure: %s" (Cause.render c)

let private drain (eff: Effect<'A, 'E, unit>) : unit = Effect.runSync () eff |> ignore

let private runIgnore (eff: Effect<'A, string, unit>) : unit = run eff |> ignore

let private makeBuckets (boundaries: float list) (values: float list) : (float * int) list =
    let vs = values |> List.sort |> List.toArray
    let mutable count = 0
    let mutable idx = 0

    [ for b in boundaries do
          while idx < vs.Length && vs.[idx] <= b do
              count <- count + 1
              idx <- idx + 1

          yield (b, count) ]

describe "Metric" (fun () ->
    test "counters with the same id and attributes share state" (fun () ->
        Metric.resetUnsafe ()

        let counter1 =
            Metric.withConstantInput (Metric.withAttributes (Metric.counter "rt") attrs) 1.0

        let counter2 =
            Metric.withConstantInput (Metric.withAttributes (Metric.counter "rt") attrs) 1.0

        let counter3 =
            Metric.withConstantInput (Metric.withAttributes (Metric.counter "rt") [ "z", "c" ]) 1.0

        runIgnore (Metric.track (Effect.succeed 0.0) counter1)
        runIgnore (Metric.track (Effect.succeed 0.0) counter2)
        runIgnore (Metric.track (Effect.succeed 0.0) counter3)

        same (run (Metric.value counter1)) { Count = 2.0; Incremental = false }
        same (run (Metric.value counter2)) { Count = 2.0; Incremental = false }
        same (run (Metric.value counter3)) { Count = 1.0; Incremental = false })

    test "dump renders the registry" (fun () ->
        Metric.resetUnsafe ()

        let counter1 =
            Metric.withConstantInput (Metric.withAttributes (Metric.counter "counter") attrs) 1.0

        let counter2 =
            Metric.withConstantInput (Metric.withAttributes (Metric.counter "counter") attrs) 1.0

        let counter3 =
            Metric.withConstantInput (Metric.withAttributes (Metric.counter "counter") [ "z", "c" ]) 1.0

        runIgnore (Metric.track (Effect.succeed 0.0) counter1)
        runIgnore (Metric.track (Effect.succeed 0.0) counter2)
        runIgnore (Metric.track (Effect.succeed 0.0) counter3)

        let expected =
            "name=counter  description=  type=Counter  attributes=[x: a, y: b]  state=[count: [2]]\n"
            + "name=counter  description=  type=Counter  attributes=[z: c]        state=[count: [1]]"

        toBe (run (Metric.dump ())) expected)

    test "counter increments with value" (fun () ->
        Metric.resetUnsafe ()
        let counter = Metric.withAttributes (Metric.counter "c-iv") attrs
        runIgnore (Metric.trackSuccesses (Effect.succeed 1.0) counter)
        runIgnore (Metric.trackSuccesses (Effect.succeed 2.0) counter)
        same (run (Metric.value counter)) { Count = 3.0; Incremental = false })

    test "counter increments with constant" (fun () ->
        Metric.resetUnsafe ()

        let counter =
            Metric.withConstantInput (Metric.withAttributes (Metric.counter "c-ic") attrs) 1.0

        runIgnore (Metric.trackSuccesses (Effect.succeed 1.0) counter)
        runIgnore (Metric.trackSuccesses (Effect.succeed 2.0) counter)
        same (run (Metric.value counter)) { Count = 2.0; Incremental = false })

    test "counter decrements with value" (fun () ->
        Metric.resetUnsafe ()
        let counter = Metric.withAttributes (Metric.counter "c-dv") attrs
        runIgnore (Metric.trackSuccesses (Effect.succeed -1.0) counter)
        runIgnore (Metric.trackSuccesses (Effect.succeed -2.0) counter)
        same (run (Metric.value counter)) { Count = -3.0; Incremental = false })

    test "counter decrements with constant" (fun () ->
        Metric.resetUnsafe ()

        let counter =
            Metric.withConstantInput (Metric.withAttributes (Metric.counter "c-dc") attrs) -1.0

        runIgnore (Metric.trackSuccesses (Effect.succeed -1.0) counter)
        runIgnore (Metric.trackSuccesses (Effect.succeed -2.0) counter)
        same (run (Metric.value counter)) { Count = -2.0; Incremental = false })

    test "incremental counter ignores decrements" (fun () ->
        Metric.resetUnsafe ()

        let counter =
            Metric.withConstantInput (Metric.withAttributes (Metric.counterIncremental "c-inc") attrs) -1.0

        runIgnore (Metric.trackSuccesses (Effect.succeed -1.0) counter)
        runIgnore (Metric.trackSuccesses (Effect.succeed -2.0) counter)
        same (run (Metric.value counter)) { Count = 0.0; Incremental = true })

    test "gauge sets with value" (fun () ->
        Metric.resetUnsafe ()
        let gauge = Metric.withAttributes (Metric.gauge "g-v") attrs
        runIgnore (Metric.trackSuccesses (Effect.succeed 1.0) gauge)
        runIgnore (Metric.trackSuccesses (Effect.succeed 2.0) gauge)
        same (run (Metric.value gauge)) { GaugeState.Value = 2.0 })

    test "gauge sets with constant" (fun () ->
        Metric.resetUnsafe ()

        let gauge =
            Metric.withConstantInput (Metric.withAttributes (Metric.gauge "g-c") attrs) 1.0

        runIgnore (Metric.trackSuccesses (Effect.succeed 1.0) gauge)
        runIgnore (Metric.trackSuccesses (Effect.succeed 2.0) gauge)
        same (run (Metric.value gauge)) { GaugeState.Value = 1.0 })

    test "gauge sets with negative value" (fun () ->
        Metric.resetUnsafe ()
        let gauge = Metric.withAttributes (Metric.gauge "g-nv") attrs
        runIgnore (Metric.trackSuccesses (Effect.succeed -1.0) gauge)
        runIgnore (Metric.trackSuccesses (Effect.succeed -2.0) gauge)
        same (run (Metric.value gauge)) { GaugeState.Value = -2.0 })

    test "frequency counts occurrences" (fun () ->
        Metric.resetUnsafe ()
        let frequency = Metric.withAttributes (Metric.frequency "f-v") attrs
        runIgnore (Metric.trackSuccesses (Effect.succeed "foo") frequency)
        runIgnore (Metric.trackSuccesses (Effect.succeed "bar") frequency)
        same (run (Metric.value frequency)) { Occurrences = Map.ofList [ "foo", 1; "bar", 1 ] })

    test "frequency with constant input" (fun () ->
        Metric.resetUnsafe ()

        let frequency =
            Metric.withConstantInput (Metric.withAttributes (Metric.frequency "f-c") attrs) "constant"

        runIgnore (Metric.trackSuccesses (Effect.succeed "foo") frequency)
        runIgnore (Metric.trackSuccesses (Effect.succeed "bar") frequency)
        same (run (Metric.value frequency)) { Occurrences = Map.ofList [ "constant", 2 ] })

    test "histogram observes with value" (fun () ->
        Metric.resetUnsafe ()
        let boundaries = Metric.linearBoundaries 0.0 1.0 10
        let histogram = Metric.withAttributes (Metric.histogram "h-v" boundaries) attrs
        runIgnore (Metric.trackSuccesses (Effect.succeed 1.0) histogram)
        runIgnore (Metric.trackSuccesses (Effect.succeed 3.0) histogram)
        let result = run (Metric.value histogram)
        same result.Buckets (makeBuckets boundaries [ 1.0; 3.0 ])
        toBe result.Count 2
        toBe result.Sum 4.0
        toBe result.Min 1.0
        toBe result.Max 3.0)

    test "histogram observes with constant" (fun () ->
        Metric.resetUnsafe ()
        let boundaries = Metric.linearBoundaries 0.0 1.0 10

        let histogram =
            Metric.withConstantInput (Metric.withAttributes (Metric.histogram "h-c" boundaries) attrs) 1.0

        runIgnore (Metric.trackSuccesses (Effect.succeed 1.0) histogram)
        runIgnore (Metric.trackSuccesses (Effect.succeed 3.0) histogram)
        let result = run (Metric.value histogram)
        same result.Buckets (makeBuckets boundaries [ 1.0; 1.0 ])
        toBe result.Count 2
        toBe result.Sum 2.0)

    test "histogram preserves precision of boundary values" (fun () ->
        Metric.resetUnsafe ()
        let boundaries = [ 0.005; 0.01; 0.025; 0.05; 0.075; 0.1 ]
        let histogram = Metric.histogram "h-precision" boundaries
        let result = run (Metric.value histogram)
        boundaries |> List.iteri (fun i b -> toBe (fst result.Buckets.[i]) b))

    test "summary observes with value" (fun () ->
        Metric.resetUnsafe ()

        let summary =
            Metric.withAttributes (Metric.summary "s-v" 60000.0 10 [ 0.0; 0.1; 0.9 ]) attrs

        runIgnore (Metric.trackSuccesses (Effect.succeed 1.0) summary)
        runIgnore (Metric.trackSuccesses (Effect.succeed 3.0) summary)
        let result = run (Metric.value summary)
        same result.Quantiles [ (0.0, Some 1.0); (0.1, Some 1.0); (0.9, Some 3.0) ]
        toBe result.Count 2
        toBe result.Sum 4.0
        toBe result.Min 1.0
        toBe result.Max 3.0)

    test "summary quantile when the first chunk overshoots" (fun () ->
        Metric.resetUnsafe ()
        let summary = Metric.summary "s-overshoot" 60000.0 15 [ 0.5 ]
        let samples = [ 10.0; 10.0; 10.0; 10.0; 10.0; 10.0; 20.0; 30.0; 40.0; 50.0 ]
        runIgnore (Effect.forEach samples (fun s -> Metric.update summary s) |> Effect.map ignore)
        let result = run (Metric.value summary)
        same result.Quantiles [ (0.5, Some 10.0) ]
        toBe result.Count 10
        toBe result.Min 10.0
        toBe result.Max 50.0
        toBe result.Sum 200.0)

    test "track updates on success failure and defect" (fun () ->
        Metric.resetUnsafe ()
        let onSuccess = Metric.withConstantInput (Metric.counter "t-s") 1.0
        runIgnore (Metric.track (Effect.succeed 1.0) onSuccess)
        same (run (Metric.value onSuccess)) { Count = 1.0; Incremental = false }

        let onFailure = Metric.withConstantInput (Metric.counter "t-f") 1.0
        drain (Metric.track (Effect.fail 1) onFailure)
        same (run (Metric.value onFailure)) { Count = 1.0; Incremental = false }

        let onDefect = Metric.withConstantInput (Metric.counter "t-d") 1.0
        drain (Metric.track (Effect.failCause (Cause.die (box 1))) onDefect)
        same (run (Metric.value onDefect)) { Count = 1.0; Incremental = false })

    test "trackErrors only updates on typed failure" (fun () ->
        Metric.resetUnsafe ()
        let onSuccess = Metric.withConstantInput (Metric.counter "te-s") 1.0
        runIgnore (Metric.trackErrors (Effect.succeed 1.0) onSuccess)
        same (run (Metric.value onSuccess)) { Count = 0.0; Incremental = false }

        let onFailure = Metric.withConstantInput (Metric.counter "te-f") 1.0
        drain (Metric.trackErrors (Effect.fail 1) onFailure)
        same (run (Metric.value onFailure)) { Count = 1.0; Incremental = false }

        let onDefect = Metric.withConstantInput (Metric.counter "te-d") 1.0
        drain (Metric.trackErrors (Effect.failCause (Cause.die (box 1))) onDefect)
        same (run (Metric.value onDefect)) { Count = 0.0; Incremental = false })

    test "trackDefects only updates on defect" (fun () ->
        Metric.resetUnsafe ()
        let onSuccess = Metric.withConstantInput (Metric.counter "td-s") 1.0
        runIgnore (Metric.trackDefects (Effect.succeed 1.0) onSuccess)
        same (run (Metric.value onSuccess)) { Count = 0.0; Incremental = false }

        let onFailure = Metric.withConstantInput (Metric.counter "td-f") 1.0
        drain (Metric.trackDefects (Effect.fail 1) onFailure)
        same (run (Metric.value onFailure)) { Count = 0.0; Incremental = false }

        let onDefect = Metric.withConstantInput (Metric.counter "td-d") 1.0
        drain (Metric.trackDefects (Effect.failCause (Cause.die (box 1))) onDefect)
        same (run (Metric.value onDefect)) { Count = 1.0; Incremental = false }))
