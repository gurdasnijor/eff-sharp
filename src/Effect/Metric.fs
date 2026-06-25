namespace Effect

open System.Collections.Generic

/// Wave 6: `Metric` — counters, gauges, frequencies, histograms and summaries
/// with attributes.
///
/// Port of repos/effect-smol/packages/effect/src/Metric.ts. A metric is an
/// immutable descriptor; updating or reading it resolves a shared, mutable
/// *hook* from a registry, keyed by `(type, id, description, attributes)`. Two
/// descriptors with the same key therefore share state (referential
/// transparency), while differing attributes create separate series.
///
/// Representation notes / deviations (per CONVENTIONS):
///   * The registry is a process-global default `Dictionary` (upstream resolves
///     a `MetricRegistry` from the `Context`, falling back to a shared default).
///     `resetUnsafe` clears it — used by tests to mirror "provide a fresh Map".
///   * `track`/`trackSuccesses`/`trackErrors`/`trackDefects` live here, not on
///     `Effect` (upstream puts them on `Effect`; this wave may only touch the
///     Metric files), as `Metric.track* effect metric`.
///   * Hook input/state are boxed internally (`obj`) — F# has no HKT, so one
///     registry holds heterogeneous metrics; the typed `Metric<'Input,'State>`
///     surface re-imposes safety.
///
/// Deferred (noted, not ported): `bigint` counters/gauges (JS BigInt); `timer` /
/// `Effect.trackDuration` and the `FiberRuntimeMetrics` service (need TestClock /
/// fiber-runtime hooks); the `Context`-provided `MetricRegistry` /
/// `CurrentMetricAttributes` references; `exponentialBoundaries`; `snapshot`.
/// `summary` takes `maxAge` in milliseconds rather than a `Duration.Input`.

// --- public state shapes (match upstream's deepStrictEqual targets) ---

type CounterState = { Count: float; Incremental: bool }
type GaugeState = { Value: float }
type FrequencyState = { Occurrences: Map<string, int> }

type HistogramState =
    { Buckets: (float * int) list
      Count: int
      Sum: float
      Min: float
      Max: float }

type SummaryState =
    { Quantiles: (float * float option) list
      Count: int
      Sum: float
      Min: float
      Max: float }

/// A mutable hook: heterogeneous, so input/state are boxed.
type internal MetricHook =
    { Get: unit -> obj
      Update: obj -> unit }

/// Registered metric metadata.
type internal MetricMetadata =
    { Type: string
      Id: string
      Description: string option
      Attributes: Map<string, string>
      Hook: MetricHook }

/// An immutable metric descriptor. `'Input` is the update type, `'State` the
/// read type. `MapInput`/`ReadState` bridge the boxed hook.
type Metric<'Input, 'State> =
    internal
        { Type: string
          Id: string
          Description: string option
          Attributes: Map<string, string>
          MapInput: obj -> obj
          ReadState: obj -> 'State
          CreateHook: unit -> MetricHook }

[<RequireQualifiedAccess>]
module Metric =

    // --- registry (process-global default) ---

    let private registry = Dictionary<string, MetricMetadata>()
    let private registryLock = obj ()

    /// Clear the default registry (test isolation; mirrors providing a fresh Map).
    let resetUnsafe () =
        lock registryLock (fun () -> registry.Clear())

    let private keyOf (m: Metric<'Input, 'State>) : string =
        let attrs =
            m.Attributes
            |> Seq.map (fun kv -> sprintf "%s=%s" kv.Key kv.Value)
            |> String.concat ","

        sprintf "%s|%s|%s|%s" m.Type m.Id (defaultArg m.Description "") attrs

    let private getOrCreate (m: Metric<'Input, 'State>) : MetricHook =
        lock registryLock (fun () ->
            let key = keyOf m

            match registry.TryGetValue key with
            | true, meta -> meta.Hook
            | _ ->
                let hook = m.CreateHook()

                registry.[key] <-
                    { Type = m.Type
                      Id = m.Id
                      Description = m.Description
                      Attributes = m.Attributes
                      Hook = hook }

                hook)

    let private updateBoxed (m: Metric<'Input, 'State>) (boxedInput: obj) =
        let hook = getOrCreate m
        hook.Update(m.MapInput boxedInput)

    // --- transformations ---

    /// Attach attributes, creating a separate series. (Metric.withAttributes)
    let withAttributes (m: Metric<'Input, 'State>) (attrs: (string * string) list) : Metric<'Input, 'State> =
        let merged = (m.Attributes, attrs) ||> List.fold (fun acc (k, v) -> Map.add k v acc)
        { m with Attributes = merged }

    /// Ignore the supplied input and always update with `input`.
    /// (Metric.withConstantInput)
    let withConstantInput (m: Metric<'Input, 'State>) (input: 'Input) : Metric<'Input, 'State> =
        { m with
            MapInput = fun _ -> m.MapInput(box input) }

    // --- read / update ---

    /// Read the current state. (Metric.value)
    let value (m: Metric<'Input, 'State>) : Effect<'State, 'E, 'R> =
        Effect.sync (fun () ->
            let hook = getOrCreate m
            m.ReadState(hook.Get()))

    /// Update the metric with `input`. (Metric.update)
    let update (m: Metric<'Input, 'State>) (input: 'Input) : Effect<unit, 'E, 'R> =
        Effect.sync (fun () -> updateBoxed m (box input))

    // --- tracking (upstream's Effect.track* — kept here for scope) ---

    let private withExit (effect: Effect<'A, 'E, 'R>) (onExit: Exit<'A, 'E> -> unit) : Effect<'A, 'E, 'R> =
        let (Effect run) = effect

        Effect(fun fib r ->
            async {
                let! exit = run fib r
                onExit exit
                return exit
            })

    /// Update on every exit (with the success value, or the metric's constant
    /// input on failure/defect). (Effect.track)
    let track (effect: Effect<'A, 'E, 'R>) (m: Metric<'A, 'State>) : Effect<'A, 'E, 'R> =
        withExit effect (fun exit ->
            let boxed =
                match exit with
                | Success a -> box a
                | Failure _ -> box (Unchecked.defaultof<'A>)

            updateBoxed m boxed)

    /// Update only on success, with the success value. (Effect.trackSuccesses)
    let trackSuccesses (effect: Effect<'A, 'E, 'R>) (m: Metric<'A, 'State>) : Effect<'A, 'E, 'R> =
        withExit effect (function
            | Success a -> updateBoxed m (box a)
            | Failure _ -> ())

    /// Update only on typed failure, with the error value. (Effect.trackErrors)
    let trackErrors (effect: Effect<'A, 'E, 'R>) (m: Metric<'M, 'State>) : Effect<'A, 'E, 'R> =
        withExit effect (function
            | Failure c ->
                match Cause.failures c with
                | e :: _ -> updateBoxed m (box e)
                | [] -> ()
            | Success _ -> ())

    /// Update only on defect, with the defect value. (Effect.trackDefects)
    let trackDefects (effect: Effect<'A, 'E, 'R>) (m: Metric<'M, 'State>) : Effect<'A, 'E, 'R> =
        withExit effect (function
            | Failure c ->
                match Cause.defects c with
                | d :: _ -> updateBoxed m d
                | [] -> ()
            | Success _ -> ())

    // --- boundaries ---

    /// Normalize boundaries: drop non-positive, dedupe, append +Infinity.
    /// (Metric.boundariesFromIterable)
    let boundariesFromIterable (xs: seq<float>) : float list =
        (xs |> Seq.filter (fun n -> n > 0.0) |> Seq.distinct |> List.ofSeq)
        @ [ System.Double.PositiveInfinity ]

    /// `count - 1` linear boundaries `start + i + width`, then `boundariesFromIterable`.
    /// (Metric.linearBoundaries)
    let linearBoundaries (start: float) (width: float) (count: int) : float list =
        boundariesFromIterable [ for n in 0 .. count - 2 -> start + float n + width ]

    // --- constructors ---

    let private counterHook (incremental: bool) () : MetricHook =
        let mutable count = 0.0

        { Get =
            fun () ->
                box
                    { Count = count
                      Incremental = incremental }
          Update =
            fun v ->
                let x = unbox<float> v

                if (not incremental) || x >= 0.0 then
                    count <- count + x }

    let private mk
        (typ: string)
        (id: string)
        (createHook: unit -> MetricHook)
        (readState: obj -> 'State)
        : Metric<'Input, 'State> =
        { Type = typ
          Id = id
          Description = None
          Attributes = Map.empty
          MapInput = (fun o -> o)
          ReadState = readState
          CreateHook = createHook }

    /// A non-incremental counter (adds each input). (Metric.counter)
    let counter (id: string) : Metric<float, CounterState> =
        mk "Counter" id (counterHook false) (fun o -> unbox<CounterState> o)

    /// An incremental counter (ignores negative updates). (Metric.counter incremental)
    let counterIncremental (id: string) : Metric<float, CounterState> =
        mk "Counter" id (counterHook true) (fun o -> unbox<CounterState> o)

    /// A gauge (set to the last input). (Metric.gauge)
    let gauge (id: string) : Metric<float, GaugeState> =
        let createHook () =
            let mutable v = 0.0

            { Get = fun () -> box { Value = v }
              Update = fun x -> v <- unbox<float> x }

        mk "Gauge" id createHook (fun o -> unbox<GaugeState> o)

    /// A frequency (counts occurrences of each string). (Metric.frequency)
    let frequency (id: string) : Metric<string, FrequencyState> =
        let createHook () =
            let occ = Dictionary<string, int>()

            { Get =
                fun () ->
                    let m = occ |> Seq.fold (fun acc kv -> Map.add kv.Key kv.Value acc) Map.empty
                    box { Occurrences = m }
              Update =
                fun w ->
                    let word = unbox<string> w

                    let c =
                        match occ.TryGetValue word with
                        | true, n -> n
                        | _ -> 0

                    occ.[word] <- c + 1 }

        mk "Frequency" id createHook (fun o -> unbox<FrequencyState> o)

    /// A histogram over `boundaries` (sorted ascending). (Metric.histogram)
    let histogram (id: string) (boundaries: float list) : Metric<float, HistogramState> =
        let createHook () =
            let bounds = boundaries |> List.sort |> List.toArray
            let size = bounds.Length
            let counts = Array.zeroCreate<int> (size + 1) // last slot = overflow
            let mutable count = 0
            let mutable sum = 0.0
            let mutable mn = System.Double.MaxValue
            let mutable mx = System.Double.MinValue

            { Get =
                fun () ->
                    let mutable cumulated = 0

                    let buckets =
                        [ for i in 0 .. size - 1 ->
                              cumulated <- cumulated + counts.[i]
                              (bounds.[i], cumulated) ]

                    box
                        { Buckets = buckets
                          Count = count
                          Sum = sum
                          Min = mn
                          Max = mx }
              Update =
                fun v ->
                    let x = unbox<float> v
                    let mutable i = 0

                    while i < size && x > bounds.[i] do
                        i <- i + 1

                    counts.[i] <- counts.[i] + 1 // i = size => overflow slot
                    count <- count + 1
                    sum <- sum + x

                    if x < mn then
                        mn <- x

                    if x > mx then
                        mx <- x }

        mk "Histogram" id createHook (fun o -> unbox<HistogramState> o)

    /// A summary computing quantiles over a sliding window. `maxAge` is in
    /// milliseconds. (Metric.summary)
    let summary
        (id: string)
        (maxAgeMillis: float)
        (maxSize: int)
        (quantiles: float list)
        : Metric<float, SummaryState> =
        let sortedQuantiles = List.sort quantiles

        let now () =
            float (System.DateTimeOffset.UtcNow.ToUnixTimeMilliseconds())

        let createHook () =
            let observations = Array.zeroCreate<(float * float) option> (max 0 maxSize)
            let mutable head = 0
            let mutable count = 0
            let mutable sum = 0.0
            let mutable mn = System.Double.MaxValue
            let mutable mx = System.Double.MinValue

            let snapshot (current: float) : (float * float option) list =
                let samples =
                    [ for o in observations do
                          match o with
                          | Some(ts, value) when current - ts >= 0.0 && current - ts <= maxAgeMillis -> yield value
                          | _ -> () ]
                    |> List.sort
                    |> List.toArray

                let n = samples.Length

                if n = 0 then
                    sortedQuantiles |> List.map (fun q -> (q, None))
                else
                    sortedQuantiles
                    |> List.map (fun q ->
                        let value =
                            if q <= 0.0 then samples.[0]
                            elif q >= 1.0 then samples.[n - 1]
                            else samples.[int (ceil (q * float n)) - 1]

                        (q, Some value))

            { Get =
                fun () ->
                    box
                        { Quantiles = snapshot (now ())
                          Count = count
                          Sum = sum
                          Min = mn
                          Max = mx }
              Update =
                fun v ->
                    let value, ts = unbox<float * float> v

                    if maxSize > 0 then
                        observations.[head % maxSize] <- Some(ts, value)
                        head <- head + 1

                    count <- count + 1
                    sum <- sum + value

                    if value < mn then
                        mn <- value

                    if value > mx then
                        mx <- value }

        // summary's hook input is (value, timestamp); attach the clock per update.
        { mk "Summary" id createHook (fun o -> unbox<SummaryState> o) with
            MapInput = fun o -> box (unbox<float> o, now ()) }

    // --- dump ---

    let private attributesToString (attrs: Map<string, string>) : string =
        let body =
            attrs
            |> Seq.map (fun kv -> sprintf "%s: %s" kv.Key kv.Value)
            |> String.concat ", "

        sprintf "attributes=[%s]" body

    let private renderState (meta: MetricMetadata) : string =
        let s = meta.Hook.Get()

        match meta.Type with
        | "Counter" ->
            let st = unbox<CounterState> s
            sprintf "state=[count: [%s]]" (Cause.renderValue (box st.Count))
        | "Gauge" ->
            let st = unbox<GaugeState> s
            sprintf "state=[value: [%s]]" (Cause.renderValue (box st.Value))
        | _ -> "state=[]"

    let private padEnd (s: string) (n: int) : string =
        if s.Length >= n then
            s
        else
            s + System.String(' ', n - s.Length)

    /// Render all registered metrics, grouped and sorted by id. (Metric.dump —
    /// a unit function here to dodge the value restriction on a generic effect.)
    let dump () : Effect<string, 'E, 'R> =
        Effect.sync (fun () ->
            lock registryLock (fun () ->
                let metrics = registry.Values |> List.ofSeq

                if List.isEmpty metrics then
                    ""
                else
                    let maxName = (metrics |> List.map (fun m -> m.Id.Length) |> List.max) + 2

                    let maxDesc =
                        (metrics |> List.map (fun m -> (defaultArg m.Description "").Length) |> List.max)
                        + 2

                    let maxType = (metrics |> List.map (fun m -> m.Type.Length) |> List.max) + 2

                    let maxAttrs =
                        (metrics
                         |> List.map (fun m -> (attributesToString m.Attributes).Length)
                         |> List.max)
                        + 2

                    metrics
                    |> List.groupBy (fun m -> m.Id)
                    |> List.sortBy fst
                    |> List.collect (fun (_, group) ->
                        group
                        |> List.map (fun m ->
                            let attrs = attributesToString m.Attributes

                            sprintf
                                "name=%sdescription=%stype=%s%s%s%s"
                                (padEnd m.Id maxName)
                                (padEnd (defaultArg m.Description "") maxDesc)
                                (padEnd m.Type maxType)
                                attrs
                                (System.String(' ', max 0 (maxAttrs - attrs.Length)))
                                (renderState m)))
                    |> String.concat "\n"))
