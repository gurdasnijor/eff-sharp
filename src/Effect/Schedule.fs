namespace Effect

/// `Schedule` — Effect's retry/repeat/pacing decision machine.
///
/// Port of repos/effect-smol/packages/effect/src/Schedule.ts. A
/// `Schedule<'Out,'In>` is a stateful policy: stepped with a timestamp (`now`,
/// epoch-milliseconds as upstream) and an input, each step either **continues**
/// (producing an output and the delay until the next step) or is **done**
/// (producing a final output). Constructors and combinators build these
/// machines; `recurs`/`spaced`/`exponential`/`fixed`/`windowed`/`cron`/
/// `union`/`intersect`/`jittered` and the metadata-driven `while`/`reduce`/
/// `collect*` mirror upstream's observable `(output, delay)` semantics exactly
/// (see Schedule.test.ts).
///
/// Model (faithful to upstream `fromStep`/`toStep`):
///   * Upstream's step is `Effect<(now, input) => Pull<[Out, Duration], …>>`,
///     where a `Pull` either succeeds with `[output, delay]` or signals *done*
///     with a final output. Here that lowers to a pure state machine:
///     `Start : unit -> (now -> input -> ScheduleDecision)` allocates fresh
///     mutable step state (the analogue of `Effect.sync(() => …)`), and a
///     `ScheduleDecision` is `Continue (output, delay)` | `Done output`.
///   * `metadataFn` reproduces upstream's attempt/start/elapsed bookkeeping.
///
/// Omissions (per CONVENTIONS — "too large → core surface + noted skips"):
///   * The `Effect`/`Pull`/`Clock`/`Random` wiring — `Effect.repeat`/`retry`/
///     `schedule`, `toStepWithSleep`, `CurrentMetadata`, and reading the live
///     clock — is **deferred**. `toStep` exposes the pure step so callers (and
///     the tests) can drive a schedule with explicit timestamps, exactly as
///     upstream's `toStep` test does; sleeping/forking is simulated by advancing
///     `now`. `jittered` takes its randomness via `jitteredWith` (an injectable
///     `unit -> float`) instead of the `Random` service.
///   * Effectful variants of `map`/`while`/`reduce`/`tap` (predicates returning
///     `Effect<bool>`) are lowered to pure functions — sufficient for the ported
///     behaviour; the effectful overloads are deferred.
///   * `unfold`, `linear`, `fibonacci`, `addDelay`/`modifyDelay`, `take`,
///     `andThen`, `tapInput`/`tapOutput`, `bothLeft`/`bothRight` and the HKT/
///     `TypeId`/`Pipeable` plumbing are not ported in this slice.
/// Per-step metadata before the output is known.
type InputMetadata<'In> =
    { Input: 'In
      Attempt: int
      Start: float
      Now: float
      Elapsed: float
      ElapsedSincePrevious: float }

/// Full per-step metadata, including the produced output and delay.
type ScheduleMetadata<'Out, 'In> =
    { Input: 'In
      Output: 'Out
      Duration: Duration
      Attempt: int
      Start: float
      Now: float
      Elapsed: float
      ElapsedSincePrevious: float }

/// The result of stepping a schedule: continue with an output and the delay
/// until the next step, or stop with a final output.
type ScheduleDecision<'Out> =
    | Continue of output: 'Out * delay: Duration
    | Done of output: 'Out

/// A schedule: allocates fresh step state, yielding a `now -> input -> decision`
/// stepper.
type Schedule<'Out, 'In> =
    internal
        { Start: unit -> (float -> 'In -> ScheduleDecision<'Out>) }

[<RequireQualifiedAccess>]
module Schedule =

    // --- core ---

    /// Build a schedule from a state-allocating step function. (Schedule.fromStep)
    let internal fromStep (start: unit -> (float -> 'In -> ScheduleDecision<'Out>)) : Schedule<'Out, 'In> =
        { Start = start }

    /// Allocate a fresh step function (with its own mutable state). The analogue
    /// of upstream `toStep` — callers supply `now` and handle the delay. (Schedule.toStep)
    let toStep (self: Schedule<'Out, 'In>) : float -> 'In -> ScheduleDecision<'Out> = self.Start()

    /// Upstream `metadataFn`: tracks attempt count and elapsed timing per step.
    let private metadataFn () : float -> 'In -> InputMetadata<'In> =
        let mutable n = 0
        let mutable previous = ValueNone
        let mutable start = ValueNone

        fun now input ->
            let s =
                match start with
                | ValueSome v -> v
                | ValueNone ->
                    start <- ValueSome now
                    now

            let elapsedSincePrevious =
                match previous with
                | ValueSome p -> now - p
                | ValueNone -> 0.0

            previous <- ValueSome now
            n <- n + 1

            { Input = input
              Attempt = n
              Start = s
              Now = now
              Elapsed = now - s
              ElapsedSincePrevious = elapsedSincePrevious }

    /// Build a schedule from a metadata-driven decision. (Schedule.fromStepWithMetadata)
    let internal fromMetaStep (f: InputMetadata<'In> -> ScheduleDecision<'Out>) : Schedule<'Out, 'In> =
        fromStep (fun () ->
            let meta = metadataFn ()
            fun now input -> f (meta now input))

    // --- base constructors ---

    /// Recurs forever with a constant `interval` between recurrences, outputting
    /// the recurrence count (`0, 1, 2, …`). (Schedule.spaced)
    let spaced (interval: Duration) : Schedule<int, 'In> =
        fromMetaStep (fun m -> Continue(m.Attempt - 1, interval))

    /// Recurs forever with no delay, outputting the recurrence count. (Schedule.forever)
    let forever<'In> : Schedule<int, 'In> = spaced Duration.zero

    // --- metadata filtering ---

    /// Continues while `predicate` holds over the step metadata, otherwise stops
    /// with the current output. (Schedule.while)
    let ``while`` (predicate: ScheduleMetadata<'Out, 'In> -> bool) (self: Schedule<'Out, 'In>) : Schedule<'Out, 'In> =
        fromStep (fun () ->
            let step = toStep self
            let meta = metadataFn ()

            fun now input ->
                match step now input with
                | Done o -> Done o
                | Continue(o, d) ->
                    let im = meta now input

                    let full =
                        { Input = im.Input
                          Output = o
                          Duration = d
                          Attempt = im.Attempt
                          Start = im.Start
                          Now = im.Now
                          Elapsed = im.Elapsed
                          ElapsedSincePrevious = im.ElapsedSincePrevious }

                    if predicate full then Continue(o, d) else Done o)

    /// Recurs the specified number of times, outputting the recurrence count. (Schedule.recurs)
    let recurs (times: int) : Schedule<int, 'In> =
        ``while`` (fun m -> m.Attempt <= times) forever

    /// Recurs once after `d`, outputting `d`, then completes. (Schedule.duration)
    let duration (d: Duration) : Schedule<Duration, 'In> =
        fromMetaStep (fun m -> if m.Attempt = 1 then Continue(d, d) else Done Duration.zero)

    /// Recurs with exponentially increasing delays (`base * factor^(n-1)`),
    /// outputting the delay. (Schedule.exponential)
    let exponentialFactor (factor: float) (baseDuration: Duration) : Schedule<Duration, 'In> =
        let baseMillis = Duration.toMillis baseDuration

        fromMetaStep (fun m ->
            let d = Duration.millis (baseMillis * (factor ** float (m.Attempt - 1)))
            Continue(d, d))

    /// `exponential` with the default factor of `2`. (Schedule.exponential)
    let exponential (baseDuration: Duration) : Schedule<Duration, 'In> = exponentialFactor 2.0 baseDuration

    /// Divides the timeline into `interval`-long windows and sleeps until the
    /// next window boundary on each recurrence. (Schedule.windowed)
    let windowed (interval: Duration) : Schedule<int, 'In> =
        let window = Duration.toMillis interval

        fromMetaStep (fun m ->
            let delay =
                if window = 0.0 then
                    Duration.zero
                else
                    Duration.millis (window - (m.Elapsed % window))

            Continue(m.Attempt - 1, delay))

    /// Recurs on a fixed cadence: each delay realigns to the regular `interval`
    /// boundary, shortening (or dropping to zero when running behind) so steps
    /// stay on schedule regardless of action duration. (Schedule.fixed)
    let ``fixed`` (interval: Duration) : Schedule<int, 'In> =
        let window = Duration.toMillis interval

        fromStep (fun () ->
            let meta = metadataFn ()
            let mutable start = 0.0
            let mutable lastRun = 0.0

            fun now input ->
                let m = meta now input

                if window = 0.0 then
                    Continue(m.Attempt - 1, Duration.zero)
                elif m.Attempt = 1 then
                    start <- m.Now
                    lastRun <- m.Now + window
                    Continue(0, Duration.millis window)
                else
                    let runningBehind = m.Now > (lastRun + window)
                    let boundary = window - ((m.Now - start) % window)

                    let delay =
                        if runningBehind then 0.0
                        elif boundary = 0.0 then window
                        else boundary

                    lastRun <- if runningBehind then m.Now else m.Now + delay
                    Continue(m.Attempt - 1, Duration.millis delay))

    /// Recurs on the instants matching a cron expression (evaluated in UTC, per
    /// `Cron`), outputting the duration until the next occurrence. (Schedule.cron)
    let cron (expression: string) : Schedule<Duration, 'In> =
        let parsed = Cron.parseUnsafe expression (Some "UTC")

        fromStep (fun () ->
            fun now _ ->
                let nowDt = DateTime.makeUnsafe (InputEpochMillis(int64 now))
                let next = (Cron.next parsed nowDt).EpochMillis
                let d = Duration.millis (float next - now)
                Continue(d, d))

    // --- output / delay transformers ---

    /// Maps the output of every decision. (Schedule.map)
    let map (f: 'Out -> 'Out2) (self: Schedule<'Out, 'In>) : Schedule<'Out2, 'In> =
        fromStep (fun () ->
            let step = toStep self

            fun now input ->
                match step now input with
                | Continue(o, d) -> Continue(f o, d)
                | Done o -> Done(f o))

    /// Outputs the schedule's inputs instead of its outputs. (Schedule.passthrough)
    let passthrough (self: Schedule<'Out, 'In>) : Schedule<'In, 'In> =
        fromStep (fun () ->
            let step = toStep self

            fun now input ->
                match step now input with
                | Continue(_, d) -> Continue(input, d)
                | Done _ -> Done input)

    /// Outputs the delay between recurrences; emits `Duration.zero` on completion. (Schedule.delays)
    let delays (self: Schedule<'Out, 'In>) : Schedule<Duration, 'In> =
        fromStep (fun () ->
            let step = toStep self

            fun now input ->
                match step now input with
                | Continue(_, d) -> Continue(d, d)
                | Done _ -> Done Duration.zero)

    /// Runs `f` with the full metadata on each continuing decision, leaving the
    /// schedule's inputs/outputs unchanged. (Schedule.tap)
    let tap (f: ScheduleMetadata<'Out, 'In> -> unit) (self: Schedule<'Out, 'In>) : Schedule<'Out, 'In> =
        fromStep (fun () ->
            let step = toStep self
            let meta = metadataFn ()

            fun now input ->
                match step now input with
                | Done o -> Done o
                | Continue(o, d) ->
                    let im = meta now input

                    f
                        { Input = im.Input
                          Output = o
                          Duration = d
                          Attempt = im.Attempt
                          Start = im.Start
                          Now = im.Now
                          Elapsed = im.Elapsed
                          ElapsedSincePrevious = im.ElapsedSincePrevious }

                    Continue(o, d))

    /// Folds outputs into accumulating state with a synchronous combine. (Schedule.reduce)
    let reduce (initial: unit -> 'S) (combine: 'S -> 'Out -> 'S) (self: Schedule<'Out, 'In>) : Schedule<'S, 'In> =
        fromStep (fun () ->
            let step = toStep self
            let mutable state = initial ()

            fun now input ->
                match step now input with
                | Continue(o, d) ->
                    state <- combine state o
                    Continue(state, d)
                | Done o -> Done(combine state o))

    // --- collecting ---

    /// Collects outputs into a list while `predicate` holds. (Schedule.collectWhile)
    let collectWhile
        (predicate: ScheduleMetadata<'Out, 'In> -> bool)
        (self: Schedule<'Out, 'In>)
        : Schedule<'Out list, 'In> =
        reduce (fun () -> []) (fun acc o -> acc @ [ o ]) (``while`` predicate self)

    /// Collects all of the schedule's inputs into a list. (Schedule.collectInputs)
    let collectInputs (self: Schedule<'Out, 'In>) : Schedule<'In list, 'In> =
        collectWhile (fun _ -> true) (passthrough self)

    /// Collects all of the schedule's outputs into a list. (Schedule.collectOutputs)
    let collectOutputs (self: Schedule<'Out, 'In>) : Schedule<'Out list, 'In> = collectWhile (fun _ -> true) self

    // --- jitter ---

    /// `jittered` with an injectable uniform source in `[0, 1)`, scaling each
    /// delay to `80%–120%` of its value. (Schedule.jittered, randomness injected)
    let jitteredWith (random: unit -> float) (self: Schedule<'Out, 'In>) : Schedule<'Out, 'In> =
        fromStep (fun () ->
            let step = toStep self

            fun now input ->
                match step now input with
                | Continue(o, d) ->
                    let r = random ()
                    let millis = Duration.toMillis d
                    Continue(o, Duration.millis (millis * 0.8 * (1.0 - r) + millis * 1.2 * r))
                | Done o -> Done o)

    /// Randomly jitters each delay to `80%–120%` using a fresh `System.Random`.
    /// (Schedule.jittered)
    let jittered (self: Schedule<'Out, 'In>) : Schedule<'Out, 'In> =
        fromStep (fun () ->
            let rng = System.Random()
            let inner = toStep (jitteredWith (fun () -> rng.NextDouble()) self)
            inner)

    // --- combining ---

    /// Recurs only while **both** schedules want to recur, using the **maximum**
    /// of the two delays and outputting the pair. (Schedule.intersect / both)
    let intersect (other: Schedule<'O2, 'In>) (self: Schedule<'O1, 'In>) : Schedule<'O1 * 'O2, 'In> =
        fromStep (fun () ->
            let l = toStep self
            let r = toStep other

            fun now input ->
                match l now input, r now input with
                | Continue(lo, ld), Continue(ro, rd) ->
                    let delay =
                        if Duration.toMillis ld >= Duration.toMillis rd then
                            ld
                        else
                            rd

                    Continue((lo, ro), delay)
                | Continue(lo, _), Done ro
                | Done lo, Continue(ro, _) -> Done(lo, ro)
                | Done lo, Done ro -> Done(lo, ro))

    /// Recurs while **either** schedule wants to recur, using the **minimum** of
    /// the two delays and outputting the pair; stops only when both stop.
    /// (Schedule.union / either)
    let union (other: Schedule<'O2, 'In>) (self: Schedule<'O1, 'In>) : Schedule<'O1 * 'O2, 'In> =
        fromStep (fun () ->
            let l = toStep self
            let r = toStep other

            fun now input ->
                match l now input, r now input with
                | Continue(lo, ld), Continue(ro, rd) ->
                    let delay =
                        if Duration.toMillis ld <= Duration.toMillis rd then
                            ld
                        else
                            rd

                    Continue((lo, ro), delay)
                | Continue(lo, ld), Done ro -> Continue((lo, ro), ld)
                | Done lo, Continue(ro, rd) -> Continue((lo, ro), rd)
                | Done lo, Done ro -> Done(lo, ro))

    // --- sequencing ---

    /// Runs `left` to completion, then `right` to completion; outputs `Ok` for
    /// `left`'s outputs and `Error` for `right`'s. `left`'s completion is not
    /// emitted — the schedule switches to `right` instead. (Schedule.andThenResult)
    let andThenResult (left: Schedule<'L, 'In>) (right: Schedule<'R, 'In>) : Schedule<Result<'L, 'R>, 'In> =
        fromStep (fun () ->
            let lstep = toStep left
            let rstep = toStep right
            let mutable onRight = false

            let stepRight now input =
                match rstep now input with
                | Continue(o, d) -> Continue(Error o, d)
                | Done o -> Done(Error o)

            fun now input ->
                if onRight then
                    stepRight now input
                else
                    match lstep now input with
                    | Continue(o, d) -> Continue(Ok o, d)
                    | Done _ ->
                        onRight <- true
                        stepRight now input)
