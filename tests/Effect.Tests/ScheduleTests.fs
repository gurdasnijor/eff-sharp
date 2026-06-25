module Effect.Tests.ScheduleTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/Schedule.test.ts.
//
// Upstream drives schedules through `Effect.gen` + `TestClock`, collecting
// `(output, delay)` sequences via `toStepWithSleep`. This port drives the pure
// `Schedule.toStep` machine directly and *simulates the clock*: after a step
// returns delay `d`, the next `now` advances by `d` (the sleep), and — for the
// "slow action" cases — by an additional fixed `action` duration before each
// step (the time the scheduled effect itself takes). This reproduces the exact
// observable `(output, delay)` semantics the upstream suite asserts.

// --- pure clock-simulating runners (analogues of runCollect / runDelays) ---

let private runCollectFrom
    (action: float)
    (schedule: Schedule<'Out, 'In>)
    (inputs: 'In list)
    (start: float)
    : 'Out list =
    let step = Schedule.toStep schedule
    let mutable now = start
    let out = ResizeArray<'Out>()
    let mutable rest = inputs
    let mutable go = true

    while go && not (List.isEmpty rest) do
        let input = List.head rest
        rest <- List.tail rest
        now <- now + action

        match step now input with
        | Continue(o, d) ->
            out.Add o
            now <- now + Duration.toMillis d
        | Done o ->
            out.Add o
            go <- false

    List.ofSeq out

let private runCollect schedule inputs = runCollectFrom 0.0 schedule inputs 0.0

let private runDelays schedule inputs =
    runCollect (Schedule.delays schedule) inputs

let private runDelaysAction action schedule inputs =
    runCollectFrom action (Schedule.delays schedule) inputs 0.0

let private runLast schedule inputs = runCollect schedule inputs |> List.last

let private units n : unit list = List.replicate n ()
let private ms (ds: Duration list) = ds |> List.map Duration.toMillis

let private formatUtc (millis: float) =
    System.DateTimeOffset
        .FromUnixTimeMilliseconds(int64 millis)
        .UtcDateTime.ToString("ddd MMM dd yyyy HH:mm:ss", System.Globalization.CultureInfo.InvariantCulture)

// ----------------------------------------------------------------------------
// collecting
// ----------------------------------------------------------------------------

[<Fact>]
let ``collectInputs - collects all schedule inputs`` () =
    let schedule = Schedule.collectInputs Schedule.forever
    Assert.Equal<int list>([ 1; 2; 3; 4; 5 ], runLast schedule [ 1; 2; 3; 4; 5 ])

[<Fact>]
let ``collectOutputs - collects all schedule outputs`` () =
    let schedule = Schedule.collectOutputs Schedule.forever
    Assert.Equal<int list>([ 0; 1; 2; 3; 4 ], runLast schedule (units 5))

[<Fact>]
let ``collectWhile - collects while the predicate holds`` () =
    let schedule = Schedule.collectWhile (fun m -> m.Output < 3) Schedule.forever
    Assert.Equal<int list>([ 0; 1; 2; 3 ], runLast schedule (units 5))

// ----------------------------------------------------------------------------
// sequencing
// ----------------------------------------------------------------------------

[<Fact>]
let ``tap - provides full metadata`` () =
    let observed = ResizeArray<ScheduleMetadata<int, string>>()
    let schedule = Schedule.spaced (Duration.millis 250.0) |> Schedule.tap observed.Add
    let step = Schedule.toStep schedule
    let first = step 1000.0 "a"
    let second = step 1250.0 "b"

    Assert.Equal(Continue(0, Duration.millis 250.0), first)
    Assert.Equal(Continue(1, Duration.millis 250.0), second)

    Assert.Equal(2, observed.Count)
    let m0 = observed.[0]
    Assert.Equal("a", m0.Input)
    Assert.Equal(0, m0.Output)
    Assert.Equal(Duration.millis 250.0, m0.Duration)
    Assert.Equal(1, m0.Attempt)
    Assert.Equal(1000.0, m0.Start)
    Assert.Equal(1000.0, m0.Now)
    Assert.Equal(0.0, m0.Elapsed)
    Assert.Equal(0.0, m0.ElapsedSincePrevious)
    let m1 = observed.[1]
    Assert.Equal("b", m1.Input)
    Assert.Equal(1, m1.Output)
    Assert.Equal(Duration.millis 250.0, m1.Duration)
    Assert.Equal(2, m1.Attempt)
    Assert.Equal(1000.0, m1.Start)
    Assert.Equal(1250.0, m1.Now)
    Assert.Equal(250.0, m1.Elapsed)
    Assert.Equal(250.0, m1.ElapsedSincePrevious)

[<Fact>]
let ``andThenResult - executes schedules sequentially to completion`` () =
    let left =
        Schedule.``fixed`` (Duration.millis 500.0)
        |> Schedule.``while`` (fun m -> m.Attempt <= 3)

    let right = Schedule.``fixed`` (Duration.seconds 1.0)
    let schedule = Schedule.andThenResult left right
    Assert.Equal<float list>([ 500.0; 500.0; 500.0; 1000.0; 1000.0; 1000.0 ], ms (runDelays schedule (units 6)))

[<Fact>]
let ``andThenResult - emits right completion when right schedule is finite`` () =
    let left =
        Schedule.``fixed`` (Duration.millis 500.0)
        |> Schedule.``while`` (fun m -> m.Attempt <= 2)

    let right = Schedule.duration (Duration.seconds 1.0)
    let schedule = Schedule.andThenResult left right
    Assert.Equal<float list>([ 500.0; 500.0; 1000.0; 0.0 ], ms (runDelays schedule (units 5)))

// ----------------------------------------------------------------------------
// cron (UTC — see Cron.fs scope note)
// ----------------------------------------------------------------------------

[<Fact>]
let ``cron - recurs on interval matching cron expression`` () =
    let start = 1704067235000.0 // 2024-01-01T00:00:35Z
    let schedule = Schedule.cron "*/2 * * * *" // every second minute
    let step = Schedule.toStep schedule
    let mutable now = start
    let out = ResizeArray<string>()

    for _ in 1..4 do
        match step now () with
        | Continue(_, d) ->
            now <- now + Duration.toMillis d
            out.Add(formatUtc now)
        | Done _ -> ()

    Assert.Equal<string list>(
        [ "Mon Jan 01 2024 00:02:00"
          "Mon Jan 01 2024 00:04:00"
          "Mon Jan 01 2024 00:06:00"
          "Mon Jan 01 2024 00:08:00" ],
        List.ofSeq out
    )

[<Fact>]
let ``cron - recurs on interval matching cron expression (second granularity)`` () =
    let start = 1704067200000.0 // 2024-01-01T00:00:00Z
    let schedule = Schedule.cron "*/3 * * * * *" // every third second
    let step = Schedule.toStep schedule
    let mutable now = start
    let out = ResizeArray<string>()

    for _ in 1..10 do
        match step now () with
        | Continue(_, d) ->
            now <- now + Duration.toMillis d
            out.Add(formatUtc now)
        | Done _ -> ()

    Assert.Equal<string list>(
        [ "Mon Jan 01 2024 00:00:03"
          "Mon Jan 01 2024 00:00:06"
          "Mon Jan 01 2024 00:00:09"
          "Mon Jan 01 2024 00:00:12"
          "Mon Jan 01 2024 00:00:15"
          "Mon Jan 01 2024 00:00:18"
          "Mon Jan 01 2024 00:00:21"
          "Mon Jan 01 2024 00:00:24"
          "Mon Jan 01 2024 00:00:27"
          "Mon Jan 01 2024 00:00:30" ],
        List.ofSeq out
    )

// ----------------------------------------------------------------------------
// duration / reduce
// ----------------------------------------------------------------------------

[<Fact>]
let ``duration - recurs once after the provided duration`` () =
    let schedule = Schedule.duration (Duration.seconds 1.0)
    Assert.Equal<float list>([ 1000.0; 0.0 ], ms (runDelays schedule (units 5)))

[<Fact>]
let ``reduce - accumulates state with synchronous combine`` () =
    let schedule =
        Schedule.forever
        |> Schedule.reduce (fun () -> 0) (fun state output -> state + output)

    Assert.Equal<int list>([ 0; 1; 3; 6; 10 ], runCollect schedule (units 5))

// ----------------------------------------------------------------------------
// jittered
// ----------------------------------------------------------------------------

[<Fact>]
let ``jittered - keeps delays within 80%-120% of the original`` () =
    let rng = System.Random(1234)

    let schedule =
        Schedule.jitteredWith (fun () -> rng.NextDouble()) (Schedule.spaced (Duration.seconds 1.0))

    let output = runDelays schedule (units 20) |> ms
    Assert.True(output |> List.forall (fun millis -> millis >= 800.0 && millis <= 1200.0))

[<Fact>]
let ``jittered - does not change completion output`` () =
    let rng = System.Random(99)

    let schedule =
        Schedule.jitteredWith (fun () -> rng.NextDouble()) (Schedule.duration (Duration.seconds 1.0))

    let output = runDelays schedule (units 5) |> ms
    Assert.Equal(2, output.Length)
    Assert.InRange(output.[0], 800.0, 1200.0)
    Assert.Equal(0.0, output.[1])

// ----------------------------------------------------------------------------
// spaced
// ----------------------------------------------------------------------------

[<Fact>]
let ``spaced - constant delays`` () =
    let schedule = Schedule.spaced (Duration.seconds 1.0)
    Assert.Equal<float list>(List.replicate 5 1000.0, ms (runDelays schedule (units 5)))

// ----------------------------------------------------------------------------
// fixed
// ----------------------------------------------------------------------------

[<Fact>]
let ``fixed - constant delays`` () =
    let schedule = Schedule.``fixed`` (Duration.seconds 1.0)
    Assert.Equal<float list>(List.replicate 5 1000.0, ms (runDelays schedule (units 5)))

[<Fact>]
let ``fixed - delays until the nearest window boundary when action is slow`` () =
    let schedule =
        Schedule.``fixed`` (Duration.seconds 1.0)
        |> Schedule.``while`` (fun m -> m.Attempt <= 5)
    // 500ms action each step; the next delay shrinks to realign on the 1s grid.
    let output = runDelaysAction 500.0 schedule (units 6)
    Assert.Equal<float list>([ 1000.0; 500.0; 500.0; 500.0; 500.0; 0.0 ], ms output)

[<Fact>]
let ``fixed - matches effect v3 when action duration exceeds the interval`` () =
    let schedule =
        Schedule.``fixed`` (Duration.seconds 1.0)
        |> Schedule.``while`` (fun m -> m.Attempt <= 5)

    let output = runDelaysAction 1500.0 schedule (units 6)
    Assert.Equal<float list>([ 1000.0; 0.0; 0.0; 0.0; 0.0; 0.0 ], ms output)

// ----------------------------------------------------------------------------
// windowed
// ----------------------------------------------------------------------------

[<Fact>]
let ``windowed - constant delays`` () =
    let schedule = Schedule.windowed (Duration.seconds 1.0)
    Assert.Equal<float list>(List.replicate 5 1000.0, ms (runDelays schedule (units 5)))

[<Fact>]
let ``windowed - delays until the nearest window boundary`` () =
    let schedule =
        Schedule.windowed (Duration.seconds 1.0)
        |> Schedule.``while`` (fun m -> m.Attempt <= 5)

    let output = runDelaysAction 1500.0 schedule (units 6)
    Assert.Equal<float list>([ 1000.0; 500.0; 500.0; 500.0; 500.0; 0.0 ], ms output)

// ----------------------------------------------------------------------------
// union / intersect (combinators highlighted in the brief; not in the upstream
// suite — covered here for the recurring case)
// ----------------------------------------------------------------------------

[<Fact>]
let ``intersect - uses the maximum delay and stops when either stops`` () =
    let schedule =
        Schedule.spaced (Duration.seconds 1.0) |> Schedule.intersect (Schedule.recurs 2)

    Assert.Equal<float list>([ 1000.0; 1000.0; 0.0 ], ms (runDelays schedule (units 5)))

[<Fact>]
let ``union - uses the minimum delay while either continues`` () =
    let schedule =
        Schedule.spaced (Duration.seconds 1.0)
        |> Schedule.union (Schedule.spaced (Duration.seconds 2.0))

    Assert.Equal<float list>([ 1000.0; 1000.0; 1000.0 ], ms (runDelays schedule (units 3)))

// ----------------------------------------------------------------------------
// recurs / exponential (core constructors)
// ----------------------------------------------------------------------------

[<Fact>]
let ``recurs - recurs the specified number of times then stops`` () =
    let schedule = Schedule.recurs 3
    Assert.Equal<int list>([ 0; 1; 2; 3 ], runCollect schedule (units 10))

[<Fact>]
let ``exponential - doubles the delay each step`` () =
    let schedule = Schedule.exponential (Duration.millis 100.0)
    Assert.Equal<float list>([ 100.0; 200.0; 400.0; 800.0 ], ms (runDelays schedule (units 4)))
