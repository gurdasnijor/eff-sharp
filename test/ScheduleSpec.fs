module ScheduleSpec

open System
open System.Globalization
open Effect
open Effect.Vitest

let private same actual expected = toBe (actual = expected) true

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

let private runDelays schedule inputs = runCollect (Schedule.delays schedule) inputs

let private runDelaysAction action schedule inputs =
    runCollectFrom action (Schedule.delays schedule) inputs 0.0

let private runLast schedule inputs = runCollect schedule inputs |> List.last
let private units n : unit list = List.replicate n ()
let private ms (ds: Duration list) = ds |> List.map Duration.toMillis

let private formatUtc (millis: float) =
    DateTimeOffset
        .FromUnixTimeMilliseconds(int64 millis)
        .UtcDateTime.ToString("ddd MMM dd yyyy HH:mm:ss", CultureInfo.InvariantCulture)

describe "Schedule" (fun () ->
    test "collectInputs collects all schedule inputs" (fun () ->
        let schedule = Schedule.collectInputs Schedule.forever
        same (runLast schedule [ 1; 2; 3; 4; 5 ]) [ 1; 2; 3; 4; 5 ])

    test "collectOutputs collects all schedule outputs" (fun () ->
        let schedule = Schedule.collectOutputs Schedule.forever
        same (runLast schedule (units 5)) [ 0; 1; 2; 3; 4 ])

    test "collectWhile collects while the predicate holds" (fun () ->
        let schedule = Schedule.collectWhile (fun m -> m.Output < 3) Schedule.forever
        same (runLast schedule (units 5)) [ 0; 1; 2; 3 ])

    test "tap provides full metadata" (fun () ->
        let observed = ResizeArray<ScheduleMetadata<int, string>>()
        let schedule = Schedule.spaced (Duration.millis 250.0) |> Schedule.tap observed.Add
        let step = Schedule.toStep schedule
        let first = step 1000.0 "a"
        let second = step 1250.0 "b"

        same first (Continue(0, Duration.millis 250.0))
        same second (Continue(1, Duration.millis 250.0))

        toBe observed.Count 2
        let m0 = observed.[0]
        toBe m0.Input "a"
        toBe m0.Output 0
        same m0.Duration (Duration.millis 250.0)
        toBe m0.Attempt 1
        toBe m0.Start 1000.0
        toBe m0.Now 1000.0
        toBe m0.Elapsed 0.0
        toBe m0.ElapsedSincePrevious 0.0

        let m1 = observed.[1]
        toBe m1.Input "b"
        toBe m1.Output 1
        same m1.Duration (Duration.millis 250.0)
        toBe m1.Attempt 2
        toBe m1.Start 1000.0
        toBe m1.Now 1250.0
        toBe m1.Elapsed 250.0
        toBe m1.ElapsedSincePrevious 250.0)

    test "andThenResult executes schedules sequentially to completion" (fun () ->
        let left =
            Schedule.``fixed`` (Duration.millis 500.0)
            |> Schedule.``while`` (fun m -> m.Attempt <= 3)

        let right = Schedule.``fixed`` (Duration.seconds 1.0)
        let schedule = Schedule.andThenResult left right
        same (ms (runDelays schedule (units 6))) [ 500.0; 500.0; 500.0; 1000.0; 1000.0; 1000.0 ])

    test "andThenResult emits right completion when right schedule is finite" (fun () ->
        let left =
            Schedule.``fixed`` (Duration.millis 500.0)
            |> Schedule.``while`` (fun m -> m.Attempt <= 2)

        let right = Schedule.duration (Duration.seconds 1.0)
        let schedule = Schedule.andThenResult left right
        same (ms (runDelays schedule (units 5))) [ 500.0; 500.0; 1000.0; 0.0 ])

    test "cron recurs on interval matching cron expression" (fun () ->
        let start = 1704067235000.0
        let schedule = Schedule.cron "*/2 * * * *"
        let step = Schedule.toStep schedule
        let mutable now = start
        let out = ResizeArray<string>()

        for _ in 1..4 do
            match step now () with
            | Continue(_, d) ->
                now <- now + Duration.toMillis d
                out.Add(formatUtc now)
            | Done _ -> ()

        same
            (List.ofSeq out)
            [ "Mon Jan 01 2024 00:02:00"
              "Mon Jan 01 2024 00:04:00"
              "Mon Jan 01 2024 00:06:00"
              "Mon Jan 01 2024 00:08:00" ])

    test "cron recurs on interval matching cron expression with second granularity" (fun () ->
        let start = 1704067200000.0
        let schedule = Schedule.cron "*/3 * * * * *"
        let step = Schedule.toStep schedule
        let mutable now = start
        let out = ResizeArray<string>()

        for _ in 1..10 do
            match step now () with
            | Continue(_, d) ->
                now <- now + Duration.toMillis d
                out.Add(formatUtc now)
            | Done _ -> ()

        same
            (List.ofSeq out)
            [ "Mon Jan 01 2024 00:00:03"
              "Mon Jan 01 2024 00:00:06"
              "Mon Jan 01 2024 00:00:09"
              "Mon Jan 01 2024 00:00:12"
              "Mon Jan 01 2024 00:00:15"
              "Mon Jan 01 2024 00:00:18"
              "Mon Jan 01 2024 00:00:21"
              "Mon Jan 01 2024 00:00:24"
              "Mon Jan 01 2024 00:00:27"
              "Mon Jan 01 2024 00:00:30" ])

    test "duration recurs once after the provided duration" (fun () ->
        let schedule = Schedule.duration (Duration.seconds 1.0)
        same (ms (runDelays schedule (units 5))) [ 1000.0; 0.0 ])

    test "reduce accumulates state with synchronous combine" (fun () ->
        let schedule =
            Schedule.forever
            |> Schedule.reduce (fun () -> 0) (fun state output -> state + output)

        same (runCollect schedule (units 5)) [ 0; 1; 3; 6; 10 ])

    test "jittered keeps delays within 80%-120% of the original" (fun () ->
        let rng = Random(1234)

        let schedule =
            Schedule.jitteredWith (fun () -> rng.NextDouble()) (Schedule.spaced (Duration.seconds 1.0))

        let output = runDelays schedule (units 20) |> ms
        toBe (output |> List.forall (fun millis -> millis >= 800.0 && millis <= 1200.0)) true)

    test "jittered does not change completion output" (fun () ->
        let rng = Random(99)

        let schedule =
            Schedule.jitteredWith (fun () -> rng.NextDouble()) (Schedule.duration (Duration.seconds 1.0))

        let output = runDelays schedule (units 5) |> ms
        toBe output.Length 2
        toBe (output.[0] >= 800.0 && output.[0] <= 1200.0) true
        toBe output.[1] 0.0)

    test "spaced uses constant delays" (fun () ->
        let schedule = Schedule.spaced (Duration.seconds 1.0)
        same (ms (runDelays schedule (units 5))) (List.replicate 5 1000.0))

    test "fixed uses constant delays" (fun () ->
        let schedule = Schedule.``fixed`` (Duration.seconds 1.0)
        same (ms (runDelays schedule (units 5))) (List.replicate 5 1000.0))

    test "fixed delays until the nearest window boundary when action is slow" (fun () ->
        let schedule =
            Schedule.``fixed`` (Duration.seconds 1.0)
            |> Schedule.``while`` (fun m -> m.Attempt <= 5)

        let output = runDelaysAction 500.0 schedule (units 6)
        same (ms output) [ 1000.0; 500.0; 500.0; 500.0; 500.0; 0.0 ])

    test "fixed matches effect v3 when action duration exceeds the interval" (fun () ->
        let schedule =
            Schedule.``fixed`` (Duration.seconds 1.0)
            |> Schedule.``while`` (fun m -> m.Attempt <= 5)

        let output = runDelaysAction 1500.0 schedule (units 6)
        same (ms output) [ 1000.0; 0.0; 0.0; 0.0; 0.0; 0.0 ])

    test "windowed uses constant delays" (fun () ->
        let schedule = Schedule.windowed (Duration.seconds 1.0)
        same (ms (runDelays schedule (units 5))) (List.replicate 5 1000.0))

    test "windowed delays until the nearest window boundary" (fun () ->
        let schedule =
            Schedule.windowed (Duration.seconds 1.0)
            |> Schedule.``while`` (fun m -> m.Attempt <= 5)

        let output = runDelaysAction 1500.0 schedule (units 6)
        same (ms output) [ 1000.0; 500.0; 500.0; 500.0; 500.0; 0.0 ])

    test "intersect uses the maximum delay and stops when either stops" (fun () ->
        let schedule =
            Schedule.spaced (Duration.seconds 1.0) |> Schedule.intersect (Schedule.recurs 2)

        same (ms (runDelays schedule (units 5))) [ 1000.0; 1000.0; 0.0 ])

    test "union uses the minimum delay while either continues" (fun () ->
        let schedule =
            Schedule.spaced (Duration.seconds 1.0)
            |> Schedule.union (Schedule.spaced (Duration.seconds 2.0))

        same (ms (runDelays schedule (units 3))) [ 1000.0; 1000.0; 1000.0 ])

    test "recurs the specified number of times then stops" (fun () ->
        let schedule = Schedule.recurs 3
        same (runCollect schedule (units 10)) [ 0; 1; 2; 3 ])

    test "exponential doubles the delay each step" (fun () ->
        let schedule = Schedule.exponential (Duration.millis 100.0)
        same (ms (runDelays schedule (units 4))) [ 100.0; 200.0; 400.0; 800.0 ]))
