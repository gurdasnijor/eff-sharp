/// @title Schedules — composable retry / repeat / pacing policies
///
/// A `Schedule<'Out,'In>` is a *decision machine*: stepped with a timestamp and
/// an input, it either **continues** (emitting an output and the delay until the
/// next step) or is **done**. Backoff, capped retries, fixed cadences, jitter,
/// and "recur while both/either" all compose from small pieces.
///
/// Mirrors upstream ai-docs `06_schedule`. In this slice the `Effect.repeat` /
/// `Effect.retry` wiring is deferred, so we drive schedules the way the upstream
/// `toStep` test does: with explicit timestamps. That actually makes the
/// `(output, delay)` semantics *visible*, which is perfect for teaching.
module EffSharp.AiDocs.Schedule06

open Effect
open EffSharp.AiDocs.Demo

/// Drive a schedule for up to `maxSteps`, starting at t=0 and advancing the
/// clock by each returned delay. Collects the `(output, delay)` of every step.
///
/// Note the NATIVE `match` over the `ScheduleDecision` DU (`Continue` / `Done`)
/// — eff-sharp models the decision as a plain union, so control flow is ordinary
/// F# pattern matching, not a `Schedule.match` combinator.
let private drive (maxSteps: int) (input: 'In) (sched: Schedule<'Out, 'In>) : ('Out * Duration) list =
    let step = Schedule.toStep sched

    let rec loop now n acc =
        if n >= maxSteps then
            List.rev acc
        else
            match step now input with
            | Continue(out, delay) -> loop (now + Duration.toMillis delay) (n + 1) ((out, delay) :: acc)
            | Done out -> List.rev ((out, Duration.zero) :: acc)

    loop 0.0 0 []

let private show label rows =
    printfn
        "  %-22s %s"
        label
        (rows
         |> List.map (fun (o, d) -> sprintf "%A@%s" o (Duration.format d))
         |> String.concat "  ")

let run () : unit =
    section "06 Schedule — composable policies"

    // `recurs n` repeats n times then stops, emitting the recurrence count.
    show "recurs 3:" (drive 6 () (Schedule.recurs 3))

    // `spaced` keeps a constant gap between runs (here 1s), emitting the count.
    show "spaced 1s:" (drive 4 () (Schedule.spaced (Duration.seconds 1.0)))

    // `exponential` doubles the delay each step — the classic backoff. Output is
    // the delay itself.
    show "exponential 100ms:" (drive 4 () (Schedule.exponential (Duration.millis 100.0)))

    // `jitteredWith` scales each delay to 80–120% of its value, spreading retries
    // to avoid thundering herds. We inject a deterministic 0.5 source (→ exactly
    // 100%) so the demo is reproducible; production code uses `Schedule.jittered`.
    show
        "spaced+jitter(0.5):"
        (drive 3 () (Schedule.spaced (Duration.seconds 1.0) |> Schedule.jitteredWith (fun () -> 0.5)))

    // `intersect` recurs only while BOTH policies want to — using the MAXIMUM of
    // the two delays. This is "exponential backoff, but capped at 2 attempts":
    // the `recurs 2` side stops the whole thing.
    show
        "expo ∩ recurs 2:"
        (drive
            6
            ()
            (Schedule.exponential (Duration.millis 100.0)
             |> Schedule.intersect (Schedule.recurs 2)))

    // `collectOutputs` folds every output into a growing list — handy for
    // recording what a retry loop actually did.
    show "collectOutputs(recurs 2):" (drive 4 () (Schedule.collectOutputs (Schedule.recurs 2)))
