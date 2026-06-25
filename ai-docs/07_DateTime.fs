/// @title DateTime & Duration — UTC instants and calendar math
///
/// `DateTime` is a UTC instant (epoch millis under the hood); `Duration` is a
/// length of time. Together they cover formatting, comparison, calendar
/// arithmetic (`add`/`subtract` honoring real month lengths & leap years), and
/// "distance between two instants".
///
/// Mirrors upstream ai-docs `07_datetime`. Bonus F# flex at the bottom: native
/// records/DUs for domain modelling, and **units of measure** — a compile-time
/// guard against unit mix-ups that Effect's TypeScript simply cannot express.
module EffSharp.AiDocs.DateTime07

open Effect
open EffSharp.AiDocs.Demo

// ───────────────────────────────────────────────────────────────────────────
// 1. Construction & formatting
// ───────────────────────────────────────────────────────────────────────────

/// `makeUnsafe` takes a `DateTimeInput` DU — epoch millis, an ISO string, or a
/// bag of calendar `Parts`. (`make` returns an option for untrusted input.)
let epoch = DateTime.makeUnsafe (InputEpochMillis 0L)

let independenceDay =
    DateTime.makeUnsafe (InputParts { DateTimeParts.Empty with Year = Some 1776; Month = Some 7; Day = Some 4 })

// ───────────────────────────────────────────────────────────────────────────
// 2. Calendar arithmetic & distance
// ───────────────────────────────────────────────────────────────────────────

/// `add` takes signed amounts (a `DateTimeMath` record) and respects the real
/// calendar — adding 1 month to Jan 31 lands correctly, leap years included.
let nextMilestone =
    independenceDay |> DateTime.add { DateTimeMath.Zero with Years = 250; Months = 0; Days = 0 }

/// `distance` is the `Duration` between two instants — pure, total, signed.
let sinceEpoch = DateTime.distance epoch independenceDay

// ───────────────────────────────────────────────────────────────────────────
// 3. Comparison & rounding — `Duration` and `DateTime` are fully ordered
// ───────────────────────────────────────────────────────────────────────────

let timeout = Duration.seconds 30.0
let elapsed = Duration.millis 27_500.0
let withinBudget = Duration.isLessThanOrEqualTo elapsed timeout
/// `clamp` keeps a duration inside an inclusive range.
let clamped = Duration.clamp (Duration.seconds 1.0) (Duration.seconds 10.0) (Duration.minutes 5.0)

// ───────────────────────────────────────────────────────────────────────────
// 4. F# FLEX — model the domain with records/DUs + a NATIVE match on parts
// ───────────────────────────────────────────────────────────────────────────

type Event = { Name: string; At: DateTime }

/// Classify an instant by destructuring its resolved UTC parts with an ordinary
/// `match` (WeekDay: 0 = Sunday … 6 = Saturday). No `DateTime.match` combinator —
/// just F# pattern matching with guards over the `DateTimePartsUtc` record.
let dayKind (dt: DateTime) : string =
    match DateTime.toPartsUtc dt with
    | { WeekDay = 0 } | { WeekDay = 6 } -> "weekend"
    | { Hour = h } when h < 9 || h >= 17 -> "off-hours weekday"
    | _ -> "business hours"

// ───────────────────────────────────────────────────────────────────────────
// 5. F# FLEX — units of measure: typed time arithmetic, checked by the compiler
// ───────────────────────────────────────────────────────────────────────────

[<Measure>] type s   // seconds
[<Measure>] type ms  // milliseconds

/// A conversion that *carries its units*. `requestTimeout` below is `float<s>`;
/// passing it where `float<ms>` is wanted is a COMPILE ERROR — the kind of bug
/// (seconds-vs-millis) that silently ships in untyped code.
let secondsToMillis (x: float<s>) : float<ms> = x * 1000.0<ms/s>

let requestTimeout = 30.0<s>
let requestTimeoutMs : float<ms> = secondsToMillis requestTimeout

let run () : unit =
    section "07 DateTime & Duration"

    printfn "epoch            : %s" (DateTime.formatIso epoch)
    printfn "independence day : %s" (DateTime.formatIso independenceDay)
    printfn "  + 250 years    : %s" (DateTime.formatIso nextMilestone)
    printfn "epoch→1776 dist  : %s" (Duration.format sinceEpoch)

    printfn "elapsed ≤ timeout: %b   (clamp 5min→[1s,10s] = %s)" withinBudget (Duration.format clamped)

    let events =
        [ { Name = "Sat brunch"; At = DateTime.makeUnsafe (InputString "2026-06-20T11:00:00Z") }
          { Name = "Standup"; At = DateTime.makeUnsafe (InputString "2026-06-22T09:30:00Z") }
          { Name = "Late deploy"; At = DateTime.makeUnsafe (InputString "2026-06-22T22:00:00Z") } ]
    for e in events do
        printfn "  %-12s %s -> %s" e.Name (DateTime.formatIso e.At) (dayKind e.At)

    printfn "units of measure : %.1f s = %.1f ms (compiler-checked)"
        (float requestTimeout) (float requestTimeoutMs)
