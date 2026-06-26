namespace Effect

open System

/// `Clock` — time access as a service.
///
/// Port of repos/effect-smol/packages/effect/src/Clock.ts. The active `Clock`
/// provides the current time and a `sleep` operation; because time is reached
/// through a `Context` service, tests can swap in a controlled clock. Depends on
/// `Duration` (merged) and `Context`.
///
/// The service stores *unsafe* primitives (a synchronous time read and a sleep
/// that yields a `Task`); the module wraps them into effects so each accessor can
/// be used at any error type `'E`. `sleep` bridges to `Task.Delay` in the async
/// core.
///
/// Omissions vs upstream (per CONVENTIONS): nanosecond precision
/// (`currentTimeNanos`) is dropped — .NET wall-clock is millisecond-grained.
/// The time service. `CurrentTimeMillisUnsafe` reads wall-clock millis; `SleepUnsafe`
/// returns a `Task` that completes after the given duration.
type Clock =
    { CurrentTimeMillisUnsafe: unit -> int64
      SleepUnsafe: Duration -> Async<unit> }

[<RequireQualifiedAccess>]
module Clock =

    /// The live, wall-clock implementation. (effect.ClockRef default)
    let make () : Clock =
        { CurrentTimeMillisUnsafe = fun () -> DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()
          SleepUnsafe =
            fun d ->
                let ms = Duration.toMillis d
                // ms <= 0 (or NaN, since `NaN > 0.0` is false) → no wait; an "infinite"
                // sleep is approximated by the max delay. `Async.Sleep` works on both
                // .NET and the Fable/JS target (where it maps to setTimeout), replacing
                // the .NET-only `Task.Delay`.
                async {
                    if ms > 0.0 then
                        let capped = if ms > float Int32.MaxValue then Int32.MaxValue else int ms
                        do! Async.Sleep capped
                } }

    /// The defaulted `Context.Reference` under which the active clock service is stored.
    let reference: Reference<Clock> = Reference.make "effect/Clock" (make ())

    /// Compatibility tag for Layer/Context APIs. Values provided here override `reference`.
    let tag: Tag<Clock> = Reference.toTag reference

    /// A `Context` carrying the live clock. (Clock layer)
    let live: Context = Context.make tag (make ())

    /// Use the active `Clock` service to build an effect. (Clock.clockWith)
    let clockWith (f: Clock -> Effect<'A, 'E, Context>) : Effect<'A, 'E, Context> =
        Effect.serviceReference reference |> Effect.flatMap f

    /// The current wall-clock time in milliseconds. (Clock.currentTimeMillis)
    let currentTimeMillis<'E> : Effect<int64, 'E, Context> =
        clockWith (fun c -> Effect.sync (fun () -> c.CurrentTimeMillisUnsafe()))

    /// Sleep for `duration`, bridging to `Task.Delay`. (Clock.sleep)
    let sleep<'E> (duration: Duration) : Effect<unit, 'E, Context> =
        clockWith (fun c ->
            Effect(fun _ _ ->
                async {
                    do! c.SleepUnsafe duration
                    return Success()
                }))
