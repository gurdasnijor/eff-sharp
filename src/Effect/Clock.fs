namespace Effect

open System
open System.Threading.Tasks

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
/// (`currentTimeNanos`) is dropped — .NET wall-clock is millisecond-grained — as
/// is the `Context.Reference` default-service plumbing (a plain `Tag` is used).

/// The time service. `CurrentTimeMillisUnsafe` reads wall-clock millis; `SleepUnsafe`
/// returns a `Task` that completes after the given duration.
type Clock =
    { CurrentTimeMillisUnsafe: unit -> int64
      SleepUnsafe: Duration -> Task }

[<RequireQualifiedAccess>]
module Clock =

    /// The `Tag` under which the `Clock` service is stored. (Clock.Clock)
    let tag: Tag<Clock> = Tag.make<Clock> "effect/Clock"

    /// The live, wall-clock implementation. (effect.ClockRef default)
    let make () : Clock =
        { CurrentTimeMillisUnsafe = fun () -> DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()
          SleepUnsafe =
            fun d ->
                let ms = Duration.toMillis d

                if Double.IsNaN ms || ms <= 0.0 then
                    Task.CompletedTask
                elif Double.IsInfinity ms then
                    // an "infinite" sleep never completes; approximate with the max delay.
                    Task.Delay(TimeSpan.FromMilliseconds(float Int32.MaxValue))
                else
                    Task.Delay(TimeSpan.FromMilliseconds(min ms (float Int32.MaxValue))) }

    /// A `Context` carrying the live clock. (Clock layer)
    let live: Context = Context.make tag (make ())

    /// Use the active `Clock` service to build an effect. (Clock.clockWith)
    let clockWith (f: Clock -> Effect<'A, 'E, Context>) : Effect<'A, 'E, Context> =
        Effect.service tag |> Effect.flatMap f

    /// The current wall-clock time in milliseconds. (Clock.currentTimeMillis)
    let currentTimeMillis<'E> : Effect<int64, 'E, Context> =
        clockWith (fun c -> Effect.sync (fun () -> c.CurrentTimeMillisUnsafe()))

    /// Sleep for `duration`, bridging to `Task.Delay`. (Clock.sleep)
    let sleep<'E> (duration: Duration) : Effect<unit, 'E, Context> =
        clockWith (fun c ->
            Effect(fun _ _ ->
                async {
                    do! Async.AwaitTask(c.SleepUnsafe duration)
                    return Success()
                }))
