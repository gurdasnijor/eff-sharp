module Effect.Tests.ClockTests

open System.Diagnostics
open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/Clock.test.ts, adapted to
// the F# port's `Clock` service (`Tag`/`Context`-based) and `Duration`.

[<Fact>]
let ``currentTimeMillis reads the live wall clock`` () =
    let program: Effect<int64, string, Context> = Clock.currentTimeMillis
    let before = System.DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()

    match Effect.runSync Clock.live program with
    | Success now -> Assert.True(now >= before, sprintf "expected %d >= %d" now before)
    | Failure c -> Assert.Fail(Cause.render c)

[<Fact>]
let ``sleep delays for at least the duration`` () =
    let program: Effect<unit, string, Context> = Clock.sleep (Duration.millis 30.0)
    let sw = Stopwatch.StartNew()
    Effect.runSync Clock.live program |> ignore
    sw.Stop()
    // allow scheduler slack but require a real delay occurred
    Assert.True(sw.ElapsedMilliseconds >= 20L, sprintf "elapsed %dms" sw.ElapsedMilliseconds)

[<Fact>]
let ``sleep of zero completes immediately`` () =
    let program: Effect<unit, string, Context> = Clock.sleep (Duration.millis 0.0)
    Assert.Equal<Exit<unit, string>>(Exit.succeed (), Effect.runSync Clock.live program)

[<Fact>]
let ``a test clock can be injected via the Context`` () =
    // DI lets tests swap the clock for a controlled one.
    let testClock: Clock =
        { CurrentTimeMillisUnsafe = fun () -> 123L
          SleepUnsafe = fun _ -> System.Threading.Tasks.Task.CompletedTask }

    let program: Effect<int64, string, Context> = Clock.currentTimeMillis
    let exit = Effect.runSync (Context.make Clock.tag testClock) program
    Assert.Equal<Exit<int64, string>>(Exit.succeed 123L, exit)

[<Fact>]
let ``clockWith exposes the full service`` () =
    let program: Effect<int64, string, Context> =
        Clock.clockWith (fun c -> Effect.succeed (c.CurrentTimeMillisUnsafe()))

    match Effect.runSync Clock.live program with
    | Success v -> Assert.True(v > 0L)
    | Failure c -> Assert.Fail(Cause.render c)
