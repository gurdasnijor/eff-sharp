module Effect.Tests.CacheTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/Cache.test.ts.
//
// Upstream drives each case through `Effect.gen` + `TestClock.adjust`. This port
// runs each `Cache` operation with `Effect.runSync` over a `Context` carrying a
// *controllable* clock (a mutable epoch-millis cell), advancing time by mutating
// that cell between runs — the direct analogue of `TestClock.adjust`.

let private mkClock (time: int64 ref) : Clock =
    { CurrentTimeMillisUnsafe = fun () -> time.Value
      SleepUnsafe = fun _ -> System.Threading.Tasks.Task.CompletedTask }

/// Run a cache effect against the controllable clock, returning its `Exit`.
let private run (time: int64 ref) (eff: Effect<'A, 'E, Context>) : Exit<'A, 'E> =
    Effect.runSync (Context.make Clock.tag (mkClock time)) eff

let private get (time: int64 ref) (eff: Effect<'A, 'E, Context>) : 'A =
    match run time eff with
    | Success a -> a
    | Failure c -> failwithf "unexpected failure: %s" (Cause.render c)

// ----------------------------------------------------------------------------
// hit / miss
// ----------------------------------------------------------------------------

[<Fact>]
let ``get - miss invokes lookup, hit serves from cache (lookup runs once)`` () =
    let time = ref 0L
    let count = ref 0

    let cache: Cache<string, int, string> =
        get
            time
            (Cache.make 10 (fun (k: string) ->
                Effect.sync (fun () ->
                    count.Value <- count.Value + 1
                    k.Length)))

    Assert.Equal<Exit<int, string>>(Exit.succeed 5, run time (Cache.get cache "hello"))
    Assert.Equal<Exit<int, string>>(Exit.succeed 5, run time (Cache.get cache "hello"))
    Assert.Equal(1, count.Value)

[<Fact>]
let ``get - caches failures`` () =
    let time = ref 0L
    let count = ref 0

    let cache: Cache<string, int, string> =
        get
            time
            (Cache.make 10 (fun (k: string) ->
                Effect.suspend (fun () ->
                    count.Value <- count.Value + 1

                    if k = "error" then
                        Effect.fail "boom"
                    else
                        Effect.succeed k.Length)))

    Assert.Equal<Exit<int, string>>(Exit.fail "boom", run time (Cache.get cache "error"))
    Assert.Equal<Exit<int, string>>(Exit.fail "boom", run time (Cache.get cache "error"))
    Assert.Equal(1, count.Value) // failure cached, lookup not repeated

[<Fact>]
let ``getOption - None before, Some after a get`` () =
    let time = ref 0L

    let cache: Cache<string, int, string> =
        get time (Cache.make 10 (fun (k: string) -> Effect.succeed k.Length))

    Assert.Equal<Exit<int option, string>>(Exit.succeed None, run time (Cache.getOption cache "hello"))
    run time (Cache.get cache "hello") |> ignore
    Assert.Equal<Exit<int option, string>>(Exit.succeed (Some 5), run time (Cache.getOption cache "hello"))

[<Fact>]
let ``getSuccess - only resolved successes`` () =
    let time = ref 0L

    let cache: Cache<string, int, string> =
        get
            time
            (Cache.make 10 (fun (k: string) ->
                if k = "error" then
                    Effect.fail "boom"
                else
                    Effect.succeed k.Length))

    Assert.Equal<Exit<int option, string>>(Exit.succeed None, run time (Cache.getSuccess cache "hello"))
    run time (Cache.get cache "hello") |> ignore
    run time (Cache.get cache "error") |> ignore
    Assert.Equal<Exit<int option, string>>(Exit.succeed (Some 5), run time (Cache.getSuccess cache "hello"))
    Assert.Equal<Exit<int option, string>>(Exit.succeed None, run time (Cache.getSuccess cache "error"))

// ----------------------------------------------------------------------------
// set / has / invalidate
// ----------------------------------------------------------------------------

[<Fact>]
let ``set - overrides the lookup value`` () =
    let time = ref 0L

    let cache: Cache<string, int, string> =
        get time (Cache.make 100 (fun (k: string) -> Effect.succeed k.Length))

    Assert.Equal(4, get time (Cache.get cache "test"))
    run time (Cache.set cache "test" 999) |> ignore
    Assert.Equal(999, get time (Cache.get cache "test"))

[<Fact>]
let ``has / invalidate`` () =
    let time = ref 0L

    let cache: Cache<string, int, string> =
        get time (Cache.make 10 (fun (k: string) -> Effect.succeed k.Length))

    Assert.False(get time (Cache.has cache "hello"))
    run time (Cache.get cache "hello") |> ignore
    Assert.True(get time (Cache.has cache "hello"))
    run time (Cache.invalidate cache "hello") |> ignore
    Assert.False(get time (Cache.has cache "hello"))

[<Fact>]
let ``invalidate - lookup runs again afterwards`` () =
    let time = ref 0L
    let count = ref 0

    let cache: Cache<string, int, string> =
        get
            time
            (Cache.make 10 (fun (k: string) ->
                Effect.sync (fun () ->
                    count.Value <- count.Value + 1
                    k.Length)))

    run time (Cache.get cache "test") |> ignore
    run time (Cache.invalidate cache "test") |> ignore
    run time (Cache.get cache "test") |> ignore
    Assert.Equal(2, count.Value)

[<Fact>]
let ``invalidateWhen - removes only when the predicate matches`` () =
    let time = ref 0L

    let cache: Cache<string, int, string> =
        get time (Cache.make 10 (fun (k: string) -> Effect.succeed k.Length))

    run time (Cache.get cache "hello") |> ignore // 5
    run time (Cache.get cache "hi") |> ignore // 2
    Assert.True(get time (Cache.invalidateWhen cache "hello" (fun v -> v = 5)))
    Assert.False(get time (Cache.has cache "hello"))
    Assert.False(get time (Cache.invalidateWhen cache "hi" (fun v -> v = 5)))
    Assert.True(get time (Cache.has cache "hi"))

[<Fact>]
let ``invalidateAll / size`` () =
    let time = ref 0L

    let cache: Cache<string, int, string> =
        get time (Cache.make 10 (fun (k: string) -> Effect.succeed k.Length))

    run time (Cache.get cache "a") |> ignore
    run time (Cache.get cache "bb") |> ignore
    Assert.Equal(2, get time (Cache.size cache))
    run time (Cache.invalidateAll cache) |> ignore
    Assert.Equal(0, get time (Cache.size cache))

// ----------------------------------------------------------------------------
// refresh
// ----------------------------------------------------------------------------

[<Fact>]
let ``refresh - always re-runs lookup and overwrites`` () =
    let time = ref 0L
    let counter = ref 0

    let cache: Cache<string, string, string> =
        get
            time
            (Cache.make 10 (fun (k: string) ->
                Effect.sync (fun () ->
                    counter.Value <- counter.Value + 1
                    sprintf "%s-%d" k counter.Value)))

    Assert.Equal("user-1", get time (Cache.get cache "user"))
    Assert.Equal("user-1", get time (Cache.get cache "user")) // cached
    Assert.Equal("user-2", get time (Cache.refresh cache "user")) // re-run
    Assert.Equal("user-2", get time (Cache.get cache "user"))

// ----------------------------------------------------------------------------
// TTL (controllable clock)
// ----------------------------------------------------------------------------

[<Fact>]
let ``make - default infinite TTL never expires`` () =
    let time = ref 0L

    let cache: Cache<string, int, string> =
        get time (Cache.make 10 (fun (k: string) -> Effect.succeed k.Length))

    run time (Cache.get cache "test") |> ignore
    time.Value <- 1000L * 60L * 60L * 1000L // 1000 hours later
    Assert.True(get time (Cache.has cache "test"))

[<Fact>]
let ``makeWithTtl - entries expire after the TTL`` () =
    let time = ref 0L

    let cache: Cache<string, int, string> =
        get time (Cache.makeWithTtl 10 (Duration.hours 1.0) (fun (k: string) -> Effect.succeed k.Length))

    run time (Cache.get cache "test") |> ignore
    time.Value <- int64 (30.0 * 60.0 * 1000.0) // 30 min
    Assert.True(get time (Cache.has cache "test"))
    time.Value <- int64 (61.0 * 60.0 * 1000.0) // 61 min
    Assert.False(get time (Cache.has cache "test"))

[<Fact>]
let ``makeWith - dynamic TTL by exit`` () =
    let time = ref 0L

    let ttl (exit: Exit<int, string>) (_key: string) =
        match exit with
        | Success _ -> Duration.hours 1.0
        | Failure _ -> Duration.seconds 1.0

    let cache: Cache<string, int, string> =
        get
            time
            (Cache.makeWith 10 ttl (fun (k: string) ->
                if k = "fail" then
                    Effect.fail "boom"
                else
                    Effect.succeed k.Length))

    run time (Cache.get cache "ok") |> ignore
    run time (Cache.get cache "fail") |> ignore
    Assert.True(get time (Cache.has cache "ok"))
    Assert.True(get time (Cache.has cache "fail"))
    time.Value <- int64 (2.0 * 1000.0) // 2 s: failure expired, success still alive
    Assert.False(get time (Cache.has cache "fail"))
    Assert.True(get time (Cache.has cache "ok"))

// ----------------------------------------------------------------------------
// capacity
// ----------------------------------------------------------------------------

[<Fact>]
let ``capacity - exceeding it evicts the oldest entry`` () =
    let time = ref 0L

    let cache: Cache<string, int, string> =
        get time (Cache.make 2 (fun (k: string) -> Effect.succeed k.Length))

    run time (Cache.set cache "a" 1) |> ignore
    run time (Cache.set cache "b" 2) |> ignore
    Assert.Equal(2, get time (Cache.size cache))
    run time (Cache.set cache "c" 3) |> ignore
    Assert.Equal(2, get time (Cache.size cache))
    Assert.False(get time (Cache.has cache "a")) // oldest evicted
    Assert.True(get time (Cache.has cache "c"))
