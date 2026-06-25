module ClockSpec

open Effect
open Effect.Vitest

describe "Clock" (fun () ->
    itLive "currentTimeMillis reads the live wall clock" (fun () ->
        Clock.currentTimeMillis
        |> Effect.map (fun now -> toBeGreaterThan (float now) 0.0))

    itLive "sleep of zero completes immediately" (fun () ->
        Clock.sleep (Duration.millis 0.0))

    itEffect "itEffect injects a test clock" (fun () ->
        Clock.currentTimeMillis
        |> Effect.map (fun now -> toBe now 0L))

    itEffectEnv "TestClock can be advanced in the Context" (fun env ->
        TestClock.adjust env.Clock (Duration.millis 123.0)
        |> Effect.zipRight Clock.currentTimeMillis
        |> Effect.map (fun now -> toBe now 123L))

    itEffectIn
        (Context.make
            Clock.tag
            { CurrentTimeMillisUnsafe = fun () -> 456L
              SleepUnsafe = fun _ -> async { return () } })
        "custom Clock context can be injected"
        (fun () ->
            Clock.currentTimeMillis
            |> Effect.map (fun now -> toBe now 456L)))
