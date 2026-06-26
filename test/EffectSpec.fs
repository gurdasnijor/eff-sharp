module EffectSpec

open Effect
open Effect.Vitest

// `itEffect` runs an Effect to a Promise on Node — the real runtime, not a proxy.
describe "Effect — runs on Node via Fable" (fun () ->
    itEffect "succeed |> map (+1)  ==  2" (fun () ->
        Effect.succeed 1
        |> Effect.map (fun n -> n + 1)
        |> Effect.map (fun x -> toBe x 2))

    itEffect "flatMap chain (10*2+1)  ==  21" (fun () ->
        Effect.succeed 10
        |> Effect.flatMap (fun a -> Effect.succeed (a * 2))
        |> Effect.flatMap (fun b -> Effect.succeed (b + 1))
        |> Effect.map (fun x -> toBe x 21))

    itEffect "compatibility helpers map to core semantics" (fun () ->
        Effect.succeed 2
        |> Effect.tap (fun value -> Effect.sync (fun () -> toBe value 2))
        |> Effect.filterOrFail (fun value -> value > 1) (fun value -> sprintf "too small: %d" value)
        |> Effect.andThen (Effect.succeed 10)
        |> Effect.asValue 11
        |> Effect.map (fun value -> toBe value 11))

    itEffect "flip swaps typed failure into success" (fun () ->
        Effect.fail "nope"
        |> Effect.flip
        |> Effect.map (fun error -> toBe error "nope")))

// Proves the ported TestClock actually runs on Node (Cell-backed, no TCS).
describe "TestClock — Fable-portable (Cell-backed)" (fun () ->
    test "adjust advances virtual time with no wall-clock wait" (fun () ->
        let tc = TestClock.make ()
        let clock = TestClock.clock tc
        toBe (int (clock.CurrentTimeMillisUnsafe())) 0
        Effect.runSync () (TestClock.adjust tc (Duration.millis 5000.0)) |> ignore
        toBe (int (clock.CurrentTimeMillisUnsafe())) 5000))
