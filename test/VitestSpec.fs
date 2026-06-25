module VitestSpec

open Effect
open Effect.Vitest

type private Foo = { Value: string }
type private Bar = { Value: string }

let private fooTag = Tag.make<Foo> "vitest/Foo"
let private barTag = Tag.make<Bar> "vitest/Bar"

describe "Effect.Vitest" (fun () ->
    itEffect "itEffect provides a fresh TestClock by default" (fun () ->
        Clock.currentTimeMillis
        |> Effect.map (fun now -> toBe now 0L))

    itEffectEnv "itEffectEnv exposes the TestClock" (fun env ->
        TestClock.adjust env.Clock (Duration.millis 250.0)
        |> Effect.zipRight Clock.currentTimeMillis
        |> Effect.map (fun now -> toBe now 250L))

    itLive "itLive uses the live Clock" (fun () ->
        Clock.currentTimeMillis
        |> Effect.map (fun now -> toBeGreaterThan (float now) 0.0)))

layerNamed (Layer.succeed fooTag { Value = "foo" }) "Effect.Vitest.layer" (fun it ->
    it.effect "provides a shared layer context" (fun () ->
        Effect.service fooTag
        |> Effect.map (fun foo -> toBe foo.Value "foo"))

    it.layer (Layer.effect barTag (Effect.service fooTag |> Effect.map (fun foo -> { Value = foo.Value + ":bar" }))) (fun it ->
        it.effect "nests layers under the parent context" (fun () ->
            Effect.service barTag
            |> Effect.map (fun bar -> toBe bar.Value "foo:bar"))))

