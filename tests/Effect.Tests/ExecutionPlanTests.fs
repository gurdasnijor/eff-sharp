module Effect.Tests.ExecutionPlanTests

open Xunit
open Effect

// Upstream's ExecutionPlan tests exercise `Stream.withExecutionPlan` (deferred —
// see ExecutionPlan.fs). These Facts cover the ported data model + the
// Effect-level fallback executor (fall back through failing layers, per-step
// `attempts` retries, exhaustion propagates the last error, `make`/`merge`).

let private svc = Tag.make<int> "Svc"
let private eff: Effect<int, string, Context> = Effect.service svc

let private run (e: Effect<'A, 'E, unit>) : 'A =
    match Effect.runSync () e with
    | Success a -> a
    | Failure c -> failwithf "unexpected failure: %s" (Cause.render c)

let private failLayer (error: string) : Layer<string, unit> = Layer.effect svc (Effect.fail error)
let private okLayer (value: int) : Layer<string, unit> = Layer.succeed svc value

[<Fact>]
let ``falls back through failing layers`` () =
    let plan =
        ExecutionPlan.make
            [ ExecutionPlan.step (failLayer "A")
              ExecutionPlan.step (failLayer "B")
              ExecutionPlan.step (okLayer 3) ]

    Assert.Equal(3, run (ExecutionPlan.withExecutionPlan plan eff))

[<Fact>]
let ``retries within a step up to attempts`` () =
    let counter = ref 0

    let flaky: Layer<string, unit> =
        Layer.effect
            svc
            (Effect.suspend (fun () ->
                incr counter
                if counter.Value < 3 then Effect.fail "flaky" else Effect.succeed 7))

    let plan = ExecutionPlan.make [ ExecutionPlan.stepWith flaky 3 ]
    Assert.Equal(7, run (ExecutionPlan.withExecutionPlan plan eff))
    Assert.Equal(3, counter.Value)

[<Fact>]
let ``an exhausted plan propagates the last error`` () =
    let plan =
        ExecutionPlan.make [ ExecutionPlan.step (failLayer "A"); ExecutionPlan.step (failLayer "Z") ]

    match Effect.runSync () (ExecutionPlan.withExecutionPlan plan eff) with
    | Failure c -> Assert.Equal<string list>([ "Z" ], Cause.failures c)
    | Success _ -> Assert.Fail "expected failure"

[<Fact>]
let ``make rejects attempts less than 1`` () =
    Assert.Throws<System.Exception>(fun () -> ExecutionPlan.make [ ExecutionPlan.stepWith (okLayer 1) 0 ] |> ignore)
    |> ignore

[<Fact>]
let ``merge concatenates steps in order`` () =
    let p1 = ExecutionPlan.make [ ExecutionPlan.step (failLayer "A") ]
    let p2 = ExecutionPlan.make [ ExecutionPlan.step (okLayer 5) ]
    let merged = ExecutionPlan.merge [ p1; p2 ]
    Assert.Equal(2, List.length (ExecutionPlan.steps merged))
    Assert.Equal(5, run (ExecutionPlan.withExecutionPlan merged eff))
