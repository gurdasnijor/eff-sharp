module ExecutionPlanSpec

open Effect
open Effect.Vitest

let private svc = Tag.make<int> "ExecutionPlanSpec/Svc"
let private eff: Effect<int, string, Context> = Effect.service svc

let private run (e: Effect<'A, 'E, unit>) : 'A =
    match Effect.runSync () e with
    | Success a -> a
    | Failure c -> failwithf "unexpected failure: %s" (Cause.render c)

let private failLayer (error: string) : Layer<string, unit> = Layer.effect svc (Effect.fail error)
let private okLayer (value: int) : Layer<string, unit> = Layer.succeed svc value

describe "ExecutionPlan" (fun () ->
    test "falls back through failing layers" (fun () ->
        let plan =
            ExecutionPlan.make
                [ ExecutionPlan.step (failLayer "A")
                  ExecutionPlan.step (failLayer "B")
                  ExecutionPlan.step (okLayer 3) ]

        toBe (run (ExecutionPlan.withExecutionPlan plan eff)) 3)

    test "retries within a step up to attempts" (fun () ->
        let counter = ref 0

        let flaky: Layer<string, unit> =
            Layer.effect
                svc
                (Effect.suspend (fun () ->
                    counter.Value <- counter.Value + 1

                    if counter.Value < 3 then
                        Effect.fail "flaky"
                    else
                        Effect.succeed 7))

        let plan = ExecutionPlan.make [ ExecutionPlan.stepWith flaky 3 ]
        toBe (run (ExecutionPlan.withExecutionPlan plan eff)) 7
        toBe counter.Value 3)

    test "an exhausted plan propagates the last error" (fun () ->
        let plan =
            ExecutionPlan.make [ ExecutionPlan.step (failLayer "A"); ExecutionPlan.step (failLayer "Z") ]

        match Effect.runSync () (ExecutionPlan.withExecutionPlan plan eff) with
        | Failure c -> toEqual (Cause.failures c) [ "Z" ]
        | Success _ -> failwith "expected failure")

    test "make rejects attempts less than 1" (fun () ->
        toThrow (fun () -> ExecutionPlan.make [ ExecutionPlan.stepWith (okLayer 1) 0 ] |> ignore))

    test "merge concatenates steps in order" (fun () ->
        let p1 = ExecutionPlan.make [ ExecutionPlan.step (failLayer "A") ]
        let p2 = ExecutionPlan.make [ ExecutionPlan.step (okLayer 5) ]
        let merged = ExecutionPlan.merge [ p1; p2 ]
        toBe (List.length (ExecutionPlan.steps merged)) 2
        toBe (run (ExecutionPlan.withExecutionPlan merged eff)) 5))
