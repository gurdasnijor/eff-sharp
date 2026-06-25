module RuntimeSpec

open Effect
open Effect.Vitest

type private Config = { Factor: int }

let private configTag: Tag<Config> = Tag.make<Config> "test/Config"

describe "Runtime" (fun () ->
    test "defaultRuntime runs pure effects" (fun () ->
        toEqual (Runtime.runSyncExit Runtime.defaultRuntime (Effect.succeed 42)) (Exit.succeed 42)
        toBe (Runtime.runSync Runtime.defaultRuntime (Effect.succeed 42)) 42
        toEqual (Runtime.runSyncExit Runtime.defaultRuntime (Effect.fail "boom")) (Exit.fail "boom")
        toThrow (fun () -> Runtime.runSync Runtime.defaultRuntime (Effect.fail "boom") |> ignore))

    test "make builds a runtime whose context provides services" (fun () ->
        let runtime = Runtime.make (Context.make configTag { Factor = 10 })

        let program: Effect<int, string, Context> =
            Effect.service configTag |> Effect.map (fun cfg -> cfg.Factor * 5)

        toEqual (Runtime.runSyncExit runtime program) (Exit.succeed 50))

    test "context exposes bundled services" (fun () ->
        let ctx = Context.make configTag { Factor = 7 }
        let runtime = Runtime.make ctx
        toBe ((Context.get configTag (Runtime.context runtime)).Factor) 7)

    test "a missing service dies as a defect" (fun () ->
        let program: Effect<int, string, Context> =
            Effect.service configTag |> Effect.map (fun c -> c.Factor)

        match Runtime.runSyncExit Runtime.defaultRuntime program with
        | Failure cause -> toBe (List.isEmpty (Cause.defects cause)) false
        | Success v -> failwithf "expected a defect, got %d" v)

    itEffect "runExit returns the Exit asynchronously" (fun () ->
        Effect(fun _ _ ->
            async {
                let! exit = Runtime.runExit Runtime.defaultRuntime (Effect.succeed 7)
                toEqual exit (Exit.succeed 7)
                return Success()
            }))

    itEffect "runPromise resolves with the success value" (fun () ->
        Effect(fun _ _ ->
            async {
                let! value = Runtime.runPromise Runtime.defaultRuntime (Effect.succeed 99)
                toBe value 99
                return Success()
            }))

    itEffect "runPromiseExit resolves with the Exit even on failure" (fun () ->
        Effect(fun _ _ ->
            async {
                let! exit = Runtime.runPromiseExit Runtime.defaultRuntime (Effect.fail "boom")
                toEqual exit (Exit.fail "boom")
                return Success()
            }))

    itEffect "runFork starts a fiber whose result can be joined" (fun () ->
        let runtime = Runtime.make (Context.make configTag { Factor = 4 })

        let work: Effect<int, string, Context> =
            effect {
                do! Effect.sleep 5
                let! cfg = Effect.service configTag
                return cfg.Factor * 100
            }

        let fiber = Runtime.runFork runtime work
        Fiber.join fiber
        |> Effect.provideContext (Runtime.context runtime)
        |> Effect.map (fun result -> toBe result 400)))
