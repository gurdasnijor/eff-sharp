module Effect.Tests.RuntimeTests

open Xunit
open Effect

// Ported to the F# `Runtime` bundle: a `Context` of services + the `run*` family.
// Upstream effect-smol's `Runtime.ts` is the platform `runMain` layer (deferred —
// needs fiber observers); these cover the runtime *bundle* the brief specifies,
// mirroring the `Effect.run*` test patterns.

type Config = { Factor: int }

let private configTag: Tag<Config> = Tag.make<Config> "test/Config"

// --- default runtime ---

[<Fact>]
let ``defaultRuntime runs a pure effect to its value`` () =
    Assert.Equal<Exit<int, string>>(Exit.succeed 42, Runtime.runSyncExit Runtime.defaultRuntime (Effect.succeed 42))

[<Fact>]
let ``runSync returns the success value`` () =
    Assert.Equal(42, Runtime.runSync Runtime.defaultRuntime (Effect.succeed 42))

[<Fact>]
let ``runSyncExit surfaces a typed failure as an Exit`` () =
    Assert.Equal<Exit<int, string>>(Exit.fail "boom", Runtime.runSyncExit Runtime.defaultRuntime (Effect.fail "boom"))

[<Fact>]
let ``runSync raises on a typed failure`` () =
    Assert.ThrowsAny<System.Exception>(fun () -> Runtime.runSync Runtime.defaultRuntime (Effect.fail "boom") |> ignore)
    |> ignore

// --- make from a Context ---

[<Fact>]
let ``make builds a runtime whose context provides services`` () =
    let runtime = Runtime.make (Context.make configTag { Factor = 10 })

    let program: Effect<int, string, Context> =
        Effect.service configTag |> Effect.map (fun cfg -> cfg.Factor * 5)

    Assert.Equal<Exit<int, string>>(Exit.succeed 50, Runtime.runSyncExit runtime program)

[<Fact>]
let ``context exposes the bundled services`` () =
    let ctx = Context.make configTag { Factor = 7 }
    let runtime = Runtime.make ctx
    Assert.Equal(7, (Context.get configTag (Runtime.context runtime)).Factor)

[<Fact>]
let ``a missing service dies as a defect`` () =
    let program: Effect<int, string, Context> =
        Effect.service configTag |> Effect.map (fun c -> c.Factor)

    match Runtime.runSyncExit Runtime.defaultRuntime program with
    | Failure cause -> Assert.False(List.isEmpty (Cause.defects cause), "expected a defect")
    | Success v -> Assert.Fail(sprintf "expected a defect, got %d" v)

// --- async runners ---

[<Fact>]
let ``runExit returns the Exit asynchronously`` () =
    let exit =
        Runtime.runExit Runtime.defaultRuntime (Effect.succeed 7)
        |> Async.RunSynchronously

    Assert.Equal<Exit<int, string>>(Exit.succeed 7, exit)

[<Fact>]
let ``runPromise resolves with the success value`` () =
    let value =
        Runtime.runPromise Runtime.defaultRuntime (Effect.succeed 99)
        |> Async.AwaitTask
        |> Async.RunSynchronously

    Assert.Equal(99, value)

[<Fact>]
let ``runPromiseExit resolves with the Exit even on failure`` () =
    let exit =
        Runtime.runPromiseExit Runtime.defaultRuntime (Effect.fail "boom")
        |> Async.AwaitTask
        |> Async.RunSynchronously

    Assert.Equal<Exit<int, string>>(Exit.fail "boom", exit)

// --- fork ---

[<Fact>]
let ``runFork starts a fiber whose result can be joined`` () =
    let runtime = Runtime.make (Context.make configTag { Factor = 4 })

    let work: Effect<int, string, Context> =
        effect {
            do! Effect.sleep 5
            let! cfg = Effect.service configTag
            return cfg.Factor * 100
        }

    let fiber = Runtime.runFork runtime work
    // Join the fiber back through the runtime.
    Assert.Equal<Exit<int, string>>(Exit.succeed 400, Runtime.runSyncExit runtime (Fiber.join fiber))
