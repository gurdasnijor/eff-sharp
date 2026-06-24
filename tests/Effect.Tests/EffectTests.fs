module Effect.Tests.EffectTests

open System.Threading.Tasks
open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/Effect.test.ts.
//
// Slice 3 re-encodes the core as an *asynchronous* Reader over `Async<Exit>`.
// The slice-1 behaviours below are preserved (now asserted on `Exit` instead of
// a bare `Result`), and a representative *async* subset is added (success,
// typed failure, exception-as-defect, tryPromise success/failure, flatMap across
// async boundaries, environment threading). The full upstream suite (fibers,
// interruption, forEach/all concurrency, ...) is ported incrementally in later
// slices; see PORTING.md.

type DivError = DivByZero
type Config = { Factor: int }

let private safeDiv a b : Effect<int, DivError, 'R> =
    if b = 0 then Effect.fail DivByZero else Effect.succeed (a / b)

/// Run an async effect to completion for assertions.
let private exitOf env eff = Effect.runSync env eff

// ----------------------------------------------------------------------------
// Slice-1 behaviours, preserved under the async core (asserted on Exit)
// ----------------------------------------------------------------------------

[<Fact>]
let ``succeed yields the value`` () =
    Assert.Equal<Exit<int, DivError>>(Exit.succeed 42, exitOf () (Effect.succeed 42))

[<Fact>]
let ``fail yields the typed error`` () =
    Assert.Equal<Exit<int, DivError>>(Exit.fail DivByZero, exitOf () (Effect.fail DivByZero))

[<Fact>]
let ``map transforms success`` () =
    Assert.Equal<Exit<int, DivError>>(Exit.succeed 42, exitOf () (Effect.succeed 21 |> Effect.map ((*) 2)))

[<Fact>]
let ``flatMap sequences on success`` () =
    let e = Effect.succeed 84 |> Effect.flatMap (fun x -> safeDiv x 2)
    Assert.Equal<Exit<int, DivError>>(Exit.succeed 42, exitOf () e)

[<Fact>]
let ``flatMap short-circuits on error`` () =
    let e = Effect.succeed 84 |> Effect.flatMap (fun x -> safeDiv x 0)
    Assert.Equal<Exit<int, DivError>>(Exit.fail DivByZero, exitOf () e)

[<Fact>]
let ``catchAll recovers from a typed error`` () =
    let e = Effect.fail DivByZero |> Effect.catchAll (fun _ -> Effect.succeed -1)
    Assert.Equal<Exit<int, DivError>>(Exit.succeed -1, exitOf () e)

[<Fact>]
let ``mapError transforms the typed error`` () =
    let e = Effect.fail DivByZero |> Effect.mapError (fun _ -> "div")
    Assert.Equal<Exit<int, string>>(Exit.fail "div", exitOf () e)

[<Fact>]
let ``environment injects dependencies`` () =
    let program: Effect<int, DivError, Config> =
        Effect.environmentWith (fun c -> c.Factor) |> Effect.map ((*) 10)

    Assert.Equal<Exit<int, DivError>>(Exit.succeed 50, exitOf { Factor = 5 } program)

[<Fact>]
let ``effect builder threads value and environment`` () =
    let ce: Effect<int, DivError, Config> =
        effect {
            let! cfg = Effect.environment<Config, DivError>
            let! d = safeDiv 100 cfg.Factor
            return d + 1
        }

    Assert.Equal<Exit<int, DivError>>(Exit.succeed 26, exitOf { Factor = 4 } ce)

[<Fact>]
let ``effect builder propagates typed failure`` () =
    let ce: Effect<int, DivError, Config> =
        effect {
            let! cfg = Effect.environment<Config, DivError>
            let! d = safeDiv 100 (cfg.Factor - cfg.Factor)
            return d
        }

    Assert.Equal<Exit<int, DivError>>(Exit.fail DivByZero, exitOf { Factor = 4 } ce)

// ----------------------------------------------------------------------------
// Async subset (ported from Effect.test.ts: promise/tryPromise/try/runPromise)
// ----------------------------------------------------------------------------

/// A genuinely-async task that yields before producing a value.
let private delayed (v: 'a) : Task<'a> =
    task {
        do! Task.Delay 1
        return v
    }

[<Fact>]
let ``promise succeeds with the resolved value`` () =
    let e: Effect<int, DivError, unit> = Effect.promise (fun () -> delayed 42)
    Assert.Equal<Exit<int, DivError>>(Exit.succeed 42, exitOf () e)

[<Fact>]
let ``runAsync returns the success value`` () =
    let e: Effect<int, DivError, unit> = Effect.promise (fun () -> delayed 7)
    let v = Effect.runAsync () e |> Async.RunSynchronously
    Assert.Equal(7, v)

[<Fact>]
let ``async typed failure lands in the Exit`` () =
    // promise then fail: the failure crosses an async boundary.
    let e: Effect<int, DivError, unit> =
        Effect.promise (fun () -> delayed 1) |> Effect.flatMap (fun _ -> Effect.fail DivByZero)

    Assert.Equal<Exit<int, DivError>>(Exit.fail DivByZero, exitOf () e)

[<Fact>]
let ``sync maps a thrown exception to a defect`` () =
    let e: Effect<int, DivError, unit> = Effect.sync (fun () -> failwith "boom")

    match exitOf () e with
    | Failure cause ->
        match Cause.defects cause with
        | [ :? System.Exception as ex ] -> Assert.Equal("boom", ex.Message)
        | other -> Assert.Fail(sprintf "expected one exception defect, got %A" other)
    | Success v -> Assert.Fail(sprintf "expected a defect, got Success %d" v)

[<Fact>]
let ``tryPromise succeeds with the resolved value`` () =
    let e: Effect<int, DivError, unit> = Effect.tryPromise (fun () -> delayed 99)
    Assert.Equal<Exit<int, DivError>>(Exit.succeed 99, exitOf () e)

[<Fact>]
let ``tryPromise maps a rejection to a defect`` () =
    let failing () : Task<int> =
        Task.FromException<int>(System.InvalidOperationException "nope")

    let e: Effect<int, DivError, unit> = Effect.tryPromise failing

    match exitOf () e with
    | Failure cause ->
        match Cause.defects cause with
        | [ :? System.InvalidOperationException as ex ] -> Assert.Equal("nope", ex.Message)
        | other -> Assert.Fail(sprintf "expected one exception defect, got %A" other)
    | Success v -> Assert.Fail(sprintf "expected a defect, got Success %d" v)

[<Fact>]
let ``flatMap sequences across async boundaries`` () =
    let e: Effect<int, DivError, unit> =
        Effect.promise (fun () -> delayed 10)
        |> Effect.flatMap (fun x -> Effect.promise (fun () -> delayed (x * 2)))

    Assert.Equal<Exit<int, DivError>>(Exit.succeed 20, exitOf () e)

[<Fact>]
let ``environment threads through an async effect`` () =
    let program: Effect<int, DivError, Config> =
        effect {
            let! cfg = Effect.environment<Config, DivError>
            let! doubled = Effect.promise (fun () -> delayed (cfg.Factor * 2))
            return doubled + 1
        }

    Assert.Equal<Exit<int, DivError>>(Exit.succeed 13, exitOf { Factor = 6 } program)

[<Fact>]
let ``runAsync re-raises an exception defect`` () =
    let e: Effect<int, DivError, unit> = Effect.sync (fun () -> failwith "kaboom")

    let ex =
        Assert.ThrowsAny<System.Exception>(fun () -> Effect.runAsync () e |> Async.RunSynchronously |> ignore)

    Assert.Equal("kaboom", ex.Message)
