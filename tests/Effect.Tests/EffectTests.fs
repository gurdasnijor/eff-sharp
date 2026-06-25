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
    if b = 0 then
        Effect.fail DivByZero
    else
        Effect.succeed (a / b)

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
        Effect.promise (fun () -> delayed 1)
        |> Effect.flatMap (fun _ -> Effect.fail DivByZero)

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

// ----------------------------------------------------------------------------
// effect { } extended builder: for / while / try-finally / use! (w2-di)
// ----------------------------------------------------------------------------

[<Fact>]
let ``effect for-loop sequences the body over a collection`` () =
    let total = ref 0

    let program: Effect<int, DivError, unit> =
        effect {
            for x in [ 1; 2; 3; 4 ] do
                do! Effect.sync (fun () -> total.Value <- total.Value + x)

            return total.Value
        }

    Assert.Equal<Exit<int, DivError>>(Exit.succeed 10, exitOf () program)

[<Fact>]
let ``effect for-loop short-circuits on failure`` () =
    let seen = System.Collections.Generic.List<int>()

    let program: Effect<unit, DivError, unit> =
        effect {
            for x in [ 1; 2; 3 ] do
                do!
                    (if x = 2 then
                         Effect.fail DivByZero
                     else
                         Effect.sync (fun () -> seen.Add x))
        }

    Assert.Equal<Exit<unit, DivError>>(Exit.fail DivByZero, exitOf () program)
    Assert.Equal<int list>([ 1 ], List.ofSeq seen)

[<Fact>]
let ``effect while-loop repeats until the guard is false`` () =
    let i = ref 0
    let acc = ref 0

    let program: Effect<int, DivError, unit> =
        effect {
            while i.Value < 5 do
                do!
                    Effect.sync (fun () ->
                        acc.Value <- acc.Value + i.Value
                        i.Value <- i.Value + 1)

            return acc.Value
        }

    Assert.Equal<Exit<int, DivError>>(Exit.succeed 10, exitOf () program)

[<Fact>]
let ``effect try-finally runs the finalizer on success`` () =
    let ran = ref false

    let program: Effect<int, DivError, unit> =
        effect {
            try
                return 42
            finally
                ran.Value <- true
        }

    Assert.Equal<Exit<int, DivError>>(Exit.succeed 42, exitOf () program)
    Assert.True(ran.Value)

[<Fact>]
let ``effect try-finally runs the finalizer on failure`` () =
    let ran = ref false

    let program: Effect<int, DivError, unit> =
        effect {
            try
                return! Effect.fail DivByZero
            finally
                ran.Value <- true
        }

    Assert.Equal<Exit<int, DivError>>(Exit.fail DivByZero, exitOf () program)
    Assert.True(ran.Value)

/// A disposable that records its disposal into a shared log, for `use!` tests.
type private Tracked(name: string, log: System.Collections.Generic.List<string>) =
    member _.Name = name

    interface System.IDisposable with
        member _.Dispose() = log.Add name

[<Fact>]
let ``effect use! disposes the resource on success`` () =
    let log = System.Collections.Generic.List<string>()

    let program: Effect<string, DivError, unit> =
        effect {
            use! r = Effect.succeed (new Tracked("r", log))
            return r.Name
        }

    Assert.Equal<Exit<string, DivError>>(Exit.succeed "r", exitOf () program)
    Assert.Equal<string list>([ "r" ], List.ofSeq log)

[<Fact>]
let ``effect use! disposes in LIFO order`` () =
    let log = System.Collections.Generic.List<string>()

    let program: Effect<unit, DivError, unit> =
        effect {
            use! a = Effect.succeed (new Tracked("a", log))
            use! b = Effect.succeed (new Tracked("b", log))
            do! Effect.sync (fun () -> log.Add(a.Name + b.Name))
        }

    exitOf () program |> ignore
    Assert.Equal<string list>([ "ab"; "b"; "a" ], List.ofSeq log)

[<Fact>]
let ``effect use! disposes even on failure`` () =
    let log = System.Collections.Generic.List<string>()

    let program: Effect<int, DivError, unit> =
        effect {
            use! _r = Effect.succeed (new Tracked("r", log))
            return! Effect.fail DivByZero
        }

    Assert.Equal<Exit<int, DivError>>(Exit.fail DivByZero, exitOf () program)
    Assert.Equal<string list>([ "r" ], List.ofSeq log)

// ----------------------------------------------------------------------------
// catchAll defect semantics (upstream-faithful; also exercises retagDefects)
// ----------------------------------------------------------------------------

[<Fact>]
let ``catchAll does not recover a defect-only cause (Die propagates unchanged)`` () =
    let boom = System.Exception "boom"

    let e: Effect<int, DivError, unit> =
        Effect.failCause (Cause.die (box boom))
        |> Effect.catchAll (fun _ -> Effect.succeed 99)

    match exitOf () e with
    | Failure cause ->
        // The ORIGINAL defect must propagate — not a fabricated "unreachable" value.
        match Cause.defects cause with
        | [ :? System.Exception as ex ] -> Assert.Equal("boom", ex.Message)
        | other -> Assert.Fail(sprintf "expected the original Die defect, got %A" other)
    | Success v -> Assert.Fail(sprintf "expected Die to propagate, got Success %d" v)

[<Fact>]
let ``catchAll recovers the typed Fail even when a Die co-occurs (upstream-faithful)`` () =
    // Upstream `catch` recovers whenever a Fail exists, discarding the rest of
    // the cause (see Effect.ts catch_ -> catchCauseFilter + findError).
    let cause = Cause.combine (Cause.fail DivByZero) (Cause.die (box "extra"))
    let e = Effect.failCause cause |> Effect.catchAll (fun _ -> Effect.succeed 7)
    Assert.Equal<Exit<int, DivError>>(Exit.succeed 7, exitOf () e)

[<Fact>]
let ``exit captures success and failure as an Exit without short-circuiting`` () =
    // Success: the outer effect succeeds carrying the inner Success.
    match Effect.runSync () (Effect.exit (Effect.succeed 42)) with
    | Success inner -> Assert.Equal<Exit<int, string>>(Exit.succeed 42, inner)
    | Failure c -> failwithf "exit should not fail: %s" (Cause.render c)

    // Failure: exit NEVER fails the outer — it captures the inner Failure.
    match Effect.runSync () (Effect.exit (Effect.fail "boom")) with
    | Success inner ->
        match inner with
        | Failure c -> Assert.Contains("boom", Cause.render c)
        | Success _ -> failwith "expected the inner Exit to be a Failure"
    | Failure c -> failwithf "exit should not fail the outer: %s" (Cause.render c)
