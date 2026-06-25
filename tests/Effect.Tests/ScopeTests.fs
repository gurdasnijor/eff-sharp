module Effect.Tests.ScopeTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/Scope.test.ts, adapted to
// the F# port's `Scope` (a lock-backed finalizer registry) plus the
// `Effect.ensuring` / `Effect.acquireUseRelease` / `Scope.acquireRelease` /
// `Scope.scoped` combinators built on it.

let private run eff = Effect.runSync () eff

[<Fact>]
let ``close runs finalizers in LIFO order`` () =
    let order = System.Collections.Generic.List<string>()
    let scope: Scope<string, unit> = Scope.make ()

    let program =
        effect {
            do! Scope.addFinalizer scope (Effect.sync (fun () -> order.Add "a"))
            do! Scope.addFinalizer scope (Effect.sync (fun () -> order.Add "b"))
            do! Scope.addFinalizer scope (Effect.sync (fun () -> order.Add "c"))
            do! Scope.close scope (Success(box ()))
        }

    run program |> ignore
    Assert.Equal<string list>([ "c"; "b"; "a" ], List.ofSeq order)

[<Fact>]
let ``close is idempotent - finalizers run once`` () =
    let runs = ref 0
    let scope: Scope<string, unit> = Scope.make ()

    let program =
        effect {
            do! Scope.addFinalizer scope (Effect.sync (fun () -> incr runs))
            do! Scope.close scope (Success(box ()))
            do! Scope.close scope (Success(box ()))
        }

    run program |> ignore
    Assert.Equal(1, runs.Value)

[<Fact>]
let ``acquireRelease registers a finalizer that runs on scope close`` () =
    let released = ref false
    let scope: Scope<string, unit> = Scope.make ()

    let program =
        effect {
            let! resource =
                Scope.acquireRelease scope (Effect.succeed "conn") (fun _ _ ->
                    Effect.sync (fun () -> released.Value <- true))

            // resource is live; finalizer not yet run
            Assert.False(released.Value)
            do! Scope.close scope (Success(box ()))
            return resource
        }

    Assert.Equal<Exit<string, string>>(Exit.succeed "conn", run program)
    Assert.True(released.Value)

[<Fact>]
let ``ensuring runs the finalizer on success`` () =
    let ran = ref false

    let program =
        Effect.succeed 1 |> Effect.ensuring (Effect.sync (fun () -> ran.Value <- true))

    Assert.Equal<Exit<int, string>>(Exit.succeed 1, run program)
    Assert.True(ran.Value)

[<Fact>]
let ``ensuring runs the finalizer on typed failure`` () =
    let ran = ref false

    let program: Effect<int, string, unit> =
        Effect.fail "boom"
        |> Effect.ensuring (Effect.sync (fun () -> ran.Value <- true))

    Assert.Equal<Exit<int, string>>(Exit.fail "boom", run program)
    Assert.True(ran.Value)

[<Fact>]
let ``acquireUseRelease releases on success with the use exit`` () =
    let log = System.Collections.Generic.List<string>()

    let program: Effect<int, string, unit> =
        Effect.acquireUseRelease
            (Effect.sync (fun () ->
                log.Add "acquire"
                10))
            (fun r ->
                log.Add "use"
                Effect.succeed (r * 2))
            (fun _ exit ->
                Effect.sync (fun () ->
                    match exit with
                    | Success v -> log.Add(sprintf "release:%d" v)
                    | Failure _ -> log.Add "release:fail"))

    Assert.Equal<Exit<int, string>>(Exit.succeed 20, run program)
    Assert.Equal<string list>([ "acquire"; "use"; "release:20" ], List.ofSeq log)

[<Fact>]
let ``acquireUseRelease releases even when use fails`` () =
    let released = ref false

    let program: Effect<int, string, unit> =
        Effect.acquireUseRelease (Effect.succeed 1) (fun _ -> Effect.fail "nope") (fun _ _ ->
            Effect.sync (fun () -> released.Value <- true))

    Assert.Equal<Exit<int, string>>(Exit.fail "nope", run program)
    Assert.True(released.Value)

[<Fact>]
let ``acquireUseRelease skips release when acquire fails`` () =
    let released = ref false

    let program: Effect<int, string, unit> =
        Effect.acquireUseRelease (Effect.fail "acq") (fun _ -> Effect.succeed 1) (fun _ _ ->
            Effect.sync (fun () -> released.Value <- true))

    Assert.Equal<Exit<int, string>>(Exit.fail "acq", run program)
    Assert.False(released.Value)

[<Fact>]
let ``scoped closes its scope on success`` () =
    let released = ref false

    let program: Effect<string, string, unit> =
        Scope.scoped (fun scope ->
            effect {
                let! r =
                    Scope.acquireRelease scope (Effect.succeed "db") (fun _ _ ->
                        Effect.sync (fun () -> released.Value <- true))

                return r
            })

    Assert.Equal<Exit<string, string>>(Exit.succeed "db", run program)
    Assert.True(released.Value)

[<Fact>]
let ``scoped closes its scope on failure`` () =
    let released = ref false

    let program: Effect<string, string, unit> =
        Scope.scoped (fun scope ->
            effect {
                do!
                    Scope.addFinalizer scope (Effect.sync (fun () -> released.Value <- true))
                    |> Effect.map ignore

                return! Effect.fail "boom"
            })

    Assert.Equal<Exit<string, string>>(Exit.fail "boom", run program)
    Assert.True(released.Value)
