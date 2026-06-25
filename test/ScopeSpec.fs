module ScopeSpec

open Effect
open Effect.Vitest

let private run eff = Effect.runSync () eff

describe "Scope" (fun () ->
    test "close runs finalizers in LIFO order" (fun () ->
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
        toBe (List.ofSeq order = [ "c"; "b"; "a" ]) true)

    test "close is idempotent - finalizers run once" (fun () ->
        let runs = ref 0
        let scope: Scope<string, unit> = Scope.make ()

        let program =
            effect {
                do! Scope.addFinalizer scope (Effect.sync (fun () -> runs.Value <- runs.Value + 1))
                do! Scope.close scope (Success(box ()))
                do! Scope.close scope (Success(box ()))
            }

        run program |> ignore
        toBe runs.Value 1)

    test "acquireRelease registers a finalizer that runs on scope close" (fun () ->
        let released = ref false
        let scope: Scope<string, unit> = Scope.make ()

        let program =
            effect {
                let! resource =
                    Scope.acquireRelease scope (Effect.succeed "conn") (fun _ _ ->
                        Effect.sync (fun () -> released.Value <- true))

                toBe released.Value false
                do! Scope.close scope (Success(box ()))
                return resource
            }

        toBe (run program = Exit.succeed "conn") true
        toBe released.Value true)

    test "ensuring runs the finalizer on success and failure" (fun () ->
        let ranSuccess = ref false
        let success = Effect.succeed 1 |> Effect.ensuring (Effect.sync (fun () -> ranSuccess.Value <- true))
        toBe (run success = Exit.succeed 1) true
        toBe ranSuccess.Value true

        let ranFailure = ref false

        let failure: Effect<int, string, unit> =
            Effect.fail "boom"
            |> Effect.ensuring (Effect.sync (fun () -> ranFailure.Value <- true))

        toBe (run failure = Exit.fail "boom") true
        toBe ranFailure.Value true)

    test "acquireUseRelease releases on success and failure" (fun () ->
        let log = System.Collections.Generic.List<string>()

        let success: Effect<int, string, unit> =
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

        toBe (run success = Exit.succeed 20) true
        toBe (List.ofSeq log = [ "acquire"; "use"; "release:20" ]) true

        let released = ref false

        let failure: Effect<int, string, unit> =
            Effect.acquireUseRelease (Effect.succeed 1) (fun _ -> Effect.fail "nope") (fun _ _ ->
                Effect.sync (fun () -> released.Value <- true))

        toBe (run failure = Exit.fail "nope") true
        toBe released.Value true)

    test "acquireUseRelease skips release when acquire fails" (fun () ->
        let released = ref false

        let program: Effect<int, string, unit> =
            Effect.acquireUseRelease (Effect.fail "acq") (fun _ -> Effect.succeed 1) (fun _ _ ->
                Effect.sync (fun () -> released.Value <- true))

        toBe (run program = Exit.fail "acq") true
        toBe released.Value false)

    test "scoped closes its scope on success and failure" (fun () ->
        let releasedSuccess = ref false

        let success: Effect<string, string, unit> =
            Scope.scoped (fun scope ->
                effect {
                    let! r =
                        Scope.acquireRelease scope (Effect.succeed "db") (fun _ _ ->
                            Effect.sync (fun () -> releasedSuccess.Value <- true))

                    return r
                })

        toBe (run success = Exit.succeed "db") true
        toBe releasedSuccess.Value true

        let releasedFailure = ref false

        let failure: Effect<string, string, unit> =
            Scope.scoped (fun scope ->
                effect {
                    do! Scope.addFinalizer scope (Effect.sync (fun () -> releasedFailure.Value <- true))
                    return! Effect.fail "boom"
                })

        toBe (run failure = Exit.fail "boom") true
        toBe releasedFailure.Value true))
