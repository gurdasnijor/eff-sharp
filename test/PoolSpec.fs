module PoolSpec

open Effect
open Effect.Vitest

let private run (eff: Effect<'A, obj, unit>) : 'A =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure cause -> failwithf "unexpected failure: %s" (Cause.render cause)

let private counting (count: Ref<int>) (itemScope: Scope<obj, unit>) : Effect<int, obj, unit> =
    Scope.acquireRelease itemScope (Ref.updateAndGet count (fun n -> n + 1)) (fun _ _ ->
        Ref.update count (fun n -> n - 1))

describe "Pool" (fun () ->
    test "preallocates pool items" (fun () ->
        let result =
            run (
                effect {
                    let! count = Ref.make 0
                    let poolScope: Scope<obj, unit> = Scope.make ()
                    let! _ = Pool.make poolScope (counting count) 10
                    return! Ref.get count
                }
            )

        toBe result 10)

    test "acquire one item" (fun () ->
        let item =
            run (
                effect {
                    let! count = Ref.make 0
                    let poolScope: Scope<obj, unit> = Scope.make ()
                    let! pool = Pool.make poolScope (counting count) 10
                    return! Pool.getScoped pool
                }
            )

        toBe item 1)

    test "cleans up items when shut down" (fun () ->
        let result =
            run (
                effect {
                    let! count = Ref.make 0
                    let poolScope: Scope<obj, unit> = Scope.make ()
                    let! _ = Pool.make poolScope (counting count) 10
                    do! Scope.close poolScope (Success(box ()))
                    return! Ref.get count
                }
            )

        toBe result 0)

    test "defects in finalizers do not prevent cleanup" (fun () ->
        let dying (count: Ref<int>) (itemScope: Scope<obj, unit>) : Effect<int, obj, unit> =
            Scope.acquireRelease itemScope (Ref.updateAndGet count (fun n -> n + 1)) (fun _ _ ->
                Ref.update count (fun n -> n - 1)
                |> Effect.flatMap (fun () -> Effect.sync (fun () -> failwith "boom")))

        let result =
            run (
                effect {
                    let! count = Ref.make 0
                    let poolScope: Scope<obj, unit> = Scope.make ()
                    let! _ = Pool.make poolScope (dying count) 10
                    do! Scope.close poolScope (Success(box ()))
                    return! Ref.get count
                }
            )

        toBe result 0)

    test "invalidate item" (fun () ->
        let result, value =
            run (
                effect {
                    let! count = Ref.make 0
                    let poolScope: Scope<obj, unit> = Scope.make ()
                    let! pool = Pool.make poolScope (counting count) 10
                    do! Pool.invalidate pool 1
                    let! result = Pool.getScoped pool
                    let! value = Ref.get count
                    return (result, value)
                }
            )

        toBe result 2
        toBe value 10)

    test "failed allocations are finalized and reported through get" (fun () ->
        let allocations, released =
            run (
                effect {
                    let! allocationsRef = Ref.make 0
                    let! releasedRef = Ref.make 0

                    let acquire (itemScope: Scope<obj, unit>) : Effect<int, obj, unit> =
                        Scope.acquireRelease itemScope (Ref.updateAndGet allocationsRef (fun n -> n + 1)) (fun _ _ ->
                            Ref.update releasedRef (fun n -> n + 1))
                        |> Effect.flatMap (fun _ -> Effect.fail (box "boom"))

                    let poolScope: Scope<obj, unit> = Scope.make ()
                    let! pool = Pool.make poolScope acquire 10

                    do!
                        Pool.getScoped pool
                        |> Effect.catchAll (fun _ -> Effect.succeed 0)
                        |> Effect.map ignore

                    let! a = Ref.get allocationsRef
                    let! r = Ref.get releasedRef
                    return (a, r)
                }
            )

        toBe allocations 10
        toBe released 10)

    test "reports failures via get" (fun () ->
        let values =
            run (
                effect {
                    let! count = Ref.make 0

                    let acquire (_: Scope<obj, unit>) : Effect<int, obj, unit> =
                        Ref.updateAndGet count (fun n -> n + 1)
                        |> Effect.flatMap (fun n -> Effect.fail (box n))

                    let poolScope: Scope<obj, unit> = Scope.make ()
                    let! pool = Pool.make poolScope acquire 10

                    return!
                        Effect.forEach (Seq.replicate 9 ()) (fun () ->
                            Pool.getScoped pool
                            |> Effect.map (fun _ -> -1)
                            |> Effect.catchAll (fun e -> Effect.succeed (unbox<int> e)))
                }
            )

        toBe (values = [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ]) true))
