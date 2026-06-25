module Effect.Tests.PoolTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/Pool.test.ts.
//
// The upstream suite uses Effect.gen + TestClock + helpers this port lacks
// (forkChild / replicateEffect / Schedule.retry / Effect.scoped-in-R). Here each
// case is an `effect { }` program run with `Effect.runSync`. Scopes are explicit
// values (the port threads `Scope` as an argument, not through `R`), and the
// pool error channel is `obj` (like the merged Queue/Pull it is built on).
//
// Covered: the fixed-size `make` core — preallocation, single acquire, reuse,
// release-on-shutdown (defects included), blocking when exhausted,
// invalidate-and-replace, and failed-allocation cleanup / failure reporting.
// The elastic `makeWithTTL`/strategy surface is deferred (see Pool.fs).

let private run (eff: Effect<'A, obj, unit>) : 'A =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure c -> failwithf "unexpected failure: %s" (Cause.render c)

/// `acquireRelease` that bumps `count` up on acquire and down on release.
let private counting (count: Ref<int>) (itemScope: Scope<obj, unit>) : Effect<int, obj, unit> =
    Scope.acquireRelease itemScope (Ref.updateAndGet count (fun n -> n + 1)) (fun _ _ -> Ref.update count (fun n -> n - 1))

[<Fact>]
let ``preallocates pool items`` () =
    let result =
        run (
            effect {
                let! count = Ref.make 0
                let poolScope: Scope<obj, unit> = Scope.make ()
                let! _ = Pool.make poolScope (counting count) 10
                return! Ref.get count
            }
        )

    Assert.Equal(10, result)

[<Fact>]
let ``acquire one item`` () =
    let item =
        run (
            effect {
                let! count = Ref.make 0
                let poolScope: Scope<obj, unit> = Scope.make ()
                let! pool = Pool.make poolScope (counting count) 10
                return! Pool.getScoped pool
            }
        )

    Assert.Equal(1, item)

[<Fact>]
let ``reuse released items`` () =
    let result =
        run (
            effect {
                let! count = Ref.make 0
                let poolScope: Scope<obj, unit> = Scope.make ()
                let! pool = Pool.make poolScope (counting count) 10
                do! Effect.forEach (Seq.replicate 99 ()) (fun () -> Pool.getScoped pool |> Effect.map ignore) |> Effect.map ignore
                return! Ref.get count
            }
        )

    Assert.Equal(10, result)

[<Fact>]
let ``cleans up items when shut down`` () =
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

    Assert.Equal(0, result)

[<Fact>]
let ``defects in finalizers don't prevent cleanup`` () =
    let dying (count: Ref<int>) (itemScope: Scope<obj, unit>) : Effect<int, obj, unit> =
        Scope.acquireRelease
            itemScope
            (Ref.updateAndGet count (fun n -> n + 1))
            (fun _ _ -> Ref.update count (fun n -> n - 1) |> Effect.flatMap (fun () -> Effect.sync (fun () -> failwith "boom")))

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

    Assert.Equal(0, result)

[<Fact>]
let ``blocks when item not available`` () =
    let pending =
        run (
            effect {
                let! count = Ref.make 0
                let poolScope: Scope<obj, unit> = Scope.make ()
                let! pool = Pool.make poolScope (counting count) 10
                // borrow all 10 into a long-lived scope (never returned)
                let borrowScope: Scope<obj, unit> = Scope.make ()
                do! Effect.forEach (Seq.replicate 10 ()) (fun () -> Pool.get pool borrowScope |> Effect.map ignore) |> Effect.map ignore
                // an 11th get must block
                let! fib = Effect.fork (Pool.getScoped pool)
                do! Effect.sleep 50
                let! polled = Fiber.poll fib
                return Option.isNone polled
            }
        )

    Assert.True(pending)

[<Fact>]
let ``invalidate item`` () =
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

    Assert.Equal(2, result)
    Assert.Equal(10, value)

[<Fact>]
let ``invalidate all items and get does not hang`` () =
    let result, allocated, finalized =
        run (
            effect {
                let! allocatedRef = Ref.make 0
                let! finalizedRef = Ref.make 0

                let acquire (itemScope: Scope<obj, unit>) : Effect<int, obj, unit> =
                    Scope.acquireRelease
                        itemScope
                        (Ref.updateAndGet allocatedRef (fun n -> n + 1))
                        (fun _ _ -> Ref.update finalizedRef (fun n -> n + 1))

                let poolScope: Scope<obj, unit> = Scope.make ()
                let! pool = Pool.make poolScope acquire 2
                do! Pool.invalidate pool 1
                do! Pool.invalidate pool 2
                let! result = Pool.getScoped pool
                let! a = Ref.get allocatedRef
                let! f = Ref.get finalizedRef
                return (result, a, f)
            }
        )

    Assert.Equal(3, result)
    Assert.Equal(4, allocated)
    Assert.Equal(2, finalized)

[<Fact>]
let ``finalizer is called for failed allocations`` () =
    let allocations, released =
        run (
            effect {
                let! allocationsRef = Ref.make 0
                let! releasedRef = Ref.make 0

                let acquire (itemScope: Scope<obj, unit>) : Effect<int, obj, unit> =
                    Scope.acquireRelease
                        itemScope
                        (Ref.updateAndGet allocationsRef (fun n -> n + 1))
                        (fun _ _ -> Ref.update releasedRef (fun n -> n + 1))
                    |> Effect.flatMap (fun _ -> Effect.fail (box "boom"))

                let poolScope: Scope<obj, unit> = Scope.make ()
                let! pool = Pool.make poolScope acquire 10
                do! Pool.getScoped pool |> Effect.catchAll (fun _ -> Effect.succeed 0) |> Effect.map ignore
                let! a = Ref.get allocationsRef
                let! r = Ref.get releasedRef
                return (a, r)
            }
        )

    Assert.Equal(10, allocations)
    Assert.Equal(10, released)

[<Fact>]
let ``reports failures via get`` () =
    let values =
        run (
            effect {
                let! count = Ref.make 0

                let acquire (_: Scope<obj, unit>) : Effect<int, obj, unit> =
                    Ref.updateAndGet count (fun n -> n + 1) |> Effect.flatMap (fun n -> Effect.fail (box n))

                let poolScope: Scope<obj, unit> = Scope.make ()
                let! pool = Pool.make poolScope acquire 10

                return!
                    Effect.forEach (Seq.replicate 9 ()) (fun () ->
                        Pool.getScoped pool
                        |> Effect.map (fun _ -> -1)
                        |> Effect.catchAll (fun e -> Effect.succeed (unbox<int> e)))
            }
        )

    Assert.Equal<int list>([ 1; 2; 3; 4; 5; 6; 7; 8; 9 ], values)
