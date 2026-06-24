module Effect.Tests.FiberTests

open Xunit
open Effect

// Wave-3 kernel: cooperative fibers + interruption + uninterruptible masking.
// These exercise the hybrid (Proto2) model proven in spikes/fiber.

[<Fact>]
let ``fork then join returns the fiber's value`` () =
    let prog: Effect<int, string, unit> =
        effect {
            let! fib = Effect.fork (Effect.succeed 42)
            return! Effect.join fib
        }

    Assert.Equal<Exit<int, string>>(Exit.succeed 42, Effect.runSync () prog)

[<Fact>]
let ``fork then await observes the fiber's Exit`` () =
    let prog: Effect<Exit<int, string>, string, unit> =
        effect {
            let! fib = Effect.fork (Effect.fail "boom")
            return! Effect.await fib
        }

    Assert.Equal<Exit<Exit<int, string>, string>>(Exit.succeed (Exit.fail "boom"), Effect.runSync () prog)

[<Fact>]
let ``interrupt stops a sleeping fiber and runs its finalizer`` () =
    let log = System.Collections.Generic.List<string>()

    let prog: Effect<Exit<unit, string>, string, unit> =
        effect {
            let! fib =
                Effect.fork (Effect.ensuring (Effect.sync (fun () -> log.Add "released")) (Effect.sleep 5000))
            do! Effect.sleep 50 // let the child start sleeping
            do! Effect.interrupt fib
            return! Effect.await fib
        }

    match Effect.runSync () prog with
    | Success childExit ->
        Assert.True(Exit.isFailure childExit) // child was interrupted
        Assert.Equal<string>("released", System.String.Join(",", log)) // finalizer still ran
    | Failure c -> Assert.Fail(sprintf "outer failed: %s" (Cause.render c))

[<Fact>]
let ``uninterruptible region completes, interrupt honored after`` () =
    let log = System.Collections.Generic.List<string>()

    let masked: Effect<unit, string, unit> =
        Effect.uninterruptible (
            effect {
                do! Effect.sleep 100 // not interruptible while masked
                do! Effect.sync (fun () -> log.Add "done")
            }
        )

    let prog: Effect<Exit<unit, string>, string, unit> =
        effect {
            let! fib = Effect.fork masked
            do! Effect.sleep 30
            do! Effect.interrupt fib // arrives mid-mask; must be deferred
            return! Effect.await fib
        }

    match Effect.runSync () prog with
    | Success childExit ->
        Assert.Equal<string>("done", System.String.Join(",", log)) // masked region ran to completion
        Assert.True(Exit.isFailure childExit) // deferred interrupt honored when mask lifted
    | Failure c -> Assert.Fail(sprintf "outer failed: %s" (Cause.render c))
