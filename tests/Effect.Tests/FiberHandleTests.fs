module Effect.Tests.FiberHandleTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/FiberHandle.test.ts, adapted
// to the port's explicit-`Scope` model and cooperative interruption (see
// FiberHandle.fs). A "blocker" sleeps until interrupted and records the
// interruption via an `ensuring` finalizer.

let private run (eff: Effect<'A, 'E, unit>) : 'A =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure c -> failwithf "unexpected failure: %s" (Cause.render c)

/// Runs forever until interrupted; `onStop` runs when it stops.
let private blocker (onStop: unit -> unit) : Effect<unit, string, unit> =
    Effect.sleep 1_000_000 |> Effect.ensuring (Effect.sync onStop)

[<Fact>]
let ``run interrupts the previous fiber`` () =
    let interruptions = ref 0

    let program: Effect<int * bool, string, unit> =
        Scope.scoped (fun parent ->
            effect {
                let! handle = FiberHandle.make parent
                let! _ = FiberHandle.run handle (blocker (fun () -> incr interruptions))
                let! _ = FiberHandle.run handle (blocker (fun () -> incr interruptions))
                return interruptions.Value, (FiberHandle.getUnsafe handle).IsSome
            })

    let count, hasCurrent = run program
    Assert.Equal(1, count)
    Assert.True(hasCurrent)

[<Fact>]
let ``clear interrupts the current fiber and empties the handle`` () =
    let interruptions = ref 0

    let program: Effect<bool, string, unit> =
        Scope.scoped (fun parent ->
            effect {
                let! handle = FiberHandle.make parent
                let! _ = FiberHandle.run handle (blocker (fun () -> incr interruptions))
                do! FiberHandle.clear handle
                return (FiberHandle.getUnsafe handle).IsSome
            })

    let hasCurrent = run program
    Assert.Equal(1, interruptions.Value)
    Assert.False(hasCurrent)

[<Fact>]
let ``scope close interrupts the current fiber`` () =
    let interruptions = ref 0

    let program: Effect<unit, string, unit> =
        Scope.scoped (fun parent ->
            effect {
                let! handle = FiberHandle.make parent
                let! _ = FiberHandle.run handle (blocker (fun () -> incr interruptions))
                return ()
            })

    run program
    Assert.Equal(1, interruptions.Value)

[<Fact>]
let ``onlyIfMissing keeps the current fiber and interrupts the rejected start`` () =
    let interruptions = ref 0

    let program: Effect<int * bool, string, unit> =
        Scope.scoped (fun parent ->
            effect {
                let! handle = FiberHandle.make parent
                let! _ = FiberHandle.run handle (blocker (fun () -> incr interruptions))
                let! _ = FiberHandle.runWith handle true (blocker (fun () -> incr interruptions))
                let! insideCount = Effect.sync (fun () -> interruptions.Value)
                return insideCount, (FiberHandle.getUnsafe handle).IsSome
            })

    let insideCount, hasCurrent = run program
    // only the rejected (second) start was interrupted; the first is still live
    Assert.Equal(1, insideCount)
    Assert.True(hasCurrent)
    // the kept fiber is interrupted later, when the scope closes
    Assert.Equal(2, interruptions.Value)

[<Fact>]
let ``get returns None initially and Some after run`` () =
    let program: Effect<bool * bool, string, unit> =
        Scope.scoped (fun parent ->
            effect {
                let! handle = FiberHandle.make parent
                let! before = FiberHandle.get handle
                let! _ = FiberHandle.run handle (blocker ignore)
                let! after = FiberHandle.get handle
                return before.IsSome, after.IsSome
            })

    let before, after = run program
    Assert.False(before)
    Assert.True(after)

[<Fact>]
let ``awaitEmpty waits for the current fiber to complete`` () =
    let program: Effect<bool, string, unit> =
        Scope.scoped (fun parent ->
            effect {
                let! handle = FiberHandle.make parent
                let! _ = FiberHandle.run handle (Effect.sleep 30)
                do! FiberHandle.awaitEmpty handle
                return (FiberHandle.getUnsafe handle).IsSome
            })

    Assert.False(run program)

[<Fact>]
let ``join surfaces the child failure`` () =
    let program: Effect<unit, string, unit> =
        Scope.scoped (fun parent ->
            effect {
                let! handle = FiberHandle.make parent
                let! fiber = FiberHandle.run handle (Effect.fail "fail")
                let! _ = Effect.await fiber // ensure it has completed
                return! FiberHandle.join handle
            })

    match Effect.runSync () program with
    | Failure c -> Assert.Equal<string list>([ "fail" ], Cause.failures c)
    | Success _ -> Assert.Fail "expected failure"
