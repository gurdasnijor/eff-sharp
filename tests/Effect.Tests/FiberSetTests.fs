module Effect.Tests.FiberSetTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/FiberSet.test.ts, adapted
// to the port's explicit-`Scope` model and cooperative interruption (see
// FiberSet.fs).

let private voidExit: Exit<obj, obj> = Success(box ())

let private run (eff: Effect<'A, 'E, unit>) : 'A =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure c -> failwithf "unexpected failure: %s" (Cause.render c)

let private blocker (onStop: unit -> unit) : Effect<unit, string, unit> =
    Effect.sleep 1_000_000 |> Effect.ensuring (Effect.sync onStop)

[<Fact>]
let ``scope close interrupts all members`` () =
    let interruptions = ref 0

    let program: Effect<unit, string, unit> =
        Scope.scoped (fun parent ->
            effect {
                let! set = FiberSet.make parent
                let! _ = FiberSet.run set (blocker (fun () -> incr interruptions))
                let! _ = FiberSet.run set (blocker (fun () -> incr interruptions))
                let! _ = FiberSet.run set (blocker (fun () -> incr interruptions))
                return ()
            })

    run program
    Assert.Equal(3, interruptions.Value)

[<Fact>]
let ``size reflects live fibers and drops to zero after close`` () =
    let program: Effect<int * int, string, unit> =
        effect {
            let scope = Scope.make<string, unit> ()
            let! set = FiberSet.make scope
            let! _ = FiberSet.run set (blocker ignore)
            let! _ = FiberSet.run set (blocker ignore)
            let! before = FiberSet.size set
            do! Scope.close scope voidExit
            let! after = FiberSet.size set
            return before, after
        }

    let before, after = run program
    Assert.Equal(2, before)
    Assert.Equal(0, after)

[<Fact>]
let ``clear interrupts all members and empties the set`` () =
    let interruptions = ref 0

    let program: Effect<int, string, unit> =
        Scope.scoped (fun parent ->
            effect {
                let! set = FiberSet.make parent
                let! _ = FiberSet.run set (blocker (fun () -> incr interruptions))
                let! _ = FiberSet.run set (blocker (fun () -> incr interruptions))
                do! FiberSet.clear set
                return! FiberSet.size set
            })

    let remaining = run program
    Assert.Equal(2, interruptions.Value)
    Assert.Equal(0, remaining)

[<Fact>]
let ``join surfaces the first failure`` () =
    let program: Effect<unit, string, unit> =
        Scope.scoped (fun parent ->
            effect {
                let! set = FiberSet.make parent
                let! _ = FiberSet.run set (Effect.succeed ())
                let! _ = FiberSet.run set (Effect.succeed ())
                let! f = FiberSet.run set (Effect.fail "fail")
                let! _ = Effect.await f
                return! FiberSet.join set
            })

    match Effect.runSync () program with
    | Failure c -> Assert.Equal<string list>([ "fail" ], Cause.failures c)
    | Success _ -> Assert.Fail "expected failure"

[<Fact>]
let ``addUnsafe stores a forked fiber`` () =
    let program: Effect<int, string, unit> =
        Scope.scoped (fun parent ->
            effect {
                let! set = FiberSet.make parent
                let! fiber = Effect.fork (blocker ignore)
                do! Effect.sync (fun () -> FiberSet.addUnsafe set fiber)
                return! FiberSet.size set
            })

    Assert.Equal(1, run program)
