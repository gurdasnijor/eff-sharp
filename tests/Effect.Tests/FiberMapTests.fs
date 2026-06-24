module Effect.Tests.FiberMapTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/FiberMap.test.ts, adapted
// to the port's explicit-`Scope` model and cooperative interruption (see
// FiberMap.fs).

let private voidExit: Exit<obj, obj> = Success(box ())

let private run (eff: Effect<'A, 'E, unit>) : 'A =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure c -> failwithf "unexpected failure: %s" (Cause.render c)

let private blocker (onStop: unit -> unit) : Effect<unit, string, unit> =
    Effect.sleep 1_000_000 |> Effect.ensuring (Effect.sync onStop)

[<Fact>]
let ``run at an existing key interrupts the previous fiber`` () =
    let interruptions = ref 0

    let program: Effect<int * bool * int, string, unit> =
        Scope.scoped (fun parent ->
            effect {
                let! map = FiberMap.make parent
                let! _ = FiberMap.run map "a" (blocker (fun () -> incr interruptions))
                let! _ = FiberMap.run map "a" (blocker (fun () -> incr interruptions))
                let! present = FiberMap.contains map "a"
                let! size = FiberMap.size map
                return interruptions.Value, present, size
            })

    let count, present, size = run program
    Assert.Equal(1, count)
    Assert.True(present)
    Assert.Equal(1, size)

[<Fact>]
let ``different keys coexist`` () =
    let program: Effect<int, string, unit> =
        Scope.scoped (fun parent ->
            effect {
                let! map = FiberMap.make parent
                let! _ = FiberMap.run map "a" (blocker ignore)
                let! _ = FiberMap.run map "b" (blocker ignore)
                return! FiberMap.size map
            })

    Assert.Equal(2, run program)

[<Fact>]
let ``remove interrupts and removes the keyed fiber`` () =
    let interruptions = ref 0

    let program: Effect<bool, string, unit> =
        Scope.scoped (fun parent ->
            effect {
                let! map = FiberMap.make parent
                let! _ = FiberMap.run map "a" (blocker (fun () -> incr interruptions))
                do! FiberMap.remove map "a"
                return! FiberMap.contains map "a"
            })

    let present = run program
    Assert.Equal(1, interruptions.Value)
    Assert.False(present)

[<Fact>]
let ``scope close interrupts all members`` () =
    let interruptions = ref 0

    let program: Effect<unit, string, unit> =
        Scope.scoped (fun parent ->
            effect {
                let! map = FiberMap.make parent
                let! _ = FiberMap.run map "a" (blocker (fun () -> incr interruptions))
                let! _ = FiberMap.run map "b" (blocker (fun () -> incr interruptions))
                return ()
            })

    run program
    Assert.Equal(2, interruptions.Value)

[<Fact>]
let ``size drops to zero after the scope closes`` () =
    let program: Effect<int * int, string, unit> =
        effect {
            let scope = Scope.make<string, unit> ()
            let! map = FiberMap.make scope
            let! _ = FiberMap.run map "a" (blocker ignore)
            let! _ = FiberMap.run map "b" (blocker ignore)
            let! before = FiberMap.size map
            do! Scope.close scope voidExit
            let! after = FiberMap.size map
            return before, after
        }

    let before, after = run program
    Assert.Equal(2, before)
    Assert.Equal(0, after)

[<Fact>]
let ``join surfaces the first failure`` () =
    let program: Effect<unit, string, unit> =
        Scope.scoped (fun parent ->
            effect {
                let! map = FiberMap.make parent
                let! _ = FiberMap.run map "ok" (Effect.succeed ())
                let! f = FiberMap.run map "bad" (Effect.fail "fail")
                let! _ = Effect.await f
                return! FiberMap.join map
            })

    match Effect.runSync () program with
    | Failure c -> Assert.Equal<string list>([ "fail" ], Cause.failures c)
    | Success _ -> Assert.Fail "expected failure"
