module Effect.Tests.TxHashMapTests

open System.Threading
open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/TxHashMap.test.ts (subset)
// plus an STM atomicity check.

let private run (eff: Effect<'a, 'e, unit>) : 'a =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure cause -> failwithf "unexpected failure: %s" (Cause.render cause)

[<Fact>]
let ``set then get within a transaction`` () =
    let m: TxHashMap<string, int> = run (TxHashMap.empty ())

    let value =
        run (
            TxRef.atomically (
                stm {
                    do! TxHashMap.set m "a" 1
                    return! TxHashMap.get m "a"
                })
        )

    Assert.Equal(Some 1, value)

[<Fact>]
let ``has and remove`` () =
    let m = run (TxHashMap.fromIterable [ ("x", 10); ("y", 20) ])
    Assert.True(run (TxRef.atomically (TxHashMap.has m "x")))
    run (TxRef.atomically (TxHashMap.remove m "x"))
    Assert.False(run (TxRef.atomically (TxHashMap.has m "x")))
    Assert.Equal(1, run (TxRef.atomically (TxHashMap.size m)))

[<Fact>]
let ``multi-key writes commit atomically`` () =
    let m: TxHashMap<string, int> = run (TxHashMap.empty ())

    run (
        TxRef.atomically (
            stm {
                do! TxHashMap.set m "a" 1
                do! TxHashMap.set m "b" 2
            })
    )

    Assert.Equal(Some 1, run (TxRef.atomically (TxHashMap.get m "a")))
    Assert.Equal(Some 2, run (TxRef.atomically (TxHashMap.get m "b")))

[<Fact>]
let ``concurrent inserts never lose a key`` () =
    let m: TxHashMap<int, int> = run (TxHashMap.empty ())
    let threads = 6
    let perThread = 300

    let work (t: int) =
        for i in 0 .. perThread - 1 do
            let key = t * perThread + i
            run (TxRef.atomically (TxHashMap.set m key key))

    let ts = Array.init threads (fun t -> Thread(ThreadStart(fun () -> work t)))
    ts |> Array.iter (fun t -> t.Start())
    ts |> Array.iter (fun t -> t.Join())

    Assert.Equal(threads * perThread, run (TxRef.atomically (TxHashMap.size m)))
