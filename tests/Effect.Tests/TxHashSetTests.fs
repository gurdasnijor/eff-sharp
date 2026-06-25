module Effect.Tests.TxHashSetTests

open System.Threading
open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/TxHashSet.test.ts (subset)
// plus an STM atomicity check.

let private run (eff: Effect<'a, 'e, unit>) : 'a =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure cause -> failwithf "unexpected failure: %s" (Cause.render cause)

[<Fact>]
let ``add then has within a transaction`` () =
    let s: TxHashSet<int> = run (TxHashSet.empty ())

    let present =
        run (
            TxRef.atomically (
                stm {
                    do! TxHashSet.add s 1
                    return! TxHashSet.has s 1
                }
            )
        )

    Assert.True(present)

[<Fact>]
let ``remove and size`` () =
    let s = run (TxHashSet.fromIterable [ 1; 2; 3 ])
    run (TxRef.atomically (TxHashSet.remove s 2))
    Assert.False(run (TxRef.atomically (TxHashSet.has s 2)))
    Assert.Equal(2, run (TxRef.atomically (TxHashSet.size s)))

[<Fact>]
let ``duplicate adds do not grow the set`` () =
    let s: TxHashSet<int> = run (TxHashSet.empty ())

    run (
        TxRef.atomically (
            stm {
                do! TxHashSet.add s 5
                do! TxHashSet.add s 5
            }
        )
    )

    Assert.Equal(1, run (TxRef.atomically (TxHashSet.size s)))

[<Fact>]
let ``concurrent adds of distinct values never lose one`` () =
    let s: TxHashSet<int> = run (TxHashSet.empty ())
    let threads = 6
    let perThread = 300

    let work (t: int) =
        for i in 0 .. perThread - 1 do
            run (TxRef.atomically (TxHashSet.add s (t * perThread + i)))

    let ts = Array.init threads (fun t -> Thread(ThreadStart(fun () -> work t)))
    ts |> Array.iter (fun t -> t.Start())
    ts |> Array.iter (fun t -> t.Join())

    Assert.Equal(threads * perThread, run (TxRef.atomically (TxHashSet.size s)))
