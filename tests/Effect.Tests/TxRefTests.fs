module Effect.Tests.TxRefTests

open System.Threading
open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/src/TxRef.ts (no upstream
// TxRef.test.ts exists; the algorithm's properties are also proven by the
// spike at spikes/stm). These Facts assert the three STM guarantees the brief
// requires: atomicity (all-or-nothing), conflict -> retry -> no lost updates,
// and retry-blocks-until-write.

/// Run an effect that cannot fail (env = unit), returning its success value.
let private run (eff: Effect<'a, 'e, unit>) : 'a =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure cause -> failwithf "unexpected failure: %s" (Cause.render cause)

[<Fact>]
let ``single-tx increment`` () =
    let counter = TxRef.makeUnsafe 0
    run (TxRef.atomically (TxRef.update counter (fun n -> n + 1)))
    Assert.Equal(1, TxRef.getUnsafe counter)

[<Fact>]
let ``modify returns a result and stores the new value`` () =
    let counter = TxRef.makeUnsafe 10

    let result =
        run (TxRef.atomically (TxRef.modify counter (fun n -> (sprintf "was %d" n, n * 2))))

    Assert.Equal("was 10", result)
    Assert.Equal(20, TxRef.getUnsafe counter)

[<Fact>]
let ``make creates a TxRef inside an Effect`` () =
    let value =
        run (
            effect {
                let! ref = TxRef.make 99
                return! TxRef.atomically (TxRef.get ref)
            }
        )

    Assert.Equal(99, value)

[<Fact>]
let ``all-or-nothing: a multi-write commit applies every write`` () =
    let a = TxRef.makeUnsafe 0
    let b = TxRef.makeUnsafe 0

    run (
        TxRef.atomically (
            stm {
                do! TxRef.set a 1
                do! TxRef.set b 2
            }
        )
    )

    Assert.Equal(1, TxRef.getUnsafe a)
    Assert.Equal(2, TxRef.getUnsafe b)

[<Fact>]
let ``all-or-nothing: an aborted (retried) branch leaves no partial writes`` () =
    let a = TxRef.makeUnsafe 100
    let b = TxRef.makeUnsafe 200

    // Left branch writes both then retries; orElse rolls back and runs the
    // no-op right branch. Neither write may survive.
    run (
        TxRef.atomically (
            TxRef.orElse
                (stm {
                    do! TxRef.set a 999
                    do! TxRef.set b 888
                    return! TxRef.retry
                })
                (stm { return () })
        )
    )

    Assert.Equal(100, TxRef.getUnsafe a)
    Assert.Equal(200, TxRef.getUnsafe b)

[<Fact>]
let ``conflict -> retry -> no lost updates under concurrency`` () =
    let counter = TxRef.makeUnsafe 0
    let threads = 8
    let perThread = 5000

    let work () =
        for _ in 1..perThread do
            run (TxRef.atomically (TxRef.update counter (fun n -> n + 1)))

    let ts = Array.init threads (fun _ -> Thread(ThreadStart(work)))
    ts |> Array.iter (fun t -> t.Start())
    ts |> Array.iter (fun t -> t.Join())

    Assert.Equal(threads * perThread, TxRef.getUnsafe counter)

[<Fact>]
let ``retry blocks until another transaction writes the awaited TxRef`` () =
    let flag = TxRef.makeUnsafe false
    let observed = TxRef.makeUnsafe 0

    let waiter =
        Thread(
            ThreadStart(fun () ->
                run (
                    TxRef.atomically (
                        stm {
                            let! ready = TxRef.get flag

                            if not ready then
                                return! TxRef.retry
                            else
                                do! TxRef.set observed 42
                        }
                    )
                ))
        )

    waiter.Start()
    Thread.Sleep 250
    let beforeWrite = TxRef.getUnsafe observed // waiter must still be blocked
    run (TxRef.atomically (TxRef.set flag true))
    let joined = waiter.Join 5000

    Assert.Equal(0, beforeWrite)
    Assert.True(joined)
    Assert.Equal(42, TxRef.getUnsafe observed)

[<Fact>]
let ``orElse takes the second branch when the first retries`` () =
    let a = TxRef.makeUnsafe 0

    let chosen =
        run (TxRef.atomically (TxRef.orElse (stm { return! TxRef.retry }) (stm { return 7 })))

    Assert.Equal(7, chosen)
    Assert.Equal(0, TxRef.getUnsafe a)
