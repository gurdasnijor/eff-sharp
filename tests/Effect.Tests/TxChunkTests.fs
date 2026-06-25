module Effect.Tests.TxChunkTests

open System.Threading
open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/TxChunk.test.ts (subset)
// plus an STM atomicity check on the transactional chunk.

let private run (eff: Effect<'a, 'e, unit>) : 'a =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure cause -> failwithf "unexpected failure: %s" (Cause.render cause)

[<Fact>]
let ``get returns the initial chunk`` () =
    let tx = run (TxChunk.fromIterable [ 1; 2; 3 ])
    let chunk = run (TxRef.atomically (TxChunk.get tx))
    Assert.Equal<int[]>([| 1; 2; 3 |], Chunk.toArray chunk)

[<Fact>]
let ``append and prepend within one transaction commit together`` () =
    let tx = run (TxChunk.fromIterable [ 1; 2; 3 ])

    run (
        TxRef.atomically (
            stm {
                do! TxChunk.prepend tx 0
                do! TxChunk.append tx 4
            }
        )
    )

    let chunk = run (TxRef.atomically (TxChunk.get tx))
    Assert.Equal<int[]>([| 0; 1; 2; 3; 4 |], Chunk.toArray chunk)

[<Fact>]
let ``size reflects committed appends`` () =
    let tx = run (TxChunk.fromIterable [ 10; 20 ])
    run (TxRef.atomically (TxChunk.append tx 30))
    Assert.Equal(3, run (TxRef.atomically (TxChunk.size tx)))

[<Fact>]
let ``concurrent appends never lose an element`` () =
    let tx = run (TxChunk.make Chunk.empty)
    let threads = 6
    let perThread = 500

    let work () =
        for i in 1..perThread do
            run (TxRef.atomically (TxChunk.append tx i))

    let ts = Array.init threads (fun _ -> Thread(ThreadStart(work)))
    ts |> Array.iter (fun t -> t.Start())
    ts |> Array.iter (fun t -> t.Join())

    Assert.Equal(threads * perThread, run (TxRef.atomically (TxChunk.size tx)))
