module Effect.Tests.TxSubscriptionRefTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/TxSubscriptionRef.test.ts
// (subset). Subscribers take from a transactional queue, so the changes tests are
// deterministic without forking. `isTxSubscriptionRef` omitted (JS-runtime guard).

let private run (eff: Effect<'a, 'e, unit>) : 'a =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure cause -> failwithf "unexpected failure: %s" (Cause.render cause)

[<Fact>]
let ``get returns the initial value; set/update/modify change it`` () =
    let initial, afterSet, afterUpdate, modified, final =
        run (
            effect {
                let! ref = TxSubscriptionRef.make 0
                let! a = TxSubscriptionRef.get ref
                do! TxSubscriptionRef.set ref 10
                let! b = TxSubscriptionRef.get ref
                do! TxSubscriptionRef.update ref (fun n -> n + 5)
                let! c = TxSubscriptionRef.get ref
                let! m = TxSubscriptionRef.modify ref (fun n -> (sprintf "was %d" n, n * 2))
                let! d = TxSubscriptionRef.get ref
                return (a, b, c, m, d)
            }
        )

    Assert.Equal(0, initial)
    Assert.Equal(10, afterSet)
    Assert.Equal(15, afterUpdate)
    Assert.Equal("was 15", modified)
    Assert.Equal(30, final)

[<Fact>]
let ``changes yields the current value then subsequent updates`` () =
    let v0, v1, v2 =
        run (
            effect {
                let! ref = TxSubscriptionRef.make 0
                let! sub = TxSubscriptionRef.changes ref
                let! initial = TxQueue.take sub // 0
                do! TxSubscriptionRef.set ref 1
                let! a = TxQueue.take sub // 1
                do! TxSubscriptionRef.set ref 2
                let! b = TxQueue.take sub // 2
                return (initial, a, b)
            }
        )

    Assert.Equal(0, v0)
    Assert.Equal(1, v1)
    Assert.Equal(2, v2)

[<Fact>]
let ``two subscribers both receive a broadcast update`` () =
    let s1a, s1b, s2 =
        run (
            effect {
                let! ref = TxSubscriptionRef.make 0
                let! sub1 = TxSubscriptionRef.changes ref
                let! _initial1 = TxQueue.take sub1 // drain sub1's seeded 0
                do! TxSubscriptionRef.set ref 1
                let! sub2 = TxSubscriptionRef.changes ref // seeded with current 1
                do! TxSubscriptionRef.set ref 2
                let! a = TxQueue.take sub1 // 1
                let! b = TxQueue.take sub1 // 2
                let! c = TxQueue.take sub2 // 1 (seed), then 2 would follow
                return (a, b, c)
            }
        )

    Assert.Equal(1, s1a)
    Assert.Equal(2, s1b)
    Assert.Equal(1, s2)

[<Fact>]
let ``changesStream collects the current value`` () =
    let values =
        run (
            effect {
                let! ref = TxSubscriptionRef.make 0
                do! TxSubscriptionRef.set ref 1
                do! TxSubscriptionRef.set ref 2
                return! Stream.runCollect (Stream.take 1 (TxSubscriptionRef.changesStream ref))
            }
        )

    Assert.Equal<int list>([ 2 ], values)
