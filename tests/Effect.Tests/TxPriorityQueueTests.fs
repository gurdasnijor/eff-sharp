module Effect.Tests.TxPriorityQueueTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/TxPriorityQueue.test.ts
// (and the doc-comment examples in src/TxPriorityQueue.ts). Each op is its own
// transaction in this port, so tests drive them directly with `runSync`.

let private run (eff: Effect<'a, 'e, unit>) : 'a =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure cause -> failwithf "unexpected failure: %s" (Cause.render cause)

/// Ascending integer order (`Order<int>` is just `compare`).
let private intOrder: Order<int> = compare

[<Fact>]
let ``empty queue is empty`` () =
    let pq = run (TxPriorityQueue.empty intOrder)
    Assert.True(run (TxPriorityQueue.isEmpty pq))
    Assert.Equal(0, run (TxPriorityQueue.size pq))

[<Fact>]
let ``offer then take returns smallest first`` () =
    let pq = run (TxPriorityQueue.empty intOrder)
    run (TxPriorityQueue.offer pq 3)
    run (TxPriorityQueue.offer pq 1)
    run (TxPriorityQueue.offer pq 2)
    Assert.Equal(1, run (TxPriorityQueue.take pq))
    Assert.Equal(2, run (TxPriorityQueue.take pq))
    Assert.Equal(3, run (TxPriorityQueue.take pq))

[<Fact>]
let ``fromIterable sorts and take yields smallest`` () =
    let pq = run (TxPriorityQueue.fromIterable intOrder [ 3; 1; 2 ])
    Assert.Equal(1, run (TxPriorityQueue.take pq))

[<Fact>]
let ``size reflects element count`` () =
    let pq = run (TxPriorityQueue.fromIterable intOrder [ 1; 2; 3 ])
    Assert.Equal(3, run (TxPriorityQueue.size pq))
    Assert.True(run (TxPriorityQueue.isNonEmpty pq))

[<Fact>]
let ``peek observes smallest without removing`` () =
    let pq = run (TxPriorityQueue.fromIterable intOrder [ 3; 1; 2 ])
    Assert.Equal(1, run (TxPriorityQueue.peek pq))
    Assert.Equal(3, run (TxPriorityQueue.size pq))

[<Fact>]
let ``peekOption is None on empty, Some on non-empty`` () =
    let pq = run (TxPriorityQueue.empty intOrder)
    Assert.Equal(None, run (TxPriorityQueue.peekOption pq))
    run (TxPriorityQueue.offer pq 5)
    Assert.Equal(Some 5, run (TxPriorityQueue.peekOption pq))

[<Fact>]
let ``offerAll inserts all in order`` () =
    let pq = run (TxPriorityQueue.empty intOrder)
    run (TxPriorityQueue.offerAll pq [ 3; 1; 2 ])
    Assert.Equal(1, run (TxPriorityQueue.take pq))

[<Fact>]
let ``takeAll returns all in priority order`` () =
    let pq = run (TxPriorityQueue.fromIterable intOrder [ 3; 1; 2 ])
    Assert.Equal<int list>([ 1; 2; 3 ], run (TxPriorityQueue.takeAll pq))
    Assert.True(run (TxPriorityQueue.isEmpty pq))

[<Fact>]
let ``takeOption is None on empty`` () =
    let pq = run (TxPriorityQueue.empty intOrder)
    Assert.Equal(None, run (TxPriorityQueue.takeOption pq))

[<Fact>]
let ``takeUpTo limits the number of elements taken`` () =
    let pq = run (TxPriorityQueue.fromIterable intOrder [ 5; 3; 1; 4; 2 ])
    Assert.Equal<int list>([ 1; 2 ], run (TxPriorityQueue.takeUpTo pq 2))
    Assert.Equal(3, run (TxPriorityQueue.size pq))

[<Fact>]
let ``removeIf drops matching elements`` () =
    let pq = run (TxPriorityQueue.fromIterable intOrder [ 1; 2; 3; 4; 5 ])
    run (TxPriorityQueue.removeIf pq (fun n -> n % 2 = 0))
    Assert.Equal<int list>([ 1; 3; 5 ], run (TxPriorityQueue.takeAll pq))

[<Fact>]
let ``retainIf keeps only matching elements`` () =
    let pq = run (TxPriorityQueue.fromIterable intOrder [ 1; 2; 3; 4; 5 ])
    run (TxPriorityQueue.retainIf pq (fun n -> n % 2 = 0))
    Assert.Equal<int list>([ 2; 4 ], run (TxPriorityQueue.takeAll pq))

[<Fact>]
let ``toList reads all without removing`` () =
    let pq = run (TxPriorityQueue.fromIterable intOrder [ 3; 1; 2 ])
    Assert.Equal<int list>([ 1; 2; 3 ], run (TxPriorityQueue.toList pq))
    Assert.Equal(3, run (TxPriorityQueue.size pq))

[<Fact>]
let ``take retries on empty then resumes when another thread offers`` () =
    let pq = run (TxPriorityQueue.empty intOrder)

    // Fork a waiter that blocks on the empty queue.
    let waiter = System.Threading.Tasks.Task.Run(fun () -> run (TxPriorityQueue.take pq))
    Assert.False(waiter.Wait 100)

    // Offer unblocks the waiter, which takes the value.
    run (TxPriorityQueue.offer pq 42)
    Assert.True(waiter.Wait 2000)
    Assert.Equal(42, waiter.Result)
