module TxSpec

open Effect
open Effect.Vitest

let private same actual expected = toBe (actual = expected) true

let private ignored (eff: Effect<'a, 'e, 'r>) : Effect<unit, 'e, 'r> =
    eff |> Effect.map ignore

let private intOrder: Order<int> = fun a b -> sign (compare a b)

describe "TxRef" (fun () ->
    itEffect "commits multi-ref transactions atomically" (fun () ->
        effect {
            let a = TxRef.makeUnsafe 0
            let b = TxRef.makeUnsafe 0

            do!
                TxRef.atomically (
                    stm {
                        do! TxRef.set a 1
                        do! TxRef.set b 2
                    }
                )

            same (TxRef.getUnsafe a, TxRef.getUnsafe b) (1, 2)
        })

    itEffect "orElse rolls back a retrying branch" (fun () ->
        effect {
            let a = TxRef.makeUnsafe 100
            let b = TxRef.makeUnsafe 200

            let! chosen =
                TxRef.atomically (
                    TxRef.orElse
                        (stm {
                            do! TxRef.set a 999
                            do! TxRef.set b 888
                            return! TxRef.retry
                        })
                        (stm { return "fallback" })
                )

            toBe chosen "fallback"
            same (TxRef.getUnsafe a, TxRef.getUnsafe b) (100, 200)
        })

    itEffect "retry suspends and resumes after a watched ref commits" (fun () ->
        effect {
            let flag = TxRef.makeUnsafe false
            let observed = TxRef.makeUnsafe 0

            let waiter =
                TxRef.atomically (
                    stm {
                        let! ready = TxRef.get flag

                        if not ready then
                            return! TxRef.retry

                        do! TxRef.set observed 42
                    }
                )

            let! fiber = Effect.fork waiter
            do! Effect.sleep 0
            let! before = Fiber.poll fiber
            toBe (Option.isNone before) true

            do! TxRef.atomically (TxRef.set flag true)
            do! Fiber.join fiber
            toBe (TxRef.getUnsafe observed) 42
        }))

describe "TxChunk / TxHashMap / TxHashSet" (fun () ->
    itEffect "TxChunk appends and prepends inside one transaction" (fun () ->
        effect {
            let! chunk = (TxChunk.fromIterable [ 1; 2; 3 ]: Effect<TxChunk<int>, string, Context>)

            do!
                TxRef.atomically (
                    stm {
                        do! TxChunk.prepend chunk 0
                        do! TxChunk.append chunk 4
                    }
                )

            let! values = TxRef.atomically (TxChunk.get chunk)
            same (Chunk.toArray values |> Array.toList) [ 0; 1; 2; 3; 4 ]
        })

    itEffect "TxHashMap writes, reads, and removes keys" (fun () ->
        effect {
            let! map = (TxHashMap.empty (): Effect<TxHashMap<string, int>, string, Context>)

            let! value =
                TxRef.atomically (
                    stm {
                        do! TxHashMap.set map "a" 1
                        do! TxHashMap.set map "b" 2
                        return! TxHashMap.get map "a"
                    }
                )

            do! TxRef.atomically (TxHashMap.remove map "b")
            let! size = TxRef.atomically (TxHashMap.size map)
            same (value, size) (Some 1, 1)
        })

    itEffect "TxHashSet keeps unique values" (fun () ->
        effect {
            let! set = (TxHashSet.empty (): Effect<TxHashSet<int>, string, Context>)

            do!
                TxRef.atomically (
                    stm {
                        do! TxHashSet.add set 5
                        do! TxHashSet.add set 5
                        do! TxHashSet.add set 6
                    }
                )

            let! has5 = TxRef.atomically (TxHashSet.has set 5)
            let! size = TxRef.atomically (TxHashSet.size set)
            same (has5, size) (true, 2)
        }))

describe "TxDeferred" (fun () ->
    itEffect "await suspends until succeed completes the deferred" (fun () ->
        effect {
            let! deferred = (TxDeferred.make (): Effect<TxDeferred<int, string>, string, Context>)
            let! fiber = Effect.fork (TxDeferred.await deferred)
            do! Effect.sleep 0
            let! before = Fiber.poll fiber
            toBe (Option.isNone before) true

            let! first = TxRef.atomically (TxDeferred.succeed deferred 7)
            let! value = Fiber.join fiber
            same (first, value) (true, 7)
        })

    itEffect "fail completes with a typed failure" (fun () ->
        effect {
            let! deferred = (TxDeferred.make (): Effect<TxDeferred<int, string>, string, Context>)
            let! first = TxRef.atomically (TxDeferred.fail deferred "boom")
            let! exit = Effect.exit (TxDeferred.await deferred)

            match exit with
            | Success _ -> failwith "expected TxDeferred.await to fail"
            | Failure cause -> same (first, Cause.failures cause) (true, [ "boom" ])
        }))

describe "TxQueue" (fun () ->
    itEffect "bounded offer/take and size work" (fun () ->
        effect {
            let! queue = (TxQueue.bounded 2: Effect<TxQueue<int, string>, string, Context>)
            let! accepted = TxQueue.offer queue 42
            let! size = TxQueue.size queue
            let! value = TxQueue.take queue
            let! empty = TxQueue.isEmpty queue
            same (accepted, size, value, empty) (true, 1, 42, true)
        })

    itEffect "take suspends while empty and resumes after offer" (fun () ->
        effect {
            let! queue = (TxQueue.bounded 10: Effect<TxQueue<int, string>, string, Context>)
            let! fiber = Effect.fork (TxQueue.take queue)
            do! Effect.sleep 0
            let! before = Fiber.poll fiber
            toBe (Option.isNone before) true

            let! accepted = TxQueue.offer queue 99
            let! value = Fiber.join fiber
            same (accepted, value) (true, 99)
        })

    itEffect "dropping and sliding strategies handle overflow" (fun () ->
        effect {
            let! dropping = (TxQueue.dropping 2: Effect<TxQueue<int, string>, string, Context>)
            let! d1 = TxQueue.offer dropping 1
            let! d2 = TxQueue.offer dropping 2
            let! d3 = TxQueue.offer dropping 3
            let! droppedValues = TxQueue.takeAll dropping

            let! sliding = (TxQueue.sliding 2: Effect<TxQueue<int, string>, string, Context>)
            do! ignored (TxQueue.offerAll sliding [ 1; 2; 3 ])
            let! slidValues = TxQueue.takeAll sliding

            same (d1, d2, d3, droppedValues |> Array.toList, slidValues |> Array.toList) (true, true, false, [ 1; 2 ], [ 2; 3 ])
        }))

describe "TxPriorityQueue" (fun () ->
    itEffect "keeps elements in priority order" (fun () ->
        effect {
            let! queue = (TxPriorityQueue.fromIterable intOrder [ 3; 1; 2 ]: Effect<TxPriorityQueue<int>, string, Context>)
            let! first = TxPriorityQueue.take queue
            do! TxPriorityQueue.offerAll queue [ 5; 4 ]
            let! rest = TxPriorityQueue.takeAll queue
            same (first, rest) (1, [ 2; 3; 4; 5 ])
        })

    itEffect "take suspends while empty and resumes after offer" (fun () ->
        effect {
            let! queue = (TxPriorityQueue.empty intOrder: Effect<TxPriorityQueue<int>, string, Context>)
            let! fiber = Effect.fork (TxPriorityQueue.take queue)
            do! Effect.sleep 0
            let! before = Fiber.poll fiber
            toBe (Option.isNone before) true

            do! TxPriorityQueue.offer queue 11
            let! value = Fiber.join fiber
            toBe value 11
        }))

describe "TxPubSub / TxSubscriptionRef" (fun () ->
    itEffect "TxPubSub broadcasts to current subscribers" (fun () ->
        effect {
            let! hub = (TxPubSub.unbounded (): Effect<TxPubSub<int>, unit, Context>)
            let! sub1 = TxPubSub.subscribe hub
            let! sub2 = TxPubSub.subscribe hub
            let! published = TxPubSub.publish hub 7
            let! one = TxQueue.take sub1
            let! two = TxQueue.take sub2
            same (published, one, two) (true, 7, 7)
        })

    itEffect "TxSubscriptionRef changes yields current and updated values" (fun () ->
        effect {
            let! subRef = (TxSubscriptionRef.make 0: Effect<TxSubscriptionRef<int>, unit, Context>)
            let! changes = TxSubscriptionRef.changes subRef
            let! initial = TxQueue.take changes
            do! TxSubscriptionRef.set subRef 1
            do! TxSubscriptionRef.update subRef (fun n -> n + 1)
            let! one = TxQueue.take changes
            let! two = TxQueue.take changes
            same (initial, one, two) (0, 1, 2)
        }))

describe "TxSemaphore / TxReentrantLock" (fun () ->
    itEffect "TxSemaphore acquire suspends until release" (fun () ->
        effect {
            let! semaphore = (TxSemaphore.make 1: Effect<TxSemaphore, string, Context>)
            do! TxSemaphore.acquire semaphore
            let! fiber = Effect.fork (TxSemaphore.acquire semaphore)
            do! Effect.sleep 0
            let! before = Fiber.poll fiber
            toBe (Option.isNone before) true

            do! TxSemaphore.release semaphore
            do! Fiber.join fiber
            let! available = TxSemaphore.available semaphore
            toBe available 0
        })

    itEffect "TxSemaphore withPermit releases after use" (fun () ->
        effect {
            let! semaphore = (TxSemaphore.make 2: Effect<TxSemaphore, string, Context>)
            let! inside = TxSemaphore.withPermit semaphore (TxSemaphore.available semaphore)
            let! after = TxSemaphore.available semaphore
            same (inside, after) (1, 2)
        })

    itEffect "TxReentrantLock supports same-fiber reentrant write locking" (fun () ->
        effect {
            let! lock = (TxReentrantLock.make (): Effect<TxReentrantLock, string, Context>)
            let! first = TxReentrantLock.acquireWrite lock
            let! second = TxReentrantLock.acquireWrite lock
            let! held = TxReentrantLock.writeLocks lock
            do! ignored (TxReentrantLock.releaseWrite lock)
            let! afterOneRelease = TxReentrantLock.writeLocks lock
            do! ignored (TxReentrantLock.releaseWrite lock)
            let! lockedAfter = TxReentrantLock.locked lock
            same (first, second, held, afterOneRelease, lockedAfter) (1, 2, 2, 1, false)
        })

    itEffect "TxReentrantLock writer waits for a reader in another fiber" (fun () ->
        effect {
            let! lock = (TxReentrantLock.make (): Effect<TxReentrantLock, string, Context>)
            do! ignored (TxReentrantLock.acquireRead lock)

            let! writer = Effect.fork (TxReentrantLock.acquireWrite lock)
            do! Effect.sleep 0
            let! before = Fiber.poll writer
            toBe (Option.isNone before) true

            do! ignored (TxReentrantLock.releaseRead lock)
            let! writeCount = Fiber.join writer
            let! writeLocked = TxReentrantLock.writeLocked lock
            same (writeCount, writeLocked) (1, true)
        }))
