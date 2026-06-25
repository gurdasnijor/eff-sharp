module Effect.Tests.TxReentrantLockTests

open System.Collections.Concurrent
open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/TxReentrantLock.test.ts.
//
// Ownership is keyed by fiber: this port derives a fiber's id from the running
// `FiberRuntime` (reference identity). So a *manual* acquire/release pair must
// run on the SAME fiber — i.e. inside ONE `effect { }` program / one `runSync` —
// or the release would see a different owner and be a no-op. Tests that only use
// `with*`/getters (where acquire+release are internal to one call, and getters
// are owner-independent) may use separate runs. Concurrency tests fork children
// with `Effect.fork`, each getting its own fiber id.

let private run (eff: Effect<'a, 'e, unit>) : 'a =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure cause -> failwithf "unexpected failure: %s" (Cause.render cause)

let private ignored (eff: Effect<'a, 'e, 'r>) : Effect<unit, 'e, 'r> = eff |> Effect.map ignore

// --- constructors / getters ---

[<Fact>]
let ``make creates an unlocked lock`` () =
    let program =
        effect {
            let! lock = TxReentrantLock.make ()
            let! locked = TxReentrantLock.locked lock
            let! readLocked = TxReentrantLock.readLocked lock
            let! writeLocked = TxReentrantLock.writeLocked lock
            let! readLocks = TxReentrantLock.readLocks lock
            let! writeLocks = TxReentrantLock.writeLocks lock
            return (locked, readLocked, writeLocked, readLocks, writeLocks)
        }

    Assert.Equal((false, false, false, 0, 0), run program)

// --- read lock ---

[<Fact>]
let ``acquireRead and releaseRead work correctly`` () =
    let program =
        effect {
            let! lock = TxReentrantLock.make ()
            let! count = TxReentrantLock.acquireRead lock
            let! locked = TxReentrantLock.readLocked lock
            let! held = TxReentrantLock.readLocks lock
            let! remaining = TxReentrantLock.releaseRead lock
            let! lockedAfter = TxReentrantLock.readLocked lock
            return (count, locked, held, remaining, lockedAfter)
        }

    Assert.Equal((1, true, 1, 0, false), run program)

[<Fact>]
let ``multiple read acquires from same fiber are reentrant`` () =
    let program =
        effect {
            let! lock = TxReentrantLock.make ()
            let! c1 = TxReentrantLock.acquireRead lock
            let! c2 = TxReentrantLock.acquireRead lock
            let! held = TxReentrantLock.readLocks lock
            do! ignored (TxReentrantLock.releaseRead lock)
            let! after1 = TxReentrantLock.readLocks lock
            do! ignored (TxReentrantLock.releaseRead lock)
            let! after2 = TxReentrantLock.readLocks lock
            return (c1, c2, held, after1, after2)
        }

    Assert.Equal((1, 2, 2, 1, 0), run program)

// --- write lock ---

[<Fact>]
let ``acquireWrite and releaseWrite work correctly`` () =
    let program =
        effect {
            let! lock = TxReentrantLock.make ()
            let! count = TxReentrantLock.acquireWrite lock
            let! locked = TxReentrantLock.writeLocked lock
            let! held = TxReentrantLock.writeLocks lock
            let! remaining = TxReentrantLock.releaseWrite lock
            let! lockedAfter = TxReentrantLock.writeLocked lock
            return (count, locked, held, remaining, lockedAfter)
        }

    Assert.Equal((1, true, 1, 0, false), run program)

[<Fact>]
let ``multiple write acquires from same fiber are reentrant`` () =
    let program =
        effect {
            let! lock = TxReentrantLock.make ()
            let! c1 = TxReentrantLock.acquireWrite lock
            let! c2 = TxReentrantLock.acquireWrite lock
            let! held = TxReentrantLock.writeLocks lock
            do! ignored (TxReentrantLock.releaseWrite lock)
            let! after1 = TxReentrantLock.writeLocks lock
            do! ignored (TxReentrantLock.releaseWrite lock)
            let! after2 = TxReentrantLock.writeLocks lock
            return (c1, c2, held, after1, after2)
        }

    Assert.Equal((1, 2, 2, 1, 0), run program)

[<Fact>]
let ``write lock holder can acquire read lock (reentrancy)`` () =
    let program =
        effect {
            let! lock = TxReentrantLock.make ()
            do! ignored (TxReentrantLock.acquireWrite lock)
            let! readCount = TxReentrantLock.acquireRead lock
            let! locked = TxReentrantLock.locked lock
            do! ignored (TxReentrantLock.releaseRead lock)
            do! ignored (TxReentrantLock.releaseWrite lock)
            let! lockedAfter = TxReentrantLock.locked lock
            return (readCount, locked, lockedAfter)
        }

    Assert.Equal((1, true, false), run program)

// --- release without acquire ---

[<Fact>]
let ``releaseRead without acquire returns 0`` () =
    let program =
        effect {
            let! lock = TxReentrantLock.make ()
            return! TxReentrantLock.releaseRead lock
        }

    Assert.Equal(0, run program)

[<Fact>]
let ``releaseWrite without acquire returns 0`` () =
    let program =
        effect {
            let! lock = TxReentrantLock.make ()
            return! TxReentrantLock.releaseWrite lock
        }

    Assert.Equal(0, run program)

// --- with* wrappers ---

[<Fact>]
let ``withReadLock runs effect and releases`` () =
    let lock = run (TxReentrantLock.make ())

    let observed =
        run (TxReentrantLock.withReadLock lock (TxReentrantLock.readLocked lock))

    Assert.True(observed)
    Assert.False(run (TxReentrantLock.readLocked lock))

[<Fact>]
let ``withWriteLock runs effect and releases`` () =
    let lock = run (TxReentrantLock.make ())

    let observed =
        run (TxReentrantLock.withWriteLock lock (TxReentrantLock.writeLocked lock))

    Assert.True(observed)
    Assert.False(run (TxReentrantLock.writeLocked lock))

[<Fact>]
let ``withLock is an alias for withWriteLock`` () =
    let lock = run (TxReentrantLock.make ())

    let observed =
        run (TxReentrantLock.withLock lock (TxReentrantLock.writeLocked lock))

    Assert.True(observed)
    Assert.False(run (TxReentrantLock.locked lock))

[<Fact>]
let ``withReadLock releases on failure`` () =
    let lock = run (TxReentrantLock.make ())

    match Effect.runSync () (TxReentrantLock.withReadLock lock (Effect.fail "boom")) with
    | Failure cause -> Assert.Equal<string list>([ "boom" ], Cause.failures cause)
    | Success _ -> Assert.Fail "expected failure"

    Assert.False(run (TxReentrantLock.readLocked lock))

[<Fact>]
let ``withWriteLock releases on failure`` () =
    let lock = run (TxReentrantLock.make ())

    match Effect.runSync () (TxReentrantLock.withWriteLock lock (Effect.fail "boom")) with
    | Failure cause -> Assert.Equal<string list>([ "boom" ], Cause.failures cause)
    | Success _ -> Assert.Fail "expected failure"

    Assert.False(run (TxReentrantLock.writeLocked lock))

// --- scoped ---

[<Fact>]
let ``readLock scoped acquires and releases`` () =
    let lock = run (TxReentrantLock.make ())

    let inside =
        run (
            Scope.scoped (fun scope ->
                effect {
                    do! ignored (TxReentrantLock.readLock lock scope)
                    return! TxReentrantLock.readLocked lock
                })
        )

    Assert.True(inside)
    Assert.False(run (TxReentrantLock.readLocked lock))

[<Fact>]
let ``writeLock scoped acquires and releases`` () =
    let lock = run (TxReentrantLock.make ())

    let inside =
        run (
            Scope.scoped (fun scope ->
                effect {
                    do! ignored (TxReentrantLock.writeLock lock scope)
                    return! TxReentrantLock.writeLocked lock
                })
        )

    Assert.True(inside)
    Assert.False(run (TxReentrantLock.writeLocked lock))

// --- concurrency (forked fibers get distinct ids) ---

[<Fact>]
let ``multiple readers can proceed concurrently`` () =
    let program =
        effect {
            let! lock = TxReentrantLock.make ()
            let! f1 = Effect.fork (TxReentrantLock.withReadLock lock (Effect.succeed 1))
            let! f2 = Effect.fork (TxReentrantLock.withReadLock lock (Effect.succeed 2))
            let! f3 = Effect.fork (TxReentrantLock.withReadLock lock (Effect.succeed 3))
            let! r1 = Effect.join f1
            let! r2 = Effect.join f2
            let! r3 = Effect.join f3
            let! locked = TxReentrantLock.locked lock
            return (r1, r2, r3, locked)
        }

    Assert.Equal((1, 2, 3, false), run program)

[<Fact>]
let ``writer blocks another fiber's writer until released`` () =
    let order = ConcurrentQueue<string>()

    let program =
        effect {
            let! lock = TxReentrantLock.make ()
            do! ignored (TxReentrantLock.acquireWrite lock)

            let! child =
                Effect.fork (TxReentrantLock.withWriteLock lock (Effect.sync (fun () -> order.Enqueue "child-wrote")))

            order.Enqueue "parent-releasing"
            do! ignored (TxReentrantLock.releaseWrite lock)
            do! ignored (Effect.join child)
            return! TxReentrantLock.locked lock
        }

    let locked = run program
    Assert.Equal<string list>([ "parent-releasing"; "child-wrote" ], List.ofSeq order)
    Assert.False(locked)

[<Fact>]
let ``reader blocks another fiber's writer until released`` () =
    let order = ConcurrentQueue<string>()

    let program =
        effect {
            let! lock = TxReentrantLock.make ()
            do! ignored (TxReentrantLock.acquireRead lock)

            let! child =
                Effect.fork (TxReentrantLock.withWriteLock lock (Effect.sync (fun () -> order.Enqueue "child-wrote")))

            order.Enqueue "parent-releasing"
            do! ignored (TxReentrantLock.releaseRead lock)
            do! ignored (Effect.join child)
            return! TxReentrantLock.locked lock
        }

    let locked = run program
    Assert.Equal<string list>([ "parent-releasing"; "child-wrote" ], List.ofSeq order)
    Assert.False(locked)
