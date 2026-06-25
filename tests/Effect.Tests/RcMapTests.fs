module Effect.Tests.RcMapTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/RcMap.test.ts.
//
// Adapted to this port's kernel (see RcMap.fs header): `get`/`make` take an
// explicit `Scope`; idle-TTL eviction uses *real* time (there is no `TestClock`),
// so the timing tests use small real delays with generous margins instead of
// `TestClock.adjust`; capacity-exceeded is observed as a defect carrying
// `ExceededCapacityError`.

/// Run an effect, returning its value or failing the test on any failure.
let private run (eff: Effect<'A, 'E, unit>) : 'A =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure c -> failwithf "unexpected failure: %s" (Cause.render c)

/// A thread-safe append-only log (acquire/release run on background eviction
/// fibers, so reads/writes are guarded).
let private mkLog () =
    let gate = obj ()
    let items = ResizeArray<string>()
    let add (x: string) = lock gate (fun () -> items.Add x)
    let snapshot () = lock gate (fun () -> List.ofSeq items)
    add, snapshot

/// A lookup that records acquisition/release of `key` into the two logs, using
/// `Scope.acquireRelease` to tie the release to the entry's scope.
let private trackingLookup (addAcq: string -> unit) (addRel: string -> unit) =
    fun (key: string) (scope: Scope<string, unit>) ->
        Scope.acquireRelease
            scope
            (Effect.sync (fun () ->
                addAcq key
                key))
            (fun _ _ -> Effect.sync (fun () -> addRel key))

let private newScope () : Scope<string, unit> = Scope.make ()

// --- deterministic-time helpers (idle-TTL tests use a TestClock) ---

/// A thread-safe ordered log that can be *awaited* until it reaches a given
/// count — releases run on background eviction fibers, so the test waits on the
/// release *condition* (a completed task), never a wall-clock sleep.
type private CountLatch() =
    let gate = obj ()
    let items = ResizeArray<string>()
    let waiters = ResizeArray<int * System.Threading.Tasks.TaskCompletionSource<unit>>()

    member _.Add(x: string) =
        let due =
            lock gate (fun () ->
                items.Add x
                let d = waiters |> Seq.filter (fun (n, _) -> items.Count >= n) |> Seq.toList
                waiters.RemoveAll(fun (n, _) -> items.Count >= n) |> ignore
                d)

        due |> List.iter (fun (_, t) -> t.TrySetResult() |> ignore)

    member _.Snapshot() = lock gate (fun () -> List.ofSeq items)

    member _.AwaitTask(n: int) : System.Threading.Tasks.Task<unit> =
        lock gate (fun () ->
            if items.Count >= n then
                System.Threading.Tasks.Task.FromResult(())
            else
                let t =
                    System.Threading.Tasks.TaskCompletionSource<unit>(
                        System.Threading.Tasks.TaskCreationOptions.RunContinuationsAsynchronously
                    )

                waiters.Add(n, t)
                t.Task)

/// Suspend until `latch` has logged at least `n` items.
let private awaitCount (latch: CountLatch) (n: int) : Effect<unit, string, unit> =
    Effect.promise (fun () -> latch.AwaitTask n)

// ----------------------------------------------------------------------------

[<Fact>]
let ``deallocation`` () =
    let addAcq, snapAcq = mkLog ()
    let addRel, snapRel = mkLog ()

    let map =
        run (
            effect {
                let mapScope = newScope ()
                let! map = RcMap.make mapScope (trackingLookup addAcq addRel)

                do! Effect.sync (fun () -> Assert.Equal<string list>([], snapAcq ()))

                let! foo = Scope.scoped (fun s -> RcMap.get map "foo" s)

                do!
                    Effect.sync (fun () ->
                        Assert.Equal("foo", foo)
                        Assert.Equal<string list>([ "foo" ], snapAcq ())
                        Assert.Equal<string list>([ "foo" ], snapRel ()))

                let scopeA = newScope ()
                let scopeB = newScope ()
                do! RcMap.get map "bar" scopeA |> Effect.map ignore
                do! Scope.scoped (fun s -> RcMap.get map "bar" s) |> Effect.map ignore
                do! RcMap.get map "baz" scopeB |> Effect.map ignore
                do! Scope.scoped (fun s -> RcMap.get map "baz" s) |> Effect.map ignore

                do!
                    Effect.sync (fun () ->
                        Assert.Equal<string list>([ "foo"; "bar"; "baz" ], snapAcq ())
                        Assert.Equal<string list>([ "foo" ], snapRel ()))

                do! Scope.close scopeB (Success(box ()))
                do! Effect.sync (fun () -> Assert.Equal<string list>([ "foo"; "baz" ], snapRel ()))

                do! Scope.close scopeA (Success(box ()))
                do! Effect.sync (fun () -> Assert.Equal<string list>([ "foo"; "baz"; "bar" ], snapRel ()))

                let scopeC = newScope ()
                do! RcMap.get map "qux" scopeC |> Effect.map ignore

                do!
                    Effect.sync (fun () ->
                        Assert.Equal<string list>([ "foo"; "bar"; "baz"; "qux" ], snapAcq ())
                        Assert.Equal<string list>([ "foo"; "baz"; "bar" ], snapRel ()))

                do! Scope.close mapScope (Success(box ()))
                do! Effect.sync (fun () -> Assert.Equal<string list>([ "foo"; "baz"; "bar"; "qux" ], snapRel ()))

                return map
            }
        )

    // get on a closed map is interrupted
    match Effect.runSync () (Scope.scoped (fun s -> RcMap.get map "boom" s)) with
    | Failure c -> Assert.True(c.Reasons |> List.exists Cause.isInterruptReason, "expected an interrupt")
    | Success _ -> Assert.Fail "expected an interrupt on a closed map"

[<Fact>]
let ``idleTimeToLive`` () =
    let tc = TestClock.make ()
    let acq = CountLatch()
    let rel = CountLatch()

    let map =
        run (
            effect {
                let mapScope = newScope ()

                return!
                    RcMap.makeWithClock
                        (TestClock.clock tc)
                        mapScope
                        None
                        (fun _ -> Duration.millis 120.0)
                        (trackingLookup acq.Add rel.Add)
            }
        )

    run (Scope.scoped (fun s -> RcMap.get map "foo" s) |> Effect.map ignore)
    Assert.Equal<string list>([ "foo" ], acq.Snapshot())
    Assert.Equal<string list>([], rel.Snapshot())

    // foo's eviction timer is parked; advancing past its TTL releases it.
    run (TestClock.awaitSleeps tc 1)
    run (TestClock.adjust tc (Duration.millis 250.0))
    run (awaitCount rel 1)
    Assert.Equal<string list>([ "foo" ], rel.Snapshot())

    run (Scope.scoped (fun s -> RcMap.get map "bar" s) |> Effect.map ignore)
    Assert.Equal<string list>([ "foo"; "bar" ], acq.Snapshot())
    Assert.Equal<string list>([ "foo" ], rel.Snapshot())

    // re-acquire while bar is idle: shared (no second acquire), entry kept alive
    run (TestClock.awaitSleeps tc 2)
    run (Scope.scoped (fun s -> RcMap.get map "bar" s) |> Effect.map ignore)
    Assert.Equal<string list>([ "foo"; "bar" ], acq.Snapshot())
    Assert.Equal<string list>([ "foo" ], rel.Snapshot())

    run (TestClock.adjust tc (Duration.millis 250.0))
    run (awaitCount rel 2)
    Assert.Equal<string list>([ "foo"; "bar" ], rel.Snapshot())

    run (Scope.scoped (fun s -> RcMap.get map "baz" s) |> Effect.map ignore)
    Assert.Equal<string list>([ "foo"; "bar"; "baz" ], acq.Snapshot())
    run (RcMap.invalidate map "baz")
    run (awaitCount rel 3)
    Assert.Equal<string list>([ "foo"; "bar"; "baz" ], rel.Snapshot())

[<Fact>]
let ``touch`` () =
    let tc = TestClock.make ()
    let acq = CountLatch()
    let rel = CountLatch()

    let map =
        run (
            effect {
                let mapScope = newScope ()

                return!
                    RcMap.makeWithClock
                        (TestClock.clock tc)
                        mapScope
                        None
                        (fun _ -> Duration.millis 200.0)
                        (trackingLookup acq.Add rel.Add)
            }
        )

    run (Scope.scoped (fun s -> RcMap.get map "foo" s) |> Effect.map ignore)
    Assert.Equal<string list>([ "foo" ], acq.Snapshot())
    Assert.Equal<string list>([], rel.Snapshot())

    // park foo's timer, advance partway: still alive.
    run (TestClock.awaitSleeps tc 1)
    run (TestClock.adjust tc (Duration.millis 80.0))
    Assert.Equal<string list>([], rel.Snapshot())

    // touch resets the timer; advancing to the ORIGINAL deadline must not release.
    run (RcMap.touch map "foo")
    run (TestClock.adjust tc (Duration.millis 120.0))
    Assert.Equal<string list>([], rel.Snapshot())

    // advancing past the touch-reset deadline finally releases it.
    run (TestClock.awaitSleeps tc 2)
    run (TestClock.adjust tc (Duration.millis 160.0))
    run (awaitCount rel 1)
    Assert.Equal<string list>([ "foo" ], rel.Snapshot())

[<Fact>]
let ``capacity`` () =
    let tc = TestClock.make ()
    let acq = CountLatch()
    let rel = CountLatch()

    let map =
        run (
            effect {
                let mapScope = newScope ()

                return!
                    RcMap.makeWithClock
                        (TestClock.clock tc)
                        mapScope
                        (Some 2)
                        (fun _ -> Duration.millis 120.0)
                        (trackingLookup acq.Add rel.Add)
            }
        )

    Assert.Equal("foo", run (Scope.scoped (fun s -> RcMap.get map "foo" s)))
    Assert.Equal("foo", run (Scope.scoped (fun s -> RcMap.get map "foo" s)))
    Assert.Equal("bar", run (Scope.scoped (fun s -> RcMap.get map "bar" s)))

    match Effect.runSync () (Scope.scoped (fun s -> RcMap.get map "baz" s)) with
    | Failure c ->
        let hit =
            Cause.defects c
            |> List.exists (fun d ->
                match d with
                | :? ExceededCapacityError -> true
                | _ -> false)

        Assert.True(hit, "expected an ExceededCapacityError defect")
    | Success _ -> Assert.Fail "expected a capacity failure"

    // advance past the idle TTL so both idle entries are evicted, freeing capacity.
    run (TestClock.awaitSleeps tc 2)
    run (TestClock.adjust tc (Duration.millis 250.0))
    run (awaitCount rel 2)
    Assert.Equal("baz", run (Scope.scoped (fun s -> RcMap.get map "baz" s)))

type private Key = { Id: int }

[<Fact>]
let ``complex key`` () =
    let map =
        run (
            effect {
                let mapScope = newScope ()

                return!
                    RcMap.makeWith mapScope (Some 1) (fun _ -> Duration.zero) (fun (k: Key) _ -> Effect.succeed k.Id)
            }
        )

    // both gets share the open scope, so the second is a hit (capacity 1 not exceeded)
    let pair =
        run (
            Scope.scoped (fun s ->
                effect {
                    let! a = RcMap.get map { Id = 1 } s
                    let! b = RcMap.get map { Id = 1 } s
                    return (a, b)
                })
        )

    Assert.Equal((1, 1), pair)

[<Fact>]
let ``keys lookup`` () =
    let map =
        run (
            effect {
                let mapScope = newScope ()
                return! RcMap.make mapScope (fun (k: string) _ -> Effect.succeed k)
            }
        )

    let ks =
        run (
            Scope.scoped (fun s ->
                effect {
                    do! RcMap.get map "foo" s |> Effect.map ignore
                    do! RcMap.get map "bar" s |> Effect.map ignore
                    do! RcMap.get map "baz" s |> Effect.map ignore
                    return! RcMap.keys map
                })
        )

    Assert.Equal<string list>([ "bar"; "baz"; "foo" ], List.sort ks)

[<Fact>]
let ``dynamic idleTimeToLive`` () =
    let tc = TestClock.make ()
    let acq = CountLatch()
    let rel = CountLatch()

    let ttl (k: string) =
        if k.StartsWith "short:" then
            Duration.millis 80.0
        else
            Duration.millis 300.0

    let map =
        run (
            effect {
                let mapScope = newScope ()
                return! RcMap.makeWithClock (TestClock.clock tc) mapScope None ttl (trackingLookup acq.Add rel.Add)
            }
        )

    run (Scope.scoped (fun s -> RcMap.get map "short:a" s) |> Effect.map ignore)
    run (Scope.scoped (fun s -> RcMap.get map "long:b" s) |> Effect.map ignore)
    Assert.Equal<string list>([ "short:a"; "long:b" ], acq.Snapshot())
    Assert.Equal<string list>([], rel.Snapshot())

    // both timers parked; advancing past the short TTL (not the long) evicts only short.
    run (TestClock.awaitSleeps tc 2)
    run (TestClock.adjust tc (Duration.millis 160.0))
    run (awaitCount rel 1)
    Assert.Equal<string list>([ "short:a" ], rel.Snapshot())

    run (TestClock.adjust tc (Duration.millis 300.0))
    run (awaitCount rel 2)
    Assert.Equal<string list>([ "short:a"; "long:b" ], rel.Snapshot())

[<Fact>]
let ``dynamic idleTimeToLive with touch`` () =
    let tc = TestClock.make ()
    let acq = CountLatch()
    let rel = CountLatch()

    let ttl (k: string) =
        if k.StartsWith "short:" then
            Duration.millis 150.0
        else
            Duration.millis 2000.0

    let map =
        run (
            effect {
                let mapScope = newScope ()
                return! RcMap.makeWithClock (TestClock.clock tc) mapScope None ttl (trackingLookup acq.Add rel.Add)
            }
        )

    run (Scope.scoped (fun s -> RcMap.get map "short:a" s) |> Effect.map ignore)
    Assert.Equal<string list>([ "short:a" ], acq.Snapshot())
    Assert.Equal<string list>([], rel.Snapshot())

    run (TestClock.awaitSleeps tc 1)
    run (TestClock.adjust tc (Duration.millis 75.0))
    // touch resets the timer; advancing to the original deadline must not release.
    run (RcMap.touch map "short:a")
    run (TestClock.adjust tc (Duration.millis 75.0))
    Assert.Equal<string list>([], rel.Snapshot())

    // advancing past the touch-reset deadline releases it.
    run (TestClock.awaitSleeps tc 2)
    run (TestClock.adjust tc (Duration.millis 150.0))
    run (awaitCount rel 1)
    Assert.Equal<string list>([ "short:a" ], rel.Snapshot())
