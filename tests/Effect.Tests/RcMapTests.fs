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
    let addAcq, snapAcq = mkLog ()
    let addRel, snapRel = mkLog ()

    let map =
        run (
            effect {
                let mapScope = newScope ()
                return! RcMap.makeWith mapScope None (fun _ -> Duration.millis 120.0) (trackingLookup addAcq addRel)
            }
        )

    run (Scope.scoped (fun s -> RcMap.get map "foo" s) |> Effect.map ignore)
    Assert.Equal<string list>([ "foo" ], snapAcq ())
    Assert.Equal<string list>([], snapRel ())

    System.Threading.Thread.Sleep 250
    Assert.Equal<string list>([ "foo" ], snapRel ())

    run (Scope.scoped (fun s -> RcMap.get map "bar" s) |> Effect.map ignore)
    Assert.Equal<string list>([ "foo"; "bar" ], snapAcq ())
    Assert.Equal<string list>([ "foo" ], snapRel ())

    // re-acquire within the idle window: shared (no second acquire), timer extended
    System.Threading.Thread.Sleep 50
    run (Scope.scoped (fun s -> RcMap.get map "bar" s) |> Effect.map ignore)
    Assert.Equal<string list>([ "foo"; "bar" ], snapAcq ())
    Assert.Equal<string list>([ "foo" ], snapRel ())

    System.Threading.Thread.Sleep 250
    Assert.Equal<string list>([ "foo"; "bar" ], snapRel ())

    run (Scope.scoped (fun s -> RcMap.get map "baz" s) |> Effect.map ignore)
    Assert.Equal<string list>([ "foo"; "bar"; "baz" ], snapAcq ())
    run (RcMap.invalidate map "baz")
    Assert.Equal<string list>([ "foo"; "bar"; "baz" ], snapRel ())

[<Fact>]
let ``touch`` () =
    let addAcq, snapAcq = mkLog ()
    let addRel, snapRel = mkLog ()

    let map =
        run (
            effect {
                let mapScope = newScope ()
                return! RcMap.makeWith mapScope None (fun _ -> Duration.millis 200.0) (trackingLookup addAcq addRel)
            }
        )

    run (Scope.scoped (fun s -> RcMap.get map "foo" s) |> Effect.map ignore)
    Assert.Equal<string list>([ "foo" ], snapAcq ())
    Assert.Equal<string list>([], snapRel ())

    System.Threading.Thread.Sleep 80
    Assert.Equal<string list>([], snapRel ())

    run (RcMap.touch map "foo")
    System.Threading.Thread.Sleep 120
    Assert.Equal<string list>([], snapRel ())
    System.Threading.Thread.Sleep 160
    Assert.Equal<string list>([ "foo" ], snapRel ())

[<Fact>]
let ``capacity`` () =
    let addAcq, _ = mkLog ()
    let addRel, _ = mkLog ()

    let map =
        run (
            effect {
                let mapScope = newScope ()
                return! RcMap.makeWith mapScope (Some 2) (fun _ -> Duration.millis 120.0) (trackingLookup addAcq addRel)
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

    // after the idle entries expire, capacity frees up again
    System.Threading.Thread.Sleep 250
    Assert.Equal("baz", run (Scope.scoped (fun s -> RcMap.get map "baz" s)))

type private Key = { Id: int }

[<Fact>]
let ``complex key`` () =
    let map =
        run (
            effect {
                let mapScope = newScope ()
                return! RcMap.makeWith mapScope (Some 1) (fun _ -> Duration.zero) (fun (k: Key) _ -> Effect.succeed k.Id)
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
    let addAcq, snapAcq = mkLog ()
    let addRel, snapRel = mkLog ()
    let ttl (k: string) = if k.StartsWith "short:" then Duration.millis 80.0 else Duration.millis 300.0

    let map =
        run (
            effect {
                let mapScope = newScope ()
                return! RcMap.makeWith mapScope None ttl (trackingLookup addAcq addRel)
            }
        )

    run (Scope.scoped (fun s -> RcMap.get map "short:a" s) |> Effect.map ignore)
    run (Scope.scoped (fun s -> RcMap.get map "long:b" s) |> Effect.map ignore)
    Assert.Equal<string list>([ "short:a"; "long:b" ], snapAcq ())
    Assert.Equal<string list>([], snapRel ())

    System.Threading.Thread.Sleep 160
    Assert.Equal<string list>([ "short:a" ], snapRel ())

    System.Threading.Thread.Sleep 300
    Assert.Equal<string list>([ "short:a"; "long:b" ], snapRel ())

[<Fact>]
let ``dynamic idleTimeToLive with touch`` () =
    let addAcq, snapAcq = mkLog ()
    let addRel, snapRel = mkLog ()
    let ttl (k: string) = if k.StartsWith "short:" then Duration.millis 150.0 else Duration.millis 2000.0

    let map =
        run (
            effect {
                let mapScope = newScope ()
                return! RcMap.makeWith mapScope None ttl (trackingLookup addAcq addRel)
            }
        )

    run (Scope.scoped (fun s -> RcMap.get map "short:a" s) |> Effect.map ignore)
    Assert.Equal<string list>([ "short:a" ], snapAcq ())
    Assert.Equal<string list>([], snapRel ())

    System.Threading.Thread.Sleep 75
    run (RcMap.touch map "short:a")
    System.Threading.Thread.Sleep 75
    Assert.Equal<string list>([], snapRel ())

    System.Threading.Thread.Sleep 150
    Assert.Equal<string list>([ "short:a" ], snapRel ())
