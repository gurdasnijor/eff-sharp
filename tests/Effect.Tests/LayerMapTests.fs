module Effect.Tests.LayerMapTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/LayerMap.test.ts, adapted to
// the port's explicit-`Scope` model. Idle-TTL eviction is driven by a `TestClock`
// injected via `LayerMap.makeWithClock`, so virtual time (`TestClock.adjust`) —
// not wall-clock sleeps — controls eviction deterministically.

let private voidExit: Exit<obj, obj> = Success(box ())

let private run (eff: Effect<'A, 'E, unit>) : 'A =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure c -> failwithf "unexpected failure: %s" (Cause.render c)

let private dummyTag = Tag.make<unit> "LayerMapTest/dummy"

/// A thread-safe ordered log awaitable until it reaches a given count — release
/// runs on a background eviction fiber, so we wait on the *condition*, not time.
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

let private awaitCount (latch: CountLatch) (n: int) : Effect<unit, string, unit> =
    Effect.promise (fun () -> latch.AwaitTask n)

/// A layer that records acquisition/release of `key` on its build scope (the
/// service it provides is irrelevant; the resource lifecycle is what we observe).
let private makeLayer (acquired: CountLatch) (released: CountLatch) (key: string) : Layer<string, unit> =
    Layer.scoped dummyTag (fun scope ->
        Scope.acquireRelease scope (Effect.sync (fun () -> acquired.Add key)) (fun _ _ ->
            Effect.sync (fun () -> released.Add key)))

let private getScoped (lm: LayerMap<string, string>) (key: string) : Effect<unit, string, unit> =
    Scope.scoped (fun caller -> LayerMap.contextEffect lm key caller |> Effect.map ignore)

[<Fact>]
let ``make supports per-key idle time-to-live`` () =
    let tc = TestClock.make ()
    let acquired = CountLatch()
    let released = CountLatch()

    let idleTTL (key: string) =
        if key.StartsWith "short" then
            Duration.millis 50.0
        else
            Duration.millis 300.0

    let program: Effect<unit, string, unit> =
        effect {
            let mapScope = Scope.make<string, unit> ()
            let! lm = LayerMap.makeWithClock (TestClock.clock tc) mapScope idleTTL (makeLayer acquired released)
            do! getScoped lm "short:a"
            do! getScoped lm "long:b"

            do!
                Effect.sync (fun () ->
                    Assert.Equal<string list>([ "short:a"; "long:b" ], acquired.Snapshot())
                    Assert.Equal<string list>([], released.Snapshot()))

            // both timers parked; advance past the short TTL only.
            do! TestClock.awaitSleeps tc 2
            do! TestClock.adjust tc (Duration.millis 150.0)
            do! awaitCount released 1
            do! Effect.sync (fun () -> Assert.Equal<string list>([ "short:a" ], released.Snapshot()))

            do! TestClock.adjust tc (Duration.millis 400.0)
            do! awaitCount released 2
            do! Effect.sync (fun () -> Assert.Equal<string list>([ "short:a"; "long:b" ], released.Snapshot()))
            do! Scope.close mapScope voidExit
        }

    run program

[<Fact>]
let ``fromRecord supports per-key idle time-to-live`` () =
    let tc = TestClock.make ()
    let acquired = CountLatch()
    let released = CountLatch()

    let layers =
        Map.ofList
            [ "short", makeLayer acquired released "short"
              "long", makeLayer acquired released "long" ]

    let idleTTL (key: string) =
        if key = "short" then
            Duration.millis 50.0
        else
            Duration.millis 300.0

    let program: Effect<unit, string, unit> =
        effect {
            let mapScope = Scope.make<string, unit> ()
            let! lm = LayerMap.fromRecordWithClock (TestClock.clock tc) mapScope idleTTL layers
            do! getScoped lm "short"
            do! getScoped lm "long"
            do! Effect.sync (fun () -> Assert.Equal<string list>([ "short"; "long" ], acquired.Snapshot()))

            do! TestClock.awaitSleeps tc 2
            do! TestClock.adjust tc (Duration.millis 150.0)
            do! awaitCount released 1
            do! Effect.sync (fun () -> Assert.Equal<string list>([ "short" ], released.Snapshot()))

            do! TestClock.adjust tc (Duration.millis 400.0)
            do! awaitCount released 2
            do! Effect.sync (fun () -> Assert.Equal<string list>([ "short"; "long" ], released.Snapshot()))
            do! Scope.close mapScope voidExit
        }

    run program

[<Fact>]
let ``a key is built once and shared by concurrent borrowers`` () =
    let acquired = CountLatch()
    let released = CountLatch()

    let program: Effect<unit, string, unit> =
        effect {
            let mapScope = Scope.make<string, unit> ()
            let! lm = LayerMap.makeDefault mapScope (makeLayer acquired released)

            do!
                Scope.scoped (fun outer ->
                    effect {
                        let! _ = LayerMap.contextEffect lm "k" outer
                        let! _ = LayerMap.contextEffect lm "k" outer

                        do!
                            Effect.sync (fun () ->
                                Assert.Equal<string list>([ "k" ], acquired.Snapshot())
                                Assert.Equal<string list>([], released.Snapshot()))
                    })

            // both references released -> immediate release (zero TTL)
            do! Effect.sync (fun () -> Assert.Equal<string list>([ "k" ], released.Snapshot()))
            do! Scope.close mapScope voidExit
        }

    run program

[<Fact>]
let ``invalidate releases an idle entry immediately`` () =
    let tc = TestClock.make ()
    let acquired = CountLatch()
    let released = CountLatch()

    let program: Effect<unit, string, unit> =
        effect {
            let mapScope = Scope.make<string, unit> ()
            // a long TTL so the entry would not auto-evict during the test
            let! lm =
                LayerMap.makeWithClock
                    (TestClock.clock tc)
                    mapScope
                    (fun _ -> Duration.millis 10_000.0)
                    (makeLayer acquired released)

            do! getScoped lm "k"

            do!
                Effect.sync (fun () ->
                    Assert.Equal<string list>([ "k" ], acquired.Snapshot())
                    Assert.Equal<string list>([], released.Snapshot()))

            // the entry's idle timer is parked far in the future; invalidate must
            // interrupt it and release immediately (no time advance).
            do! TestClock.awaitSleeps tc 1
            do! LayerMap.invalidate lm "k"
            do! awaitCount released 1
            do! Effect.sync (fun () -> Assert.Equal<string list>([ "k" ], released.Snapshot()))
            do! Scope.close mapScope voidExit
        }

    run program
