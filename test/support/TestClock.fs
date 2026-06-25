namespace Effect

open System
open System.Collections.Generic

/// `TestClock` — a controllable `Clock` for deterministic time-based tests.
///
/// Port of repos/effect-smol/packages/effect/src/testing/TestClock.ts, adapted to
/// this kernel. Virtual time starts at `0` and only advances via `adjust`. A
/// `sleep` registers a pending wake-up and completes ONLY when `adjust` moves the
/// clock to (or past) its target time — so effects that sleep/evict on a timer
/// resume exactly when the test advances time, with no wall-clock waiting.
///
/// Lives under `test/support`, not the shipped core: it is test infrastructure
/// used by the Node/Vitest harness. The completion machinery is built on `Cell`
/// (the Fable-portable `TaskCompletionSource` replacement), so it compiles and
/// runs on both .NET and Fable/JS with no `#if`.
///
/// Adaptation vs upstream (per CONVENTIONS): upstream parks sleeps on a `Latch`
/// and resumes them on a cooperative single-threaded scheduler via `yieldNow`.
/// This kernel runs fibers on real threads (.NET) / the microtask loop (JS), so a
/// sleep is backed by a `Cell<unit>` (bridged into the async core) and
/// `awaitSleeps` exposes a *registration signal* — a condition a test can await so
/// that `adjust` is serialized with a background timer fiber's "read-now-then-
/// sleep" step (no polling / fixed delays). The warning fiber, `withLive`, and
/// nanosecond time are omitted (not needed by the ported tests; noted).
type TestClock =
    internal
        {
            Clock: Clock
            /// Advance virtual time by `millis`, completing every sleep whose target
            /// time is now reached (in time order).
            AdjustUnsafe: int64 -> unit
            /// Resolves once at least `n` sleeps have *ever* been registered
            /// (cumulative) — the registration handshake.
            AwaitSleeps: int -> Async<unit>
        }

[<RequireQualifiedAccess>]
module TestClock =

    /// One pending sleep: its absolute wake time, a sequence number (for stable
    /// time-ordering), and the cell completed when the clock reaches it.
    type private Pending =
        { WakeAt: int64
          Seq: int
          Cell: Cell<unit> }

    /// Create a fresh `TestClock` starting at virtual time `0`.
    let make () : TestClock =
        let gate = obj ()
        let mutable now = 0L
        let mutable seq = 0
        let mutable registered = 0
        let sleeps = List<Pending>()
        // Waiters on the cumulative-registration count: (threshold, cell).
        let regWaiters = List<int * Cell<unit>>()

        let signalRegistered () =
            // (caller holds `gate`) collect reg-waiters whose threshold is met.
            let due = regWaiters |> Seq.filter (fun (n, _) -> registered >= n) |> Seq.toList
            regWaiters.RemoveAll(fun (n, _) -> registered >= n) |> ignore
            due

        // A sleep parks on a `Cell<unit>` and completes only when `adjust` reaches
        // its wake time, so the determinism is independent of wall-clock (.NET
        // threads vs the JS microtask loop). `Cell` drains its waiters
        // asynchronously, so completing a cell never re-enters the completer.
        let sleepUnsafe (d: Duration) : Async<unit> =
            let ms = Duration.toMillis d

            if Double.IsNaN ms || ms <= 0.0 then
                async { return () }
            else
                let cell, due =
                    lock gate (fun () ->
                        let delta = int64 (min ms (float Int64.MaxValue))
                        let cell = Cell.make ()

                        sleeps.Add
                            { WakeAt = now + delta
                              Seq = seq
                              Cell = cell }

                        seq <- seq + 1
                        registered <- registered + 1
                        cell, signalRegistered ())

                due |> List.iter (fun (_, w) -> Cell.trySet w () |> ignore)
                Cell.await cell

        let adjustUnsafe (delta: int64) =
            let due =
                lock gate (fun () ->
                    now <- now + delta

                    let ready =
                        sleeps
                        |> Seq.filter (fun p -> p.WakeAt <= now)
                        |> Seq.sortBy (fun p -> struct (p.WakeAt, p.Seq))
                        |> Seq.toList

                    sleeps.RemoveAll(fun p -> p.WakeAt <= now) |> ignore
                    ready)

            // `Cell.trySet` drains waiters via `Async.Start` (never inline), so
            // completing here cannot re-enter the clock even were a lock held.
            due |> List.iter (fun p -> Cell.trySet p.Cell () |> ignore)

        let awaitSleeps (n: int) : Async<unit> =
            let pending =
                lock gate (fun () ->
                    if registered >= n then
                        None
                    else
                        let cell = Cell.make ()
                        regWaiters.Add(n, cell)
                        Some cell)

            match pending with
            | None -> async { return () }
            | Some cell -> Cell.await cell

        let clock: Clock =
            { CurrentTimeMillisUnsafe = fun () -> lock gate (fun () -> now)
              SleepUnsafe = sleepUnsafe }

        { Clock = clock
          AdjustUnsafe = adjustUnsafe
          AwaitSleeps = awaitSleeps }

    /// The underlying `Clock` service (to place in a `Context` under `Clock.tag`).
    let clock (self: TestClock) : Clock = self.Clock

    /// A `Context` carrying this test clock as the `Clock` service.
    let context (self: TestClock) : Context = Context.make Clock.tag self.Clock

    /// Advance virtual time by `duration`, resuming every sleep scheduled at or
    /// before the new time, in order. (TestClock.adjust)
    let adjust (self: TestClock) (duration: Duration) : Effect<unit, 'E, 'R> =
        Effect.sync (fun () -> self.AdjustUnsafe(int64 (Duration.toMillis duration)))

    /// Suspend until at least `n` sleeps have been registered (cumulatively) on
    /// this clock. Used to serialize `adjust` with a background timer fiber that
    /// must `sleep` before time is advanced — a condition wait, not a poll.
    let awaitSleeps (self: TestClock) (n: int) : Effect<unit, 'E, 'R> =
        Effect(fun _ _ ->
            async {
                do! self.AwaitSleeps n
                return Success()
            })
