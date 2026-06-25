namespace Effect

open System
open System.Threading.Tasks
open System.Collections.Generic

/// `TestClock` — a controllable `Clock` for deterministic time-based tests.
///
/// Port of repos/effect-smol/packages/effect/src/testing/TestClock.ts, adapted to
/// this kernel. Virtual time starts at `0` and only advances via `adjust`. A
/// `sleep` registers a pending wake-up and completes ONLY when `adjust` moves the
/// clock to (or past) its target time — so effects that sleep/evict on a timer
/// resume exactly when the test advances time, with no wall-clock waiting.
///
/// Adaptation vs upstream (per CONVENTIONS): upstream parks sleeps on a `Latch`
/// and resumes them on a cooperative single-threaded scheduler via `yieldNow`.
/// This kernel runs fibers on real threads, so a sleep is backed by a
/// `TaskCompletionSource` (bridged into the async core) and `awaitSleeps` exposes
/// a *registration signal* — a condition a test can await so that `adjust` is
/// serialized with a background timer fiber's "read-now-then-sleep" step (no
/// polling / fixed delays). The warning fiber, `withLive`, and nanosecond time
/// are omitted (not needed by the ported tests; noted).
type TestClock =
    internal
        {
            Clock: Clock
            /// Advance virtual time by `millis`, completing every sleep whose target
            /// time is now reached (in time order).
            AdjustUnsafe: int64 -> unit
            /// A task that completes once at least `n` sleeps have *ever* been
            /// registered (cumulative) — the registration handshake.
            AwaitSleepsTask: int -> Task
        }

[<RequireQualifiedAccess>]
module TestClock =

    /// One pending sleep: its absolute wake time, a sequence number (for stable
    /// time-ordering), and the source completed when the clock reaches it.
    type private Pending =
        { WakeAt: int64
          Seq: int
          Tcs: TaskCompletionSource<unit> }

    /// Create a fresh `TestClock` starting at virtual time `0`.
    let make () : TestClock =
        let gate = obj ()
        let mutable now = 0L
        let mutable seq = 0
        let mutable registered = 0
        let sleeps = List<Pending>()
        // Waiters on the cumulative-registration count: (threshold, tcs).
        let regWaiters = List<int * TaskCompletionSource<unit>>()

        let signalRegistered () =
            // (caller holds `gate`) collect reg-waiters whose threshold is met.
            let due = regWaiters |> Seq.filter (fun (n, _) -> registered >= n) |> Seq.toList
            regWaiters.RemoveAll(fun (n, _) -> registered >= n) |> ignore
            due

        // TestClock is a .NET-only test helper (excluded from the Fable build). It
        // keeps its `TaskCompletionSource` virtual-time machinery internally and
        // exposes it as `Async<unit>` to satisfy the `Clock.SleepUnsafe` signature —
        // the determinism (a sleep completes only when `adjust` reaches it) is
        // unchanged.
        let sleepUnsafe (d: Duration) : Async<unit> =
            let ms = Duration.toMillis d

            if Double.IsNaN ms || ms <= 0.0 then
                async { return () }
            else
                let tcs, due =
                    lock gate (fun () ->
                        let delta = int64 (min ms (float Int64.MaxValue))

                        let tcs =
                            TaskCompletionSource<unit>(TaskCreationOptions.RunContinuationsAsynchronously)

                        sleeps.Add
                            { WakeAt = now + delta
                              Seq = seq
                              Tcs = tcs }

                        seq <- seq + 1
                        registered <- registered + 1
                        tcs, signalRegistered ())

                due |> List.iter (fun (_, w) -> w.TrySetResult() |> ignore)
                Async.AwaitTask(tcs.Task)

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

            // RunContinuationsAsynchronously => continuations are scheduled, not run
            // inline, so completing here (even were a lock held) cannot re-enter.
            due |> List.iter (fun p -> p.Tcs.TrySetResult() |> ignore)

        let awaitSleepsTask (n: int) : Task =
            lock gate (fun () ->
                if registered >= n then
                    Task.CompletedTask
                else
                    let tcs =
                        TaskCompletionSource<unit>(TaskCreationOptions.RunContinuationsAsynchronously)

                    regWaiters.Add(n, tcs)
                    tcs.Task :> Task)

        let clock: Clock =
            { CurrentTimeMillisUnsafe = fun () -> lock gate (fun () -> now)
              SleepUnsafe = sleepUnsafe }

        { Clock = clock
          AdjustUnsafe = adjustUnsafe
          AwaitSleepsTask = awaitSleepsTask }

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
                do! Async.AwaitTask(self.AwaitSleepsTask n)
                return Success()
            })
