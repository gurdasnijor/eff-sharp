namespace Effect

open System.Collections.Generic
open System.Threading.Tasks

/// Wave 5: `Semaphore` — an asynchronous counting semaphore.
///
/// Port of repos/effect-smol/packages/effect/src/Semaphore.ts. A `Semaphore`
/// owns a number of permits; work runs only after acquiring the permits it
/// needs, which are returned when it finishes. Acquirers that cannot be served
/// immediately suspend and are woken, in FIFO order, as permits are released.
///
/// Representation — `Permits` (total) and `Taken` counters plus a FIFO list of
/// suspended waiters, all guarded by a lock (upstream relies on JS's single
/// thread; this port runs fibers on real threads via the `Async` core). A
/// blocked `take` suspends on a `TaskCompletionSource`; a `release`/`resize`
/// allocates freed permits to the waiting head(s) under the lock and resumes
/// them — the same suspension pattern as the merged `Latch`/`Deferred`. Because
/// allocation happens in the releaser under the lock, FIFO ordering is exact and
/// no permits are double-counted. `withPermits` is built on the merged
/// `Effect.acquireUseRelease`, so permits are released on success, failure, and
/// interruption alike.
///
/// Omissions / notes (per CONVENTIONS):
///   * The dual/pipe currying is dropped — every function takes `self` first;
///     `effect |> Semaphore.withPermit sem` still reads naturally.
///   * `available` is added as a small convenience (free permit count); upstream
///     exposes the same value only through `release`/`take` return values.
///   * Interrupting a `take` that is *already suspended in the waiter queue* is a
///     no-op in this cooperative runtime (an `Async.AwaitTask` carries no
///     cancellation); `withPermits` interruption is fully honored because the
///     guarded effect, not the acquire, is what suspends.

type internal SemaphoreWaiter =
    { N: int
      Tcs: TaskCompletionSource<unit> }

/// A counting semaphore. Mutable state is guarded by `Lock`.
type Semaphore =
    internal
        { Lock: obj
          mutable Permits: int
          mutable Taken: int
          Waiters: List<SemaphoreWaiter> }

[<RequireQualifiedAccess>]
module Semaphore =

    let private newGate () =
        TaskCompletionSource<unit>(TaskCreationOptions.RunContinuationsAsynchronously)

    // --- internal core (callers hold the lock) ---

    /// Hand freed permits to waiting acquirers in FIFO order, serving each waiter
    /// whose request fits the currently-free permits and stopping once none are
    /// free. A waiter that does not fit is skipped, letting a smaller later
    /// request proceed (matching upstream's iteration).
    let private wakeUnsafe (self: Semaphore) =
        let mutable i = 0

        while i < self.Waiters.Count && self.Permits - self.Taken > 0 do
            let w = self.Waiters.[i]

            if self.Permits - self.Taken >= w.N then
                self.Taken <- self.Taken + w.N
                self.Waiters.RemoveAt i
                w.Tcs.TrySetResult() |> ignore
            else
                i <- i + 1

    /// Apply `f` to `Taken`, wake eligible waiters, and return the free count.
    let private updateTakenUnsafe (self: Semaphore) (f: int -> int) : int =
        self.Taken <- f self.Taken
        wakeUnsafe self
        self.Permits - self.Taken

    // --- constructors ---

    /// Construct synchronously with `permits` total permits. (Semaphore.makeUnsafe)
    let makeUnsafe (permits: int) : Semaphore =
        { Lock = obj ()
          Permits = permits
          Taken = 0
          Waiters = List<SemaphoreWaiter>() }

    /// Construct inside an `Effect`. (Semaphore.make)
    let make (permits: int) : Effect<Semaphore, 'E, 'R> =
        Effect.sync (fun () -> makeUnsafe permits)

    // --- manual permit operations ---

    /// Acquire `permits`, suspending (FIFO) until they are available; succeeds
    /// with the acquired count. (Semaphore.take)
    let take (self: Semaphore) (permits: int) : Effect<int, 'E, 'R> =
        Effect(fun _ _ ->
            async {
                let waitOn =
                    lock self.Lock (fun () ->
                        if self.Permits - self.Taken >= permits then
                            self.Taken <- self.Taken + permits
                            None
                        else
                            let tcs = newGate ()
                            self.Waiters.Add { N = permits; Tcs = tcs }
                            Some tcs)

                match waitOn with
                | None -> ()
                | Some tcs -> do! Async.AwaitTask tcs.Task

                return Success permits
            })

    /// Release `permits` and return the resulting free count. (Semaphore.release)
    let release (self: Semaphore) (permits: int) : Effect<int, 'E, 'R> =
        Effect.sync (fun () -> lock self.Lock (fun () -> updateTakenUnsafe self (fun t -> t - permits)))

    /// Release every taken permit and return the resulting free count.
    /// (Semaphore.releaseAll)
    let releaseAll (self: Semaphore) : Effect<int, 'E, 'R> =
        Effect.sync (fun () -> lock self.Lock (fun () -> updateTakenUnsafe self (fun _ -> 0)))

    /// Set the total permit count. Existing acquisitions stay taken; if the new
    /// total exceeds what is taken, waiting acquirers are served. (Semaphore.resize)
    let resize (self: Semaphore) (permits: int) : Effect<unit, 'E, 'R> =
        Effect.sync (fun () ->
            lock self.Lock (fun () ->
                self.Permits <- permits

                if self.Permits - self.Taken >= 0 then
                    updateTakenUnsafe self id |> ignore))

    /// The current number of free permits (a port convenience). (Semaphore.available)
    let available (self: Semaphore) : Effect<int, 'E, 'R> =
        Effect.sync (fun () -> lock self.Lock (fun () -> self.Permits - self.Taken))

    // --- scoped permit operations ---

    /// Run `effect` while holding `permits`, releasing them when it exits
    /// (success, failure, or interruption). Waits until the permits are
    /// available. (Semaphore.withPermits)
    let withPermits (self: Semaphore) (permits: int) (effect: Effect<'A, 'E, 'R>) : Effect<'A, 'E, 'R> =
        if permits <= 0 then
            effect
        else
            Effect.acquireUseRelease (take self permits) (fun _ -> effect) (fun _ _ ->
                release self permits |> Effect.map ignore)

    /// `withPermits` holding exactly one permit. (Semaphore.withPermit)
    let withPermit (self: Semaphore) (effect: Effect<'A, 'E, 'R>) : Effect<'A, 'E, 'R> = withPermits self 1 effect

    /// Run `effect` only if `permits` are immediately available, yielding
    /// `Some result`; otherwise `None` without running it. Permits are released
    /// when it exits. (Semaphore.withPermitsIfAvailable)
    let withPermitsIfAvailable
        (self: Semaphore)
        (permits: int)
        (effect: Effect<'A, 'E, 'R>)
        : Effect<'A option, 'E, 'R> =
        if permits <= 0 then
            effect |> Effect.map Some
        else
            Effect.suspend (fun () ->
                let took =
                    lock self.Lock (fun () ->
                        if self.Permits - self.Taken >= permits then
                            self.Taken <- self.Taken + permits
                            true
                        else
                            false)

                if not took then
                    Effect.succeed None
                else
                    Effect.acquireUseRelease (Effect.succeed ()) (fun () -> effect |> Effect.map Some) (fun _ _ ->
                        release self permits |> Effect.map ignore))
