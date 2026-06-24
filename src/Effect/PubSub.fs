namespace Effect

open System.Collections.Generic
open System.Threading.Tasks

/// Wave 4: `PubSub` — an asynchronous publish/subscribe hub.
///
/// Port of repos/effect-smol/packages/effect/src/PubSub.ts. Publishers add
/// messages with `publish`/`publishAll`; every active `Subscription` receives a
/// full, independent copy of each message published *after* it subscribed, and
/// drains them FIFO with `take`/`takeAll`/`takeUpTo`.
///
/// Four overflow strategies, chosen by the constructor:
///   * `bounded`   — back-pressures the publisher until every subscriber has room
///                   (no message is ever lost).
///   * `dropping`  — silently drops the new message for any subscriber already at
///                   capacity.
///   * `sliding`   — evicts each full subscriber's oldest message to make room.
///   * `unbounded` — never blocks or drops.
///
/// Representation — rather than upstream's single shared ring buffer with
/// per-subscriber read cursors, each `Subscription` owns a `Queue` and a publish
/// fans the message out to every subscriber's queue. This is observably
/// equivalent for the broadcast contract (each subscriber sees every message in
/// order) while mapping cleanly onto FSharp.Core/.NET. A lock guards all state so
/// the hub is safe under the real threads the `Async` core may run fibers on
/// (upstream relies on JS's single thread); blocked takers/publishers suspend on
/// `TaskCompletionSource`s, woken when space/items appear — the same pattern as
/// the merged `Latch`/`Deferred`.
///
/// Omissions vs upstream (noted per CONVENTIONS):
///   * The `replay` option (replay buffer + per-subscriber replay window) is not
///     ported — it is an advanced feature whose linked-list replay-window
///     fast-forward has no bearing on the core broadcast/back-pressure contract.
///   * `publishUnsafe`, `takeBetween`, `isFull`, `Stream.fromPubSub`, the `isPubSub`
///     runtime guard, the `TypeId` brand and the dual/pipe currying are dropped.
///   * `take` of an empty, shut-down subscriber and `takeAll` likewise complete
///     with an interrupt `Cause` (Effect's `Effect.interrupt`).

type internal PubSubStrategy =
    | Bounded
    | Dropping
    | Sliding
    | Unbounded

/// The publish/subscribe hub. Mutable state is guarded by `Lock`.
type PubSub<'A> =
    internal
        { Lock: obj
          Capacity: int
          Strategy: PubSubStrategy
          Subscribers: List<Subscription<'A>>
          PublishWaiters: List<TaskCompletionSource<unit>>
          mutable IsShutdown: bool }

/// A handle on one subscriber's independent message queue. Created by `subscribe`
/// and removed from its parent when the owning `Scope` closes.
and Subscription<'A> =
    internal
        { Parent: PubSub<'A>
          Queue: Queue<'A>
          Takers: List<TaskCompletionSource<unit>> }

[<RequireQualifiedAccess>]
module PubSub =

    // --- internal helpers (callers hold the lock) ---

    let private newGate () =
        TaskCompletionSource<unit>(TaskCreationOptions.RunContinuationsAsynchronously)

    /// The interrupt cause produced when a suspended taker observes shutdown.
    let private interruptedCause<'E> : Cause<'E> = Cause.interrupt None

    let private wakeAll (waiters: List<TaskCompletionSource<unit>>) =
        for tcs in waiters do
            tcs.TrySetResult() |> ignore

        waiters.Clear()

    let private wakeTakers (sub: Subscription<'A>) = wakeAll sub.Takers
    let private wakePublishers (self: PubSub<'A>) = wakeAll self.PublishWaiters

    /// Try to fan `value` out to every subscriber under the active strategy,
    /// without blocking. Returns `false` only when a `Bounded` hub is full (the
    /// caller then suspends the publisher); every other strategy returns `true`.
    let private tryEnqueueUnsafe (self: PubSub<'A>) (value: 'A) : bool =
        if self.Subscribers.Count = 0 then
            true
        else
            match self.Strategy with
            | Unbounded ->
                for sub in self.Subscribers do
                    sub.Queue.Enqueue value
                    wakeTakers sub

                true
            | Dropping ->
                for sub in self.Subscribers do
                    if sub.Queue.Count < self.Capacity then
                        sub.Queue.Enqueue value
                        wakeTakers sub

                true
            | Sliding ->
                if self.Capacity > 0 then
                    for sub in self.Subscribers do
                        if sub.Queue.Count >= self.Capacity then sub.Queue.Dequeue() |> ignore
                        sub.Queue.Enqueue value
                        wakeTakers sub

                true
            | Bounded ->
                if self.Subscribers |> Seq.forall (fun s -> s.Queue.Count < self.Capacity) then
                    for sub in self.Subscribers do
                        sub.Queue.Enqueue value
                        wakeTakers sub

                    true
                else
                    false

    // --- construction ---

    let private makeUnsafe (strategy: PubSubStrategy) (capacity: int) : PubSub<'A> =
        { Lock = obj ()
          Capacity = max 0 capacity
          Strategy = strategy
          Subscribers = List<Subscription<'A>>()
          PublishWaiters = List<TaskCompletionSource<unit>>()
          IsShutdown = false }

    // NOTE: these constructors deliberately avoid an explicit `<'A>` type
    // parameter. Declaring one would stop F# from generalizing the *Effect's*
    // own phantom `'E`/`'R` channels (they'd default to `obj`), breaking
    // `let! ps = PubSub.bounded n` inside a typed `effect` block (same reasoning
    // as `Deferred.make`). The element type is still callable explicitly, e.g.
    // `PubSub.bounded<int> 2`.

    /// A back-pressured hub of the given capacity. (PubSub.bounded)
    let bounded (capacity: int) : Effect<PubSub<'A>, 'E, 'R> =
        Effect.sync (fun () -> makeUnsafe Bounded capacity)

    /// A hub that drops messages for full subscribers. (PubSub.dropping)
    let dropping (capacity: int) : Effect<PubSub<'A>, 'E, 'R> =
        Effect.sync (fun () -> makeUnsafe Dropping capacity)

    /// A hub that evicts each full subscriber's oldest message. (PubSub.sliding)
    let sliding (capacity: int) : Effect<PubSub<'A>, 'E, 'R> =
        Effect.sync (fun () -> makeUnsafe Sliding capacity)

    /// A hub with no capacity limit. (PubSub.unbounded)
    let unbounded () : Effect<PubSub<'A>, 'E, 'R> =
        Effect.sync (fun () -> makeUnsafe Unbounded System.Int32.MaxValue)

    // --- inspection ---

    /// The configured capacity. (PubSub.capacity)
    let capacity (self: PubSub<'A>) : int = self.Capacity

    /// The current size: the largest pending count across subscribers, or `0`
    /// when there are none or the hub is shut down. (PubSub.sizeUnsafe)
    let sizeUnsafe (self: PubSub<'A>) : int =
        lock self.Lock (fun () ->
            if self.IsShutdown || self.Subscribers.Count = 0 then
                0
            else
                self.Subscribers |> Seq.map (fun s -> s.Queue.Count) |> Seq.max)

    /// `sizeUnsafe` as an effect. (PubSub.size)
    let size (self: PubSub<'A>) : Effect<int, 'E, 'R> = Effect.sync (fun () -> sizeUnsafe self)

    // --- publishing ---

    /// Publish one message, fanning it to every subscriber. Suspends a `bounded`
    /// publisher until room exists. Succeeds with `false` once shut down.
    /// (PubSub.publish)
    let publish (self: PubSub<'A>) (value: 'A) : Effect<bool, 'E, 'R> =
        Effect(fun fib _ ->
            async {
                let mutable result = Success false
                let mutable go = true

                while go do
                    let action =
                        lock self.Lock (fun () ->
                            if self.IsShutdown then Choice1Of2 false
                            elif tryEnqueueUnsafe self value then Choice1Of2 true
                            else
                                let tcs = newGate ()
                                self.PublishWaiters.Add tcs
                                Choice2Of2 tcs)

                    match action with
                    | Choice1Of2 b ->
                        result <- Success b
                        go <- false
                    | Choice2Of2 tcs ->
                        do! Async.AwaitTask tcs.Task

                        if fib.Interrupted && fib.Mask = 0 then
                            result <- Failure interruptedCause
                            go <- false

                return result
            })

    /// Publish every message in order. Succeeds with `false` if the hub is
    /// already shut down, else `true` once all are published. (PubSub.publishAll)
    let publishAll (self: PubSub<'A>) (values: seq<'A>) : Effect<bool, 'E, 'R> =
        Effect.suspend (fun () ->
            if self.IsShutdown then
                Effect.succeed false
            else
                Effect.forEach values (fun v -> publish self v) |> Effect.map (fun _ -> true))

    // --- subscribing ---

    /// Subscribe for the lifetime of `scope`: the subscription is registered now
    /// and removed (waking any blocked taker) when the scope closes. Each
    /// subscription independently receives every message published after it.
    /// (PubSub.subscribe)
    let subscribe (scope: Scope<'E, 'R>) (self: PubSub<'A>) : Effect<Subscription<'A>, 'E, 'R> =
        Scope.acquireRelease
            scope
            (Effect.sync (fun () ->
                lock self.Lock (fun () ->
                    let sub =
                        { Parent = self
                          Queue = Queue<'A>()
                          Takers = List<TaskCompletionSource<unit>>() }

                    self.Subscribers.Add sub
                    sub)))
            (fun sub _ ->
                Effect.sync (fun () ->
                    lock self.Lock (fun () ->
                        self.Subscribers.Remove sub |> ignore
                        wakeTakers sub)))

    // --- taking ---

    /// Take one message, suspending until one is available. Completes with an
    /// interrupt if the hub is shut down while empty. (PubSub.take)
    let take (sub: Subscription<'A>) : Effect<'A, 'E, 'R> =
        let self = sub.Parent

        Effect(fun fib _ ->
            async {
                let mutable result = Unchecked.defaultof<Exit<'A, 'E>>
                let mutable go = true

                while go do
                    let action =
                        lock self.Lock (fun () ->
                            if sub.Queue.Count > 0 then
                                let v = sub.Queue.Dequeue()
                                wakePublishers self
                                Choice1Of3 v
                            elif self.IsShutdown then
                                Choice2Of3()
                            else
                                let tcs = newGate ()
                                sub.Takers.Add tcs
                                Choice3Of3 tcs)

                    match action with
                    | Choice1Of3 v ->
                        result <- Success v
                        go <- false
                    | Choice2Of3() ->
                        result <- Failure interruptedCause
                        go <- false
                    | Choice3Of3 tcs ->
                        do! Async.AwaitTask tcs.Task

                        if fib.Interrupted && fib.Mask = 0 then
                            result <- Failure interruptedCause
                            go <- false

                return result
            })

    /// Drain every currently-available message, suspending until at least one is
    /// available. Completes with an interrupt if shut down while empty.
    /// (PubSub.takeAll)
    let takeAll (sub: Subscription<'A>) : Effect<'A list, 'E, 'R> =
        let self = sub.Parent

        Effect(fun fib _ ->
            async {
                let mutable result = Unchecked.defaultof<Exit<'A list, 'E>>
                let mutable go = true

                while go do
                    let action =
                        lock self.Lock (fun () ->
                            if sub.Queue.Count > 0 then
                                let items = List.ofSeq sub.Queue
                                sub.Queue.Clear()
                                wakePublishers self
                                Choice1Of3 items
                            elif self.IsShutdown then
                                Choice2Of3()
                            else
                                let tcs = newGate ()
                                sub.Takers.Add tcs
                                Choice3Of3 tcs)

                    match action with
                    | Choice1Of3 xs ->
                        result <- Success xs
                        go <- false
                    | Choice2Of3() ->
                        result <- Failure interruptedCause
                        go <- false
                    | Choice3Of3 tcs ->
                        do! Async.AwaitTask tcs.Task

                        if fib.Interrupted && fib.Mask = 0 then
                            result <- Failure interruptedCause
                            go <- false

                return result
            })

    /// Take up to `max` currently-available messages without suspending; returns
    /// the empty list when none are buffered. (PubSub.takeUpTo)
    let takeUpTo (sub: Subscription<'A>) (max: int) : Effect<'A list, 'E, 'R> =
        let self = sub.Parent

        Effect.sync (fun () ->
            lock self.Lock (fun () ->
                let n = min max sub.Queue.Count

                [ for _ in 1..n ->
                      let v = sub.Queue.Dequeue()
                      wakePublishers self
                      v ]))

    /// The number of messages currently buffered for this subscription.
    /// (PubSub.remaining)
    let remaining (sub: Subscription<'A>) : Effect<int, 'E, 'R> =
        let self = sub.Parent
        Effect.sync (fun () -> lock self.Lock (fun () -> sub.Queue.Count))

    // --- shutdown ---

    /// Shut the hub down: future publishes succeed with `false`, and every
    /// suspended taker is interrupted. (PubSub.shutdown)
    let shutdown (self: PubSub<'A>) : Effect<unit, 'E, 'R> =
        Effect.sync (fun () ->
            lock self.Lock (fun () ->
                self.IsShutdown <- true

                for sub in self.Subscribers do
                    wakeTakers sub

                wakePublishers self))
