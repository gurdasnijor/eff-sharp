namespace Effect

open System.Collections.Generic
open System.Threading.Tasks

/// Wave 4: `PartitionedSemaphore` — a shared permit pool whose waiters are grouped
/// by partition key.
///
/// Port of repos/effect-smol/packages/effect/src/PartitionedSemaphore.ts. Many
/// independent groups of work compete for one bounded permit pool; released
/// permits are handed to waiting partitions in round-robin order so no single
/// busy partition can monopolize them.
///
/// Representation — mutable counters (`Total` available, `Waiting` reserved by
/// blocked waiters) plus a `Dictionary` of partition key → FIFO waiter list,
/// guarded by a lock. A blocked `take` reserves the permits it still needs and
/// suspends on a `TaskCompletionSource`, resumed once `release` has routed enough
/// permits to it (same suspension pattern as the merged `Latch`/`Deferred`).
/// `MutableHashMap` keys the partitions upstream; here a plain `Dictionary` with
/// structural-equality keys serves the same role.
///
/// Omissions vs upstream (noted per CONVENTIONS): the non-finite ("unbounded")
/// permit case (JS `Infinity`) is dropped — F# `int` has no infinity; the
/// `PartitionedTypeId` brand and the dual/pipe currying are dropped (every
/// function takes `self` first). A request for more permits than `capacity`
/// suspends forever, as upstream (`resume(Effect.never)`).

type internal Waiter =
    { mutable Permits: int
      Tcs: TaskCompletionSource<unit> }

/// A partitioned permit pool. Mutable state is guarded by `Lock`.
type PartitionedSemaphore<'K when 'K: equality> =
    internal
        { Lock: obj
          Capacity: int
          mutable Total: int
          mutable Waiting: int
          Partitions: Dictionary<'K, LinkedList<Waiter>>
          KeyOrder: List<'K>
          mutable Rr: int }

[<RequireQualifiedAccess>]
module PartitionedSemaphore =

    let private newGate () =
        TaskCompletionSource<unit>(TaskCreationOptions.RunContinuationsAsynchronously)

    // --- construction ---

    /// Construct synchronously. Negative permit counts clamp to `0`.
    /// (PartitionedSemaphore.makeUnsafe)
    let makeUnsafe<'K when 'K: equality> (permits: int) : PartitionedSemaphore<'K> =
        let maxPermits = max 0 permits

        { Lock = obj ()
          Capacity = maxPermits
          Total = maxPermits
          Waiting = 0
          Partitions = Dictionary<'K, LinkedList<Waiter>>(HashIdentity.Structural)
          KeyOrder = List<'K>()
          Rr = 0 }

    /// Construct inside an `Effect`. (PartitionedSemaphore.make)
    ///
    /// No explicit `<'K>` type parameter: declaring one would stop F# from
    /// generalizing the Effect's phantom `'E`/`'R` channels (defaulting them to
    /// `obj`) and break `let! sem = PartitionedSemaphore.make n` in a typed
    /// `effect` block. Still callable explicitly as `make<string> n`.
    let make (permits: int) : Effect<PartitionedSemaphore<'K>, 'E, 'R> =
        Effect.sync (fun () -> makeUnsafe permits)

    // --- internal core (callers hold the lock) ---

    let private removeWaiterUnsafe (self: PartitionedSemaphore<'K>) (key: 'K) (waiter: Waiter) =
        match self.Partitions.TryGetValue key with
        | true, set ->
            set.Remove waiter |> ignore

            if set.Count = 0 then
                self.Partitions.Remove key |> ignore
                self.KeyOrder.Remove key |> ignore
        | _ -> ()

    /// Hand `permits` to waiting partitions round-robin (one permit per partition
    /// per cycle); any not needed by waiters raise `Total`, capped at capacity.
    /// Returns the resulting available count.
    let private releaseUnsafe (self: PartitionedSemaphore<'K>) (permits: int) : int =
        let mutable remaining = permits

        while remaining > 0 do
            if self.Waiting = 0 then
                self.Total <- min self.Capacity (self.Total + remaining)
                remaining <- 0
            elif self.KeyOrder.Count = 0 then
                remaining <- 0
            else
                if self.Rr >= self.KeyOrder.Count then self.Rr <- 0
                let key = self.KeyOrder.[self.Rr]
                let set = self.Partitions.[key]
                let waiter = set.First.Value
                waiter.Permits <- waiter.Permits - 1
                self.Waiting <- self.Waiting - 1

                if waiter.Permits = 0 then
                    removeWaiterUnsafe self key waiter
                    waiter.Tcs.TrySetResult() |> ignore

                self.Rr <- self.Rr + 1
                remaining <- remaining - 1

        self.Total

    let private tryTakeUnsafe (self: PartitionedSemaphore<'K>) (permits: int) : bool =
        if permits <= 0 then true
        elif self.Capacity < permits || self.Total < permits then false
        else
            self.Total <- self.Total - permits
            true

    // --- getters ---

    /// The fixed permit capacity. (PartitionedSemaphore.capacity)
    let capacity (self: PartitionedSemaphore<'K>) : int = self.Capacity

    /// The current number of free permits. (PartitionedSemaphore.available)
    let available (self: PartitionedSemaphore<'K>) : Effect<int, 'E, 'R> =
        Effect.sync (fun () -> lock self.Lock (fun () -> self.Total))

    // --- combinators ---

    /// Acquire `permits` for `key`, suspending until enough are routed to this
    /// partition. Zero/negative permits complete immediately; requesting more
    /// than capacity suspends forever. (PartitionedSemaphore.take)
    let take (self: PartitionedSemaphore<'K>) (key: 'K) (permits: int) : Effect<unit, 'E, 'R> =
        if permits <= 0 then
            Effect.succeed ()
        else
            Effect(fun _ _ ->
                async {
                    let action =
                        lock self.Lock (fun () ->
                            if self.Capacity < permits then
                                Choice1Of2(newGate ()) // never completes (mirrors Effect.never)
                            elif self.Total >= permits then
                                self.Total <- self.Total - permits
                                Choice2Of2 None
                            else
                                let needed = permits - self.Total
                                self.Total <- 0
                                self.Waiting <- self.Waiting + needed

                                let set =
                                    match self.Partitions.TryGetValue key with
                                    | true, s -> s
                                    | _ ->
                                        let s = LinkedList<Waiter>()
                                        self.Partitions.[key] <- s
                                        self.KeyOrder.Add key
                                        s

                                let waiter = { Permits = needed; Tcs = newGate () }
                                set.AddLast waiter |> ignore
                                Choice2Of2(Some waiter.Tcs))

                    match action with
                    | Choice1Of2 forever -> do! Async.AwaitTask forever.Task
                    | Choice2Of2 None -> ()
                    | Choice2Of2 (Some tcs) -> do! Async.AwaitTask tcs.Task

                    return Success()
                })

    /// Release `permits` to the shared pool (routed to waiters first), returning
    /// the resulting available count. (PartitionedSemaphore.release)
    let release (self: PartitionedSemaphore<'K>) (permits: int) : Effect<int, 'E, 'R> =
        Effect.sync (fun () -> lock self.Lock (fun () -> releaseUnsafe self permits))

    /// Run `effect` after acquiring `permits` for `key`, releasing them when it
    /// exits (success, failure, or interrupt). (PartitionedSemaphore.withPermits)
    let withPermits
        (self: PartitionedSemaphore<'K>)
        (key: 'K)
        (permits: int)
        (effect: Effect<'A, 'E, 'R>)
        : Effect<'A, 'E, 'R> =
        if permits <= 0 then
            effect
        else
            take self key permits
            |> Effect.flatMap (fun () ->
                Effect.ensuring (Effect.sync (fun () -> lock self.Lock (fun () -> releaseUnsafe self permits |> ignore))) effect)

    /// `withPermits` with a single permit. (PartitionedSemaphore.withPermit)
    let withPermit (self: PartitionedSemaphore<'K>) (key: 'K) (effect: Effect<'A, 'E, 'R>) : Effect<'A, 'E, 'R> =
        withPermits self key 1 effect

    /// Run `effect` only if `permits` can be acquired immediately, yielding
    /// `Some result`; otherwise `None` without running it. Permits are released
    /// when it exits. (PartitionedSemaphore.withPermitsIfAvailable)
    let withPermitsIfAvailable
        (self: PartitionedSemaphore<'K>)
        (permits: int)
        (effect: Effect<'A, 'E, 'R>)
        : Effect<'A option, 'E, 'R> =
        if permits <= 0 then
            effect |> Effect.map Some
        else
            Effect.suspend (fun () ->
                let took = lock self.Lock (fun () -> tryTakeUnsafe self permits)

                if not took then
                    Effect.succeed None
                else
                    Effect.ensuring
                        (Effect.sync (fun () -> lock self.Lock (fun () -> releaseUnsafe self permits |> ignore)))
                        (effect |> Effect.map Some))
