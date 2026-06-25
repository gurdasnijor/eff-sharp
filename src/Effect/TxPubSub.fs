namespace Effect

/// Port of repos/effect-smol/packages/effect/src/TxPubSub.ts — a transactional
/// publish/subscribe hub. Each subscriber owns a `TxQueue`; a published value is
/// offered, in ONE transaction, to every subscriber queue registered at the time
/// of publication. Built on `TxQueue` + `TxRef`.
///
/// The atomic broadcast is what makes `TxQueue`'s composable `*Stm` cores
/// necessary: `publish` folds `TxQueue.offerStm` over the subscriber list inside
/// a single `stm { }`, so all subscribers receive the value together (or the
/// transaction retries, e.g. when a bounded subscriber queue is full).
///
/// Subscriber queues carry no error channel, modelled here as `TxQueue<'A,unit>`
/// (upstream `E = never`).
///
/// Porting adaptations vs upstream (noted): `subscribe` is a plain
/// acquire returning the queue plus an explicit `unsubscribe`, rather than a
/// `Scope`-based acquire/release (scoped resource wiring is a later upgrade);
/// `acquireSubscriber`/`releaseSubscriber` are the composable cores.
type TxPubSub<'A> =
    internal
        { SubscribersRef: TxRef<TxQueue<'A, unit> list>
          ShutdownRef: TxRef<bool>
          Strategy: TxQueueStrategy
          Capacity: int }

[<RequireQualifiedAccess>]
module TxPubSub =

    let private makeSubscriberQueue (strategy: TxQueueStrategy) (capacity: int) : Effect<TxQueue<'A, unit>, 'Err, 'R> =
        match strategy with
        | TxQueueStrategy.Bounded -> TxQueue.bounded capacity
        | TxQueueStrategy.Dropping -> TxQueue.dropping capacity
        | TxQueueStrategy.Sliding -> TxQueue.sliding capacity
        | TxQueueStrategy.Unbounded -> TxQueue.unbounded ()

    let private makeWith (strategy: TxQueueStrategy) (capacity: int) : Effect<TxPubSub<'A>, 'Err, 'R> =
        effect {
            let! subscribersRef = TxRef.make ([]: TxQueue<'A, unit> list)
            let! shutdownRef = TxRef.make false

            return
                { SubscribersRef = subscribersRef
                  ShutdownRef = shutdownRef
                  Strategy = strategy
                  Capacity = capacity }
        }

    // --- constructors ---

    /// Bounded hub: publishers retry while any subscriber queue is full. (TxPubSub.bounded)
    let bounded (capacity: int) : Effect<TxPubSub<'A>, 'Err, 'R> =
        makeWith TxQueueStrategy.Bounded capacity

    /// Dropping hub: full subscriber queues drop the new message. (TxPubSub.dropping)
    let dropping (capacity: int) : Effect<TxPubSub<'A>, 'Err, 'R> =
        makeWith TxQueueStrategy.Dropping capacity

    /// Sliding hub: full subscriber queues evict their oldest message. (TxPubSub.sliding)
    let sliding (capacity: int) : Effect<TxPubSub<'A>, 'Err, 'R> =
        makeWith TxQueueStrategy.Sliding capacity

    /// Unbounded hub: messages are always accepted. (TxPubSub.unbounded)
    let unbounded () : Effect<TxPubSub<'A>, 'Err, 'R> =
        makeWith TxQueueStrategy.Unbounded System.Int32.MaxValue

    // --- getters ---

    /// Configured capacity. (TxPubSub.capacity)
    let capacity (self: TxPubSub<'A>) : int = self.Capacity

    /// Whether the hub has been shut down. (TxPubSub.isShutdown)
    let isShutdown (self: TxPubSub<'A>) : Effect<bool, 'Err, 'R> =
        TxRef.atomically (TxRef.get self.ShutdownRef)

    /// Max buffered size across subscriber queues. (TxPubSub.size)
    let size (self: TxPubSub<'A>) : Effect<int, 'Err, 'R> =
        let rec maxLoop subs acc =
            match subs with
            | [] -> stm { return acc }
            | q :: rest ->
                stm {
                    let! n = TxQueue.sizeStm q
                    return! maxLoop rest (max acc n)
                }

        TxRef.atomically (
            stm {
                let! subs = TxRef.get self.SubscribersRef
                return! maxLoop subs 0
            }
        )

    /// Whether all subscriber queues are empty. (TxPubSub.isEmpty)
    let isEmpty (self: TxPubSub<'A>) : Effect<bool, 'Err, 'R> =
        size self |> Effect.map (fun s -> s = 0)

    /// Whether any subscriber queue is full. (TxPubSub.isFull)
    let isFull (self: TxPubSub<'A>) : Effect<bool, 'Err, 'R> =
        match self.Strategy with
        | TxQueueStrategy.Unbounded -> Effect.succeed false
        | _ ->
            let rec anyFull subs =
                match subs with
                | [] -> stm { return false }
                | q :: rest ->
                    stm {
                        let! full = TxQueue.isFullStm q
                        if full then return true else return! anyFull rest
                    }

            TxRef.atomically (
                stm {
                    let! subs = TxRef.get self.SubscribersRef
                    return! anyFull subs
                }
            )

    // --- mutations ---

    /// Publish a value to all current subscribers atomically. `true` if every
    /// subscriber accepted it; `false` if shut down or any dropped it. (TxPubSub.publish)
    let publishStm (self: TxPubSub<'A>) (value: 'A) : Stm<bool> =
        let rec offerLoop subs allAccepted =
            match subs with
            | [] -> stm { return allAccepted }
            | q :: rest ->
                stm {
                    let! accepted = TxQueue.offerStm q value
                    return! offerLoop rest (allAccepted && accepted)
                }

        stm {
            let! shut = TxRef.get self.ShutdownRef

            if shut then
                return false
            else
                let! subs = TxRef.get self.SubscribersRef
                return! offerLoop subs true
        }

    /// (TxPubSub.publish)
    let publish (self: TxPubSub<'A>) (value: 'A) : Effect<bool, 'Err, 'R> =
        TxRef.atomically (publishStm self value)

    /// Publish every item from a sequence. (TxPubSub.publishAll)
    let publishAll (self: TxPubSub<'A>) (values: seq<'A>) : Effect<bool, 'Err, 'R> =
        let rec loop remaining allAccepted =
            match remaining with
            | [] -> stm { return allAccepted }
            | v :: rest ->
                stm {
                    let! accepted = publishStm self v
                    return! loop rest (allAccepted && accepted)
                }

        TxRef.atomically (loop (List.ofSeq values) true)

    /// Create a subscriber queue and register it (composable core). (TxPubSub.acquireSubscriber)
    let acquireSubscriber (self: TxPubSub<'A>) : Effect<TxQueue<'A, unit>, 'Err, 'R> =
        effect {
            let! queue = makeSubscriberQueue self.Strategy self.Capacity
            do! TxRef.atomically (TxRef.update self.SubscribersRef (fun subs -> subs @ [ queue ]))
            return queue
        }

    /// Remove a subscriber queue and shut it down. (TxPubSub.releaseSubscriber)
    let releaseSubscriber (self: TxPubSub<'A>) (queue: TxQueue<'A, unit>) : Effect<unit, 'Err, 'R> =
        TxRef.atomically (
            stm {
                do!
                    TxRef.update
                        self.SubscribersRef
                        (List.filter (fun q -> not (System.Object.ReferenceEquals(q, queue))))

                let! _ = TxQueue.shutdownStm queue
                return ()
            }
        )

    /// Subscribe, returning a fresh subscriber queue. Pair with `unsubscribe`.
    /// (TxPubSub.subscribe — non-scoped v1)
    let subscribe (self: TxPubSub<'A>) : Effect<TxQueue<'A, unit>, 'Err, 'R> = acquireSubscriber self

    /// Unsubscribe a queue obtained from `subscribe`. (alias of releaseSubscriber)
    let unsubscribe (self: TxPubSub<'A>) (queue: TxQueue<'A, unit>) : Effect<unit, 'Err, 'R> =
        releaseSubscriber self queue

    /// Shut down the hub and all currently-registered subscriber queues. (TxPubSub.shutdown)
    let shutdown (self: TxPubSub<'A>) : Effect<unit, 'Err, 'R> =
        let rec shutAll subs =
            match subs with
            | [] -> stm { return () }
            | q :: rest ->
                stm {
                    let! _ = TxQueue.shutdownStm q
                    return! shutAll rest
                }

        TxRef.atomically (
            stm {
                let! already = TxRef.get self.ShutdownRef

                if already then
                    return ()
                else
                    do! TxRef.set self.ShutdownRef true
                    let! subs = TxRef.get self.SubscribersRef
                    return! shutAll subs
            }
        )

    /// Block until the hub is shut down. (TxPubSub.awaitShutdown)
    let awaitShutdown (self: TxPubSub<'A>) : Effect<unit, 'Err, 'R> =
        TxRef.atomically (
            stm {
                let! shut = TxRef.get self.ShutdownRef
                if shut then return () else return! TxRef.retry
            }
        )
