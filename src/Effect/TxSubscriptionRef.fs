namespace Effect

/// Port of repos/effect-smol/packages/effect/src/TxSubscriptionRef.ts — the STM
/// variant of `SubscriptionRef`. A `TxSubscriptionRef<'A>` pairs a `TxRef<'A>`
/// (current value) with a `TxPubSub<'A>` (committed updates). Subscribers first
/// receive the current value, then every later committed value.
///
/// Built on the merged `TxRef` + `TxPubSub` + `TxQueue` + `Stream`. Each mutation
/// reads the ref, writes the new value and publishes it in ONE transaction, so a
/// subscriber never observes a half-applied change.
///
/// Porting adaptations vs upstream (noted per CONVENTIONS):
///   - As in the w4 STM ports, transactional ops are `TxRef.atomically (stm { … })`
///     boundaries; the composable Stm cores of `TxRef`/`TxPubSub`/`TxQueue` are
///     reused so registration + seeding the current value is one transaction.
///   - `changes` returns the subscriber `TxQueue` directly (non-scoped; pair with
///     `unsubscribe`) rather than a `Scope`-bound resource — matching this port's
///     `TxPubSub.subscribe`. `changesStream` consumes it as a `Stream`.
///   - `isTxSubscriptionRef` (JS-runtime guard) is omitted.
type TxSubscriptionRef<'A> =
    internal
        { Ref: TxRef<'A>
          PubSub: TxPubSub<'A> }

[<RequireQualifiedAccess>]
module TxSubscriptionRef =

    /// Build a subscriber queue matching the hub's strategy/capacity (mirrors
    /// `TxPubSub`'s private `makeSubscriberQueue`).
    let private makeSubQueue (pubsub: TxPubSub<'A>) : Effect<TxQueue<'A, unit>, 'Err, 'R> =
        match pubsub.Strategy with
        | TxQueueStrategy.Bounded -> TxQueue.bounded pubsub.Capacity
        | TxQueueStrategy.Dropping -> TxQueue.dropping pubsub.Capacity
        | TxQueueStrategy.Sliding -> TxQueue.sliding pubsub.Capacity
        | TxQueueStrategy.Unbounded -> TxQueue.unbounded ()

    // --- constructor ---

    /// Creates a `TxSubscriptionRef` with the given initial value. (TxSubscriptionRef.make)
    let make (value: 'A) : Effect<TxSubscriptionRef<'A>, 'Err, 'R> =
        effect {
            let! ref = TxRef.make value
            let! pubsub = TxPubSub.unbounded ()
            return { Ref = ref; PubSub = pubsub }
        }

    // --- getters ---

    /// Reads the current value (composable). (TxSubscriptionRef.get core)
    let getStm (self: TxSubscriptionRef<'A>) : Stm<'A> = TxRef.get self.Ref

    /// Reads the current value. (TxSubscriptionRef.get)
    let get (self: TxSubscriptionRef<'A>) : Effect<'A, 'Err, 'R> = TxRef.atomically (getStm self)

    // --- mutations (read ref + write + publish, atomically) ---

    /// Modify the value, publishing the new value, returning a result (composable).
    /// (TxSubscriptionRef.modify core)
    let modifyStm (self: TxSubscriptionRef<'A>) (f: 'A -> 'B * 'A) : Stm<'B> =
        stm {
            let! current = TxRef.get self.Ref
            let (returnValue, newValue) = f current
            do! TxRef.set self.Ref newValue
            let! _ = TxPubSub.publishStm self.PubSub newValue
            return returnValue
        }

    /// (TxSubscriptionRef.modify)
    let modify (self: TxSubscriptionRef<'A>) (f: 'A -> 'B * 'A) : Effect<'B, 'Err, 'R> =
        TxRef.atomically (modifyStm self f)

    /// (TxSubscriptionRef.set)
    let set (self: TxSubscriptionRef<'A>) (value: 'A) : Effect<unit, 'Err, 'R> =
        modify self (fun _ -> ((), value))

    /// (TxSubscriptionRef.update)
    let update (self: TxSubscriptionRef<'A>) (f: 'A -> 'A) : Effect<unit, 'Err, 'R> =
        modify self (fun current -> ((), f current))

    /// (TxSubscriptionRef.getAndSet)
    let getAndSet (self: TxSubscriptionRef<'A>) (value: 'A) : Effect<'A, 'Err, 'R> =
        modify self (fun current -> (current, value))

    /// (TxSubscriptionRef.getAndUpdate)
    let getAndUpdate (self: TxSubscriptionRef<'A>) (f: 'A -> 'A) : Effect<'A, 'Err, 'R> =
        modify self (fun current -> (current, f current))

    /// (TxSubscriptionRef.updateAndGet)
    let updateAndGet (self: TxSubscriptionRef<'A>) (f: 'A -> 'A) : Effect<'A, 'Err, 'R> =
        modify self (fun current -> let n = f current in (n, n))

    // --- subscriptions ---

    /// Subscribe to all changes: registers a fresh subscriber queue AND seeds it
    /// with the current value in one transaction, so the queue yields the current
    /// value first, then every subsequent committed update. Pair with
    /// `unsubscribe`. (TxSubscriptionRef.changes)
    let changes (self: TxSubscriptionRef<'A>) : Effect<TxQueue<'A, unit>, 'Err, 'R> =
        effect {
            let! sub = makeSubQueue self.PubSub

            do!
                TxRef.atomically (
                    stm {
                        let! current = TxRef.get self.Ref
                        do! TxRef.update self.PubSub.SubscribersRef (fun subs -> subs @ [ sub ])
                        let! _ = TxQueue.offerStm sub current
                        return ()
                    }
                )

            return sub
        }

    /// Remove and shut down a subscriber queue from `changes`. (TxPubSub.releaseSubscriber)
    let unsubscribe (self: TxSubscriptionRef<'A>) (sub: TxQueue<'A, unit>) : Effect<unit, 'Err, 'R> =
        TxPubSub.releaseSubscriber self.PubSub sub

    /// A `Stream` of the current value followed by all subsequent updates.
    /// (TxSubscriptionRef.changesStream)
    let changesStream (self: TxSubscriptionRef<'A>) : Stream<'A, unit, 'R> =
        { Run =
            fun emit ->
                changes self
                |> Effect.flatMap (fun sub ->
                    let rec loop () =
                        TxQueue.take sub
                        |> Effect.flatMap (fun v -> emit v |> Effect.flatMap (fun () -> loop ()))

                    loop ()) }
