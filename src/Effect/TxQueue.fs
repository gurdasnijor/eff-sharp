namespace Effect

/// Port of repos/effect-smol/packages/effect/src/TxQueue.ts — a transactional
/// queue whose state changes participate in STM transactions. Built on `TxChunk`
/// (the item buffer) + `TxRef` (the lifecycle state), so offer/take compose with
/// other transactional operations and retry when they cannot proceed (taking
/// from an empty open queue, or offering to a full bounded queue).
///
/// Lifecycle (`State`): `Open` (accepting) → `Closing cause` (no new offers,
/// drain remaining) → `Done cause` (terminal; reads fail with the cause).
///
/// Strategies: `Bounded` (offer retries while full), `Unbounded`, `Dropping`
/// (offer returns false when full), `Sliding` (evict oldest when full).
///
/// Porting adaptations vs upstream (noted per CONVENTIONS):
///   - Our `Effect` core has no runtime transaction journal, so (as in the w3
///     STM port) the transactional logic lives in the `Stm` monad. Each public
///     op is its own atomic transaction via `TxRef.atomically`. The composable
///     `*Stm` cores are exposed so `TxPubSub` can broadcast to many subscriber
///     queues in ONE transaction. Composing several *public* ops into a single
///     transaction (upstream `Effect.tx`) needs Effect-runtime tx support not
///     present yet.
///   - The `TxEnqueue`/`TxDequeue` read/write split is purely a variance/typing
///     device (HKT) — collapsed into the single `TxQueue<'A,'E>` here.
///   - Skipped (noted): `Pull`-based streaming, `Cause.Done`/`end`/`ExcludeDone`
///     semantics (no `Done` cause in this port), `takeN`/`takeBetween`. `clear`
///     does not special-case a `Done` cause. `interrupt` uses fiber id `None`.

[<RequireQualifiedAccess>]
type TxQueueStrategy =
    | Bounded
    | Unbounded
    | Dropping
    | Sliding

/// Lifecycle state of a `TxQueue`. `Closing`/`Done` carry the completion cause.
type TxQueueState<'E> =
    | Open
    | Closing of Cause<'E>
    | Done of Cause<'E>

type TxQueue<'A, 'E> =
    internal
        { Strategy: TxQueueStrategy
          Capacity: int
          Items: TxChunk<'A>
          StateRef: TxRef<TxQueueState<'E>> }

[<RequireQualifiedAccess>]
module TxQueue =

    // --- constructors ---

    let private makeWith (strategy: TxQueueStrategy) (capacity: int) : Effect<TxQueue<'A, 'E>, 'Err, 'R> =
        effect {
            let! items = TxChunk.make Chunk.empty
            let! stateRef = TxRef.make (Open: TxQueueState<'E>)

            return
                { Strategy = strategy
                  Capacity = capacity
                  Items = items
                  StateRef = stateRef }
        }

    /// Bounded queue: offers retry (block) while full. (TxQueue.bounded)
    let bounded (capacity: int) : Effect<TxQueue<'A, 'E>, 'Err, 'R> = makeWith TxQueueStrategy.Bounded capacity

    /// Unbounded queue: offers always accept. (TxQueue.unbounded)
    let unbounded () : Effect<TxQueue<'A, 'E>, 'Err, 'R> =
        makeWith TxQueueStrategy.Unbounded System.Int32.MaxValue

    /// Dropping queue: offers return `false` when full. (TxQueue.dropping)
    let dropping (capacity: int) : Effect<TxQueue<'A, 'E>, 'Err, 'R> = makeWith TxQueueStrategy.Dropping capacity

    /// Sliding queue: offers evict the oldest item when full. (TxQueue.sliding)
    let sliding (capacity: int) : Effect<TxQueue<'A, 'E>, 'Err, 'R> = makeWith TxQueueStrategy.Sliding capacity

    // --- composable Stm cores (also used by TxPubSub) ---

    /// Current buffered size. (TxQueue.size, transactional core)
    let sizeStm (self: TxQueue<'A, 'E>) : Stm<int> = TxChunk.size self.Items

    /// Whether the buffer is empty. (TxQueue.isEmpty core)
    let isEmptyStm (self: TxQueue<'A, 'E>) : Stm<bool> = TxChunk.isEmpty self.Items

    /// Whether the buffer is at capacity. (TxQueue.isFull core)
    let isFullStm (self: TxQueue<'A, 'E>) : Stm<bool> =
        match self.Strategy with
        | TxQueueStrategy.Unbounded -> stm { return false }
        | _ -> stm { let! n = sizeStm self in return n >= self.Capacity }

    /// Offer a value, returning whether it was accepted. Bounded-full retries.
    /// (TxQueue.offer core)
    let offerStm (self: TxQueue<'A, 'E>) (value: 'A) : Stm<bool> =
        stm {
            let! state = TxRef.get self.StateRef

            match state with
            | Closing _
            | Done _ -> return false
            | Open ->
                let! currentSize = sizeStm self

                match self.Strategy with
                | TxQueueStrategy.Unbounded ->
                    do! TxChunk.append self.Items value
                    return true
                | _ ->
                    if currentSize < self.Capacity then
                        do! TxChunk.append self.Items value
                        return true
                    else
                        match self.Strategy with
                        | TxQueueStrategy.Dropping -> return false
                        | TxQueueStrategy.Sliding ->
                            do! TxChunk.update self.Items (Chunk.drop 1)
                            do! TxChunk.append self.Items value
                            return true
                        | _ -> return! TxRef.retry // bounded: block until space
        }

    /// Take the head, retrying while empty+open; `Error cause` when done.
    /// (TxQueue.take core)
    let takeStm (self: TxQueue<'A, 'E>) : Stm<Result<'A, Cause<'E>>> =
        stm {
            let! state = TxRef.get self.StateRef

            match state with
            | Done cause -> return Error cause
            | _ ->
                let! chunk = TxChunk.get self.Items

                match Chunk.get 0 chunk with
                | None -> return! TxRef.retry
                | Some head ->
                    do! TxChunk.update self.Items (Chunk.drop 1)
                    let! nowEmpty = isEmptyStm self

                    match state with
                    | Closing cause when nowEmpty -> do! TxRef.set self.StateRef (Done cause)
                    | _ -> ()

                    return Ok head
        }

    /// Non-blocking take. `None` if empty or done. (TxQueue.poll core)
    let pollStm (self: TxQueue<'A, 'E>) : Stm<'A option> =
        stm {
            let! state = TxRef.get self.StateRef

            match state with
            | Done _ -> return None
            | _ ->
                let! chunk = TxChunk.get self.Items

                match Chunk.get 0 chunk with
                | None -> return None
                | Some head ->
                    do! TxChunk.update self.Items (Chunk.drop 1)
                    return Some head
        }

    /// Take every buffered item, retrying while empty+open; `Error cause` if done.
    /// (TxQueue.takeAll core)
    let takeAllStm (self: TxQueue<'A, 'E>) : Stm<Result<'A[], Cause<'E>>> =
        stm {
            let! state = TxRef.get self.StateRef

            match state with
            | Done cause -> return Error cause
            | _ ->
                let! empty = isEmptyStm self

                if empty then
                    return! TxRef.retry
                else
                    let! chunk = TxChunk.get self.Items
                    do! TxChunk.set self.Items Chunk.empty

                    match state with
                    | Closing cause -> do! TxRef.set self.StateRef (Done cause)
                    | _ -> ()

                    return Ok(Chunk.toArray chunk)
        }

    /// Peek the head without removing it; retries while empty+open. (TxQueue.peek core)
    let peekStm (self: TxQueue<'A, 'E>) : Stm<Result<'A, Cause<'E>>> =
        stm {
            let! state = TxRef.get self.StateRef

            match state with
            | Done cause -> return Error cause
            | _ ->
                let! chunk = TxChunk.get self.Items

                match Chunk.get 0 chunk with
                | None -> return! TxRef.retry
                | Some head -> return Ok head
        }

    /// Complete the queue with a cause: empty → Done, otherwise → Closing.
    /// (TxQueue.failCause core)
    let failCauseStm (self: TxQueue<'A, 'E>) (cause: Cause<'E>) : Stm<bool> =
        stm {
            let! state = TxRef.get self.StateRef

            match state with
            | Open ->
                let! empty = isEmptyStm self
                do! TxRef.set self.StateRef (if empty then Done cause else Closing cause)
                return true
            | _ -> return false
        }

    /// Fail the queue, discarding buffered items, transitioning straight to Done.
    /// (TxQueue.fail core)
    let failStm (self: TxQueue<'A, 'E>) (error: 'E) : Stm<bool> =
        stm {
            let! state = TxRef.get self.StateRef

            match state with
            | Open ->
                do! TxChunk.set self.Items Chunk.empty
                do! TxRef.set self.StateRef (Done(Cause.fail error))
                return true
            | _ -> return false
        }

    /// Shut down: clear items and mark Done (with an interrupt cause).
    /// (TxQueue.shutdown core — simplified clear+interrupt)
    let shutdownStm (self: TxQueue<'A, 'E>) : Stm<bool> =
        stm {
            let! state = TxRef.get self.StateRef

            match state with
            | Done _ -> return false
            | _ ->
                do! TxChunk.set self.Items Chunk.empty
                do! TxRef.set self.StateRef (Done(Cause.interrupt None))
                return true
        }

    // --- Effect-returning public surface (each op is one atomic transaction) ---

    let private surface (result: Stm<Result<'A, Cause<'E>>>) : Effect<'A, 'E, 'R> =
        TxRef.atomically result
        |> Effect.flatMap (function
            | Ok a -> Effect.succeed a
            | Error cause -> Effect.failCause cause)

    /// Offer an item; `true` if accepted. (TxQueue.offer)
    let offer (self: TxQueue<'A, 'E>) (value: 'A) : Effect<bool, 'Err, 'R> =
        TxRef.atomically (offerStm self value)

    /// Offer every item in one transaction, returning those rejected.
    /// (TxQueue.offerAll) — `stm { }` has no `for`, so this folds by recursion.
    let offerAllStm (self: TxQueue<'A, 'E>) (values: seq<'A>) : Stm<'A list> =
        let rec loop remaining rejected =
            match remaining with
            | [] -> stm { return List.rev rejected }
            | value :: rest ->
                stm {
                    let! accepted = offerStm self value
                    return! loop rest (if accepted then rejected else value :: rejected)
                }

        loop (List.ofSeq values) []

    /// Offer many; returns the items that were rejected. (TxQueue.offerAll)
    let offerAll (self: TxQueue<'A, 'E>) (values: seq<'A>) : Effect<'A list, 'Err, 'R> =
        TxRef.atomically (offerAllStm self values)

    /// Take the head, blocking while empty; fails with the cause when done.
    /// (TxQueue.take)
    let take (self: TxQueue<'A, 'E>) : Effect<'A, 'E, 'R> = surface (takeStm self)

    /// Non-blocking take. (TxQueue.poll)
    let poll (self: TxQueue<'A, 'E>) : Effect<'A option, 'Err, 'R> = TxRef.atomically (pollStm self)

    /// Take all buffered items, blocking while empty. (TxQueue.takeAll)
    let takeAll (self: TxQueue<'A, 'E>) : Effect<'A[], 'E, 'R> = surface (takeAllStm self)

    /// Peek the head without removing it. (TxQueue.peek)
    let peek (self: TxQueue<'A, 'E>) : Effect<'A, 'E, 'R> = surface (peekStm self)

    /// Current size. (TxQueue.size)
    let size (self: TxQueue<'A, 'E>) : Effect<int, 'Err, 'R> = TxRef.atomically (sizeStm self)

    /// Whether empty. (TxQueue.isEmpty)
    let isEmpty (self: TxQueue<'A, 'E>) : Effect<bool, 'Err, 'R> = TxRef.atomically (isEmptyStm self)

    /// Whether at capacity. (TxQueue.isFull)
    let isFull (self: TxQueue<'A, 'E>) : Effect<bool, 'Err, 'R> = TxRef.atomically (isFullStm self)

    /// Fail the queue with an error. (TxQueue.fail)
    let fail (self: TxQueue<'A, 'E>) (error: 'E) : Effect<bool, 'Err, 'R> = TxRef.atomically (failStm self error)

    /// Complete the queue with a cause. (TxQueue.failCause)
    let failCause (self: TxQueue<'A, 'E>) (cause: Cause<'E>) : Effect<bool, 'Err, 'R> =
        TxRef.atomically (failCauseStm self cause)

    /// Interrupt the queue (completes with an interrupt cause). (TxQueue.interrupt)
    let interrupt (self: TxQueue<'A, 'E>) : Effect<bool, 'Err, 'R> =
        TxRef.atomically (failCauseStm self (Cause.interrupt None))

    /// Shut down: clear items and mark done. (TxQueue.shutdown)
    let shutdown (self: TxQueue<'A, 'E>) : Effect<bool, 'Err, 'R> = TxRef.atomically (shutdownStm self)

    // --- state predicates ---

    let private stateIs (self: TxQueue<'A, 'E>) (pred: TxQueueState<'E> -> bool) : Effect<bool, 'Err, 'R> =
        TxRef.atomically (stm { let! s = TxRef.get self.StateRef in return pred s })

    /// (TxQueue.isOpen)
    let isOpen (self: TxQueue<'A, 'E>) : Effect<bool, 'Err, 'R> =
        stateIs self (function Open -> true | _ -> false)

    /// (TxQueue.isClosing)
    let isClosing (self: TxQueue<'A, 'E>) : Effect<bool, 'Err, 'R> =
        stateIs self (function Closing _ -> true | _ -> false)

    /// (TxQueue.isDone)
    let isDone (self: TxQueue<'A, 'E>) : Effect<bool, 'Err, 'R> =
        stateIs self (function Done _ -> true | _ -> false)

    /// (TxQueue.isShutdown — alias of isDone)
    let isShutdown (self: TxQueue<'A, 'E>) : Effect<bool, 'Err, 'R> = isDone self

    /// Block until the queue is done. (TxQueue.awaitCompletion)
    let awaitCompletion (self: TxQueue<'A, 'E>) : Effect<unit, 'Err, 'R> =
        TxRef.atomically (
            stm {
                let! state = TxRef.get self.StateRef

                match state with
                | Done _ -> return ()
                | _ -> return! TxRef.retry
            }
        )
