namespace Effect

open System.Collections.Generic

/// Port of repos/effect-smol/packages/effect/src/Queue.ts.
///
/// An asynchronous queue: producers `offer`, consumers `take`, with bounded /
/// unbounded capacity and `suspend` (backpressure) / `dropping` / `sliding`
/// overflow strategies. A queue can be `end`ed (normal completion), `fail`ed
/// (typed error), `interrupt`ed (graceful, drains first) or `shutdown` (drop
/// everything). Built on the merged `Deferred` (blocking waiters) over a
/// lock-guarded buffer — forks run on real tasks, so the state is synchronized.
///
/// Modeling choices (noted per CONVENTIONS.md):
///   * The error channel is `obj` (boxed), like `Pull`: ordinary errors and the
///     end-of-input completion marker share one channel and F# has no
///     `E | Done` union. `Queue<'A>` therefore takes a single type parameter;
///     completion is signaled with the `QueueDone` marker (detect with
///     `Queue.isDone`), and `fail` boxes the typed error.
///   * Upstream's structural `Enqueue`/`Dequeue` read/write views and the
///     `isQueue`/`isEnqueue`/`isDequeue` runtime guards are dropped — a single
///     `Queue` handle is both ends. `asEnqueue`/`asDequeue` are kept as identity.
///   * `into`/`takeBetween`/`isFull`/`*Unsafe` variants are omitted as deferred
///     surface; the tested core is complete.
///
/// Interruption note: a fiber blocked in `Deferred.await` (a real task await) is
/// not cancelled by the cooperative `Fiber.interrupt`, so the upstream
/// "offerAll can be interrupted" behavior is not reproduced.
/// Overflow behavior when a bounded queue is full.
type QueueStrategy =
    | Suspend
    | Dropping
    | Sliding

/// End-of-input completion marker (upstream `Cause.Done`). Detect with
/// `Queue.isDone`.
type QueueDone = private | QueueDone

type internal Offerer<'A> =
    { Items: ResizeArray<'A>
      Resume: Deferred<'A list, obj> }

/// An asynchronous queue handle (both producer and consumer end).
type Queue<'A> =
    internal
        { Buffer: ResizeArray<'A>
          Capacity: int
          Strategy: QueueStrategy
          mutable Terminal: Cause<obj> option
          Takers: ResizeArray<Deferred<unit, obj>>
          Offerers: ResizeArray<Offerer<'A>>
          Awaiters: ResizeArray<Deferred<unit, obj>>
          Lock: obj }

[<RequireQualifiedAccess>]
module Queue =

    /// Whether a boxed error is the end-of-input completion marker.
    let isDone (error: obj) : bool =
        match error with
        | :? QueueDone -> true
        | _ -> false

    let private isDoneCause (cause: Cause<obj>) : bool =
        cause.Reasons
        |> List.exists (function
            | Reason.Fail e -> isDone e
            | _ -> false)

    let private doneCause: Cause<obj> = { Reasons = [ Reason.Fail(box QueueDone) ] }
    let private interruptCause: Cause<obj> = { Reasons = [ Reason.Interrupt None ] }

    // --- internal helpers (all callers hold q.Lock) ---

    /// Wake every blocked taker and awaiter so they re-check the queue.
    let private signalAll (q: Queue<'A>) =
        for d in q.Takers do
            Deferred.doneUnsafe d (Effect.succeed ()) |> ignore

        q.Takers.Clear()

        for d in q.Awaiters do
            Deferred.doneUnsafe d (Effect.succeed ()) |> ignore

        q.Awaiters.Clear()

    /// Move suspended offers into freed buffer capacity, resuming any offerer
    /// that becomes fully admitted (its leftover is then empty).
    let private refill (q: Queue<'A>) =
        while q.Buffer.Count < q.Capacity && q.Offerers.Count > 0 do
            let off = q.Offerers.[0]
            q.Buffer.Add off.Items.[0]
            off.Items.RemoveAt 0

            if off.Items.Count = 0 then
                Deferred.doneUnsafe off.Resume (Effect.succeed []) |> ignore
                q.Offerers.RemoveAt 0

    /// Pull all remaining items from suspended offerers (rendezvous path, used
    /// when capacity is 0 or the buffer is empty), resuming each offerer.
    let private drainOfferers (q: Queue<'A>) : 'A list =
        let out = ResizeArray<'A>()

        for off in q.Offerers do
            out.AddRange off.Items
            Deferred.doneUnsafe off.Resume (Effect.succeed []) |> ignore

        q.Offerers.Clear()
        List.ofSeq out

    // --- constructors ---

    /// A queue with the given capacity and overflow strategy.
    let make (capacity: int) (strategy: QueueStrategy) : Effect<Queue<'A>, 'E, 'R> =
        Effect.sync (fun () ->
            { Buffer = ResizeArray<'A>()
              Capacity = max 0 capacity
              Strategy = strategy
              Terminal = None
              Takers = ResizeArray<Deferred<unit, obj>>()
              Offerers = ResizeArray<Offerer<'A>>()
              Awaiters = ResizeArray<Deferred<unit, obj>>()
              Lock = obj () })

    /// A bounded queue: producers block (backpressure) when full. (Queue.bounded)
    let bounded (capacity: int) : Effect<Queue<'A>, 'E, 'R> = make capacity Suspend

    /// An unbounded queue. (Queue.unbounded)
    let unbounded () : Effect<Queue<'A>, 'E, 'R> = make System.Int32.MaxValue Suspend

    /// A bounded queue that drops new offers when full. (Queue.dropping)
    let dropping (capacity: int) : Effect<Queue<'A>, 'E, 'R> = make capacity Dropping

    /// A bounded queue that evicts the oldest item when full. (Queue.sliding)
    let sliding (capacity: int) : Effect<Queue<'A>, 'E, 'R> = make capacity Sliding

    /// The same handle viewed as a producer (identity). (Queue.asEnqueue)
    let asEnqueue (q: Queue<'A>) : Queue<'A> = q

    /// The same handle viewed as a consumer (identity). (Queue.asDequeue)
    let asDequeue (q: Queue<'A>) : Queue<'A> = q

    // --- offering ---

    /// Offer a single message. Returns `true` if it was accepted. Suspends until
    /// there is room (suspend strategy), drops/evicts (dropping/sliding), or
    /// returns `false` once the queue has completed. (Queue.offer)
    let offer (q: Queue<'A>) (message: 'A) : Effect<bool, obj, 'R> =
        Effect.suspend (fun () ->
            let outcome =
                lock q.Lock (fun () ->
                    match q.Terminal with
                    | Some _ -> Choice1Of2 false
                    | None ->
                        match q.Strategy with
                        | Suspend ->
                            if q.Buffer.Count < q.Capacity then
                                q.Buffer.Add message
                                signalAll q
                                Choice1Of2 true
                            else
                                let d = Deferred.makeUnsafe<'A list, obj> ()

                                q.Offerers.Add
                                    { Items = ResizeArray<'A>([ message ])
                                      Resume = d }

                                signalAll q
                                Choice2Of2 d
                        | Dropping ->
                            if q.Buffer.Count < q.Capacity then
                                q.Buffer.Add message
                                signalAll q
                                Choice1Of2 true
                            else
                                Choice1Of2 false
                        | Sliding ->
                            if q.Capacity > 0 then
                                if q.Buffer.Count >= q.Capacity then
                                    q.Buffer.RemoveAt 0

                                q.Buffer.Add message
                                signalAll q

                            Choice1Of2 true)

            match outcome with
            | Choice1Of2 b -> Effect.succeed b
            | Choice2Of2 d -> Deferred.await d |> Effect.map List.isEmpty)

    /// Offer many messages. Returns the messages that were **not** accepted
    /// (empty for suspend once fully admitted and for sliding; the overflow for
    /// dropping). (Queue.offerAll)
    let offerAll (q: Queue<'A>) (messages: 'A list) : Effect<'A list, obj, 'R> =
        Effect.suspend (fun () ->
            let outcome =
                lock q.Lock (fun () ->
                    match q.Terminal with
                    | Some _ -> Choice1Of2 messages
                    | None ->
                        match q.Strategy with
                        | Suspend ->
                            let mutable rest = messages

                            while q.Buffer.Count < q.Capacity && not (List.isEmpty rest) do
                                q.Buffer.Add(List.head rest)
                                rest <- List.tail rest

                            signalAll q

                            if List.isEmpty rest then
                                Choice1Of2 []
                            else
                                let d = Deferred.makeUnsafe<'A list, obj> ()

                                q.Offerers.Add
                                    { Items = ResizeArray<'A>(rest)
                                      Resume = d }

                                Choice2Of2 d
                        | Dropping ->
                            let mutable rest = messages

                            while q.Buffer.Count < q.Capacity && not (List.isEmpty rest) do
                                q.Buffer.Add(List.head rest)
                                rest <- List.tail rest

                            signalAll q
                            Choice1Of2 rest
                        | Sliding ->
                            if q.Capacity > 0 then
                                for m in messages do
                                    if q.Buffer.Count >= q.Capacity then
                                        q.Buffer.RemoveAt 0

                                    q.Buffer.Add m

                                signalAll q

                            Choice1Of2 [])

            match outcome with
            | Choice1Of2 leftover -> Effect.succeed leftover
            | Choice2Of2 d -> Deferred.await d)

    // --- completion ---

    let private complete (q: Queue<'A>) (cause: Cause<obj>) (clearBuffer: bool) : bool =
        lock q.Lock (fun () ->
            match q.Terminal with
            | Some _ -> false
            | None ->
                q.Terminal <- Some cause

                if clearBuffer then
                    q.Buffer.Clear()

                for off in q.Offerers do
                    Deferred.doneUnsafe off.Resume (Effect.succeed (List.ofSeq off.Items)) |> ignore

                q.Offerers.Clear()
                signalAll q
                true)

    /// Signal normal end-of-input. Pending/future takes drain the buffer, then
    /// fail with the `Done` marker. (Queue.end)
    let endQueue (q: Queue<'A>) : Effect<bool, 'F, 'R> =
        Effect.sync (fun () -> complete q doneCause false)

    /// Fail the queue with a (boxed) error. Buffered values are drained before
    /// takers see the failure. (Queue.fail / failCause)
    let fail (q: Queue<'A>) (error: obj) : Effect<bool, 'F, 'R> =
        Effect.sync (fun () -> complete q { Reasons = [ Reason.Fail error ] } false)

    /// Gracefully interrupt: no new offers, but buffered values may still be
    /// drained; once empty, takes fail with interruption. (Queue.interrupt)
    let interrupt (q: Queue<'A>) : Effect<bool, 'F, 'R> =
        Effect.sync (fun () -> complete q interruptCause false)

    /// Shut down immediately: drop all buffered values and fail everything with
    /// interruption. (Queue.shutdown)
    let shutdown (q: Queue<'A>) : Effect<bool, 'F, 'R> =
        Effect.sync (fun () -> complete q interruptCause true)

    // --- taking ---

    /// Take one message, suspending until one is available, or failing with the
    /// queue's completion. (Queue.take)
    let rec take (q: Queue<'A>) : Effect<'A, obj, 'R> =
        Effect.suspend (fun () ->
            let outcome =
                lock q.Lock (fun () ->
                    if q.Buffer.Count > 0 then
                        let v = q.Buffer.[0]
                        q.Buffer.RemoveAt 0
                        refill q
                        signalAll q
                        Choice1Of3 v
                    elif q.Offerers.Count > 0 then
                        let off = q.Offerers.[0]
                        let v = off.Items.[0]
                        off.Items.RemoveAt 0

                        if off.Items.Count = 0 then
                            Deferred.doneUnsafe off.Resume (Effect.succeed []) |> ignore
                            q.Offerers.RemoveAt 0

                        Choice1Of3 v
                    else
                        match q.Terminal with
                        | Some c -> Choice2Of3 c
                        | None ->
                            let d = Deferred.makeUnsafe<unit, obj> ()
                            q.Takers.Add d
                            Choice3Of3 d)

            match outcome with
            | Choice1Of3 v -> Effect.succeed v
            | Choice2Of3 cause -> Effect.failCause cause
            | Choice3Of3 d -> Deferred.await d |> Effect.flatMap (fun () -> take q))

    /// Take all currently available messages (at least one — suspends until then
    /// or until completion). (Queue.takeAll)
    let rec takeAll (q: Queue<'A>) : Effect<'A list, obj, 'R> =
        Effect.suspend (fun () ->
            let outcome =
                lock q.Lock (fun () ->
                    if q.Buffer.Count > 0 then
                        let items = List.ofSeq q.Buffer
                        q.Buffer.Clear()
                        refill q
                        signalAll q
                        Choice1Of3 items
                    elif q.Offerers.Count > 0 then
                        Choice1Of3(drainOfferers q)
                    else
                        match q.Terminal with
                        | Some c -> Choice2Of3 c
                        | None ->
                            let d = Deferred.makeUnsafe<unit, obj> ()
                            q.Takers.Add d
                            Choice3Of3 d)

            match outcome with
            | Choice1Of3 items -> Effect.succeed items
            | Choice2Of3 cause -> Effect.failCause cause
            | Choice3Of3 d -> Deferred.await d |> Effect.flatMap (fun () -> takeAll q))

    /// Take exactly `n` messages (suspending across refills). (Queue.takeN)
    let takeN (q: Queue<'A>) (n: int) : Effect<'A list, obj, 'R> =
        let rec loop (acc: 'A list) (remaining: int) : Effect<'A list, obj, 'R> =
            if remaining <= 0 then
                Effect.succeed (List.rev acc)
            else
                take q |> Effect.flatMap (fun v -> loop (v :: acc) (remaining - 1))

        loop [] n

    /// Non-blocking take: `Some head` or `None` when empty. (Queue.poll)
    let poll (q: Queue<'A>) : Effect<'A option, obj, 'R> =
        Effect.sync (fun () ->
            lock q.Lock (fun () ->
                if q.Buffer.Count > 0 then
                    let v = q.Buffer.[0]
                    q.Buffer.RemoveAt 0
                    refill q
                    signalAll q
                    Some v
                elif q.Offerers.Count > 0 then
                    let off = q.Offerers.[0]
                    let v = off.Items.[0]
                    off.Items.RemoveAt 0

                    if off.Items.Count = 0 then
                        Deferred.doneUnsafe off.Resume (Effect.succeed []) |> ignore
                        q.Offerers.RemoveAt 0

                    Some v
                else
                    None))

    /// This queue as a `Stream`: emit each taken value until the queue is shut
    /// down / interrupted and drained — at which point `take` fails (terminal
    /// cause) and the stream ends. The eff-sharp analogue of `Stream.fromQueue`;
    /// it lives here because compile order puts `Queue` after `Stream`. Composes
    /// `take` through `Effect.exit` (a terminal failure becomes end-of-stream).
    /// (Stream.fromQueue)
    let toStream (q: Queue<'A>) : Stream<'A, 'E, 'R> =
        Stream.repeatEffectOption (
            take q
            |> Effect.exit
            |> Effect.map (function
                | Success a -> Some a
                | Failure _ -> None)
        )

    /// View the next message without removing it (suspends until available).
    /// (Queue.peek)
    let rec peek (q: Queue<'A>) : Effect<'A, obj, 'R> =
        Effect.suspend (fun () ->
            let outcome =
                lock q.Lock (fun () ->
                    if q.Buffer.Count > 0 then
                        Choice1Of3 q.Buffer.[0]
                    elif q.Offerers.Count > 0 then
                        Choice1Of3 q.Offerers.[0].Items.[0]
                    else
                        match q.Terminal with
                        | Some c -> Choice2Of3 c
                        | None ->
                            let d = Deferred.makeUnsafe<unit, obj> ()
                            q.Takers.Add d
                            Choice3Of3 d)

            match outcome with
            | Choice1Of3 v -> Effect.succeed v
            | Choice2Of3 cause -> Effect.failCause cause
            | Choice3Of3 d -> Deferred.await d |> Effect.flatMap (fun () -> peek q))

    /// Drain all messages until end-of-input, collecting the values (the `Done`
    /// completion stops collection; other failures propagate). (Queue.collect)
    let collect (q: Queue<'A>) : Effect<'A list, obj, 'R> =
        let rec loop (acc: 'A list) : Effect<'A list, obj, 'R> =
            takeAll q
            |> Effect.flatMap (fun batch -> loop (acc @ batch))
            |> Effect.catchAll (fun (err: obj) -> if isDone err then Effect.succeed acc else Effect.fail err)

        loop []

    /// The number of buffered messages. (Queue.size)
    let size (q: Queue<'A>) : Effect<int, 'F, 'R> =
        Effect.sync (fun () -> lock q.Lock (fun () -> q.Buffer.Count))

    /// Wait for the queue to complete with no remaining buffered messages.
    /// Succeeds on normal end; fails with the error/interruption otherwise.
    /// (Queue.await)
    let rec await (q: Queue<'A>) : Effect<unit, obj, 'R> =
        Effect.suspend (fun () ->
            let outcome =
                lock q.Lock (fun () ->
                    match q.Terminal with
                    | Some c when q.Buffer.Count = 0 && q.Offerers.Count = 0 ->
                        if isDoneCause c then
                            Choice1Of2(Effect.succeed ())
                        else
                            Choice1Of2(Effect.failCause c)
                    | _ ->
                        let d = Deferred.makeUnsafe<unit, obj> ()
                        q.Awaiters.Add d
                        Choice2Of2 d)

            match outcome with
            | Choice1Of2 eff -> eff
            | Choice2Of2 d -> Deferred.await d |> Effect.flatMap (fun () -> await q))
