namespace Effect

/// Port of repos/effect-smol/packages/effect/src/SubscriptionRef.ts — a mutable
/// reference whose updates are serialized and published to subscribers as a
/// `Stream`. A `SubscriptionRef<'A>` keeps the latest value, and `changes` yields
/// the current value followed by every committed update.
///
/// Built on the merged `Semaphore` (serializes updates), `PubSub` (fans changes
/// out to subscribers) and `Stream`.
///
/// Porting adaptations vs upstream (noted per CONVENTIONS):
///   - This port's `PubSub` has no `replay: 1` buffer and no `publishUnsafe`, so
///     (a) `make` does not pre-publish the initial value, and (b) `changes` seeds
///     the current value itself: it subscribes AND reads the current value while
///     holding the update permit (so no update can interleave), emits that value,
///     then drains the subscription. Each subscriber owns a fresh `Scope` that is
///     closed (unsubscribing) when the stream terminates.
///   - `isSubscriptionRef` (a JS-runtime `unknown` guard) is omitted — dead weight
///     under F#'s static types.
///   - HKT variance plumbing dropped; ops take `self` first.
type SubscriptionRef<'A> =
    internal
        { mutable Value: 'A
          Semaphore: Semaphore
          PubSub: PubSub<'A> }

[<RequireQualifiedAccess>]
module SubscriptionRef =

    let private voidExit: Exit<obj, obj> = Success(box ())

    // --- constructor ---

    /// Constructs a `SubscriptionRef` from an initial value. (SubscriptionRef.make)
    let make (value: 'A) : Effect<SubscriptionRef<'A>, 'E, 'R> =
        PubSub.unbounded ()
        |> Effect.map (fun pubsub ->
            { Value = value
              Semaphore = Semaphore.makeUnsafe 1
              PubSub = pubsub })

    // --- getters ---

    /// Reads the current value synchronously. (SubscriptionRef.getUnsafe)
    let getUnsafe (self: SubscriptionRef<'A>) : 'A = self.Value

    /// Reads the current value. (SubscriptionRef.get)
    let get (self: SubscriptionRef<'A>) : Effect<'A, 'E, 'R> = Effect.sync (fun () -> self.Value)

    /// A stream of the current value followed by all subsequent changes.
    /// (SubscriptionRef.changes)
    let changes (self: SubscriptionRef<'A>) : Stream<'A, 'E, 'R> =
        { Run =
            fun emit ->
                Effect.suspend (fun () ->
                    let scope = Scope.make<'E, 'R> ()

                    // Subscribe and capture the current value atomically w.r.t.
                    // updates (under the permit), so no update is lost or doubled.
                    Semaphore.withPermit
                        self.Semaphore
                        (PubSub.subscribe scope self.PubSub
                         |> Effect.map (fun sub -> (self.Value, sub, scope))))
                |> Effect.flatMap (fun (current, sub, scope) ->
                    let rec loop () =
                        PubSub.take sub
                        |> Effect.flatMap (fun v -> emit v |> Effect.flatMap (fun () -> loop ()))

                    emit current
                    |> Effect.flatMap (fun () -> loop ())
                    |> Effect.ensuring (Scope.close scope voidExit)) }

    // --- internal modify cores (all publish on a committed change) ---

    let private publishNew (self: SubscriptionRef<'A>) (b: 'B) (newValue: 'A) : Effect<'B, 'E, 'R> =
        self.Value <- newValue
        PubSub.publish self.PubSub newValue |> Effect.map (fun _ -> b)

    let private modifyWith (self: SubscriptionRef<'A>) (f: 'A -> 'B * 'A) : Effect<'B, 'E, 'R> =
        Semaphore.withPermit
            self.Semaphore
            (Effect.suspend (fun () ->
                let (b, newValue) = f self.Value
                publishNew self b newValue))

    let private modifySomeWith (self: SubscriptionRef<'A>) (f: 'A -> 'B * 'A option) : Effect<'B, 'E, 'R> =
        Semaphore.withPermit
            self.Semaphore
            (Effect.suspend (fun () ->
                let (b, option) = f self.Value

                match option with
                | None -> Effect.succeed b
                | Some newValue -> publishNew self b newValue))

    let private modifyEffectWith (self: SubscriptionRef<'A>) (f: 'A -> Effect<'B * 'A, 'E, 'R>) : Effect<'B, 'E, 'R> =
        Semaphore.withPermit
            self.Semaphore
            (Effect.suspend (fun () -> f self.Value |> Effect.flatMap (fun (b, newValue) -> publishNew self b newValue)))

    let private modifySomeEffectWith
        (self: SubscriptionRef<'A>)
        (f: 'A -> Effect<'B * 'A option, 'E, 'R>)
        : Effect<'B, 'E, 'R> =
        Semaphore.withPermit
            self.Semaphore
            (Effect.suspend (fun () ->
                f self.Value
                |> Effect.flatMap (fun (b, option) ->
                    match option with
                    | None -> Effect.succeed b
                    | Some newValue -> publishNew self b newValue)))

    // --- setters / updates (pure) ---

    /// Sets a new value, notifying subscribers. (SubscriptionRef.set)
    let set (self: SubscriptionRef<'A>) (value: 'A) : Effect<unit, 'E, 'R> = modifyWith self (fun _ -> ((), value))

    /// Sets and returns the new value. (SubscriptionRef.setAndGet)
    let setAndGet (self: SubscriptionRef<'A>) (value: 'A) : Effect<'A, 'E, 'R> =
        modifyWith self (fun _ -> (value, value))

    /// Sets and returns the previous value. (SubscriptionRef.getAndSet)
    let getAndSet (self: SubscriptionRef<'A>) (value: 'A) : Effect<'A, 'E, 'R> =
        modifyWith self (fun current -> (current, value))

    /// Updates with `f`, notifying subscribers. (SubscriptionRef.update)
    let update (self: SubscriptionRef<'A>) (f: 'A -> 'A) : Effect<unit, 'E, 'R> =
        modifyWith self (fun current -> ((), f current))

    /// Updates and returns the new value. (SubscriptionRef.updateAndGet)
    let updateAndGet (self: SubscriptionRef<'A>) (f: 'A -> 'A) : Effect<'A, 'E, 'R> =
        modifyWith self (fun current -> let n = f current in (n, n))

    /// Updates and returns the previous value. (SubscriptionRef.getAndUpdate)
    let getAndUpdate (self: SubscriptionRef<'A>) (f: 'A -> 'A) : Effect<'A, 'E, 'R> =
        modifyWith self (fun current -> (current, f current))

    /// Computes `[result, newValue]`, stores newValue, returns result. (SubscriptionRef.modify)
    let modify (self: SubscriptionRef<'A>) (f: 'A -> 'B * 'A) : Effect<'B, 'E, 'R> = modifyWith self f

    // --- conditional updates (pure; publish only on Some) ---

    /// Conditional update, returning the previous value. (SubscriptionRef.getAndUpdateSome)
    let getAndUpdateSome (self: SubscriptionRef<'A>) (f: 'A -> 'A option) : Effect<'A, 'E, 'R> =
        modifySomeWith self (fun current -> (current, f current))

    /// Conditional update, no result. (SubscriptionRef.updateSome)
    let updateSome (self: SubscriptionRef<'A>) (f: 'A -> 'A option) : Effect<unit, 'E, 'R> =
        modifySomeWith self (fun current -> ((), f current))

    /// Conditional update, returning the resulting current value. (SubscriptionRef.updateSomeAndGet)
    let updateSomeAndGet (self: SubscriptionRef<'A>) (f: 'A -> 'A option) : Effect<'A, 'E, 'R> =
        modifySomeWith self (fun current ->
            match f current with
            | Some n -> (n, Some n)
            | None -> (current, None))

    /// Conditional modify with a separate result. (SubscriptionRef.modifySome)
    let modifySome (self: SubscriptionRef<'A>) (f: 'A -> 'B * 'A option) : Effect<'B, 'E, 'R> = modifySomeWith self f

    // --- effectful updates ---

    /// (SubscriptionRef.updateEffect)
    let updateEffect (self: SubscriptionRef<'A>) (f: 'A -> Effect<'A, 'E, 'R>) : Effect<unit, 'E, 'R> =
        modifyEffectWith self (fun current -> f current |> Effect.map (fun n -> ((), n)))

    /// (SubscriptionRef.updateAndGetEffect)
    let updateAndGetEffect (self: SubscriptionRef<'A>) (f: 'A -> Effect<'A, 'E, 'R>) : Effect<'A, 'E, 'R> =
        modifyEffectWith self (fun current -> f current |> Effect.map (fun n -> (n, n)))

    /// (SubscriptionRef.getAndUpdateEffect)
    let getAndUpdateEffect (self: SubscriptionRef<'A>) (f: 'A -> Effect<'A, 'E, 'R>) : Effect<'A, 'E, 'R> =
        modifyEffectWith self (fun current -> f current |> Effect.map (fun n -> (current, n)))

    /// (SubscriptionRef.modifyEffect)
    let modifyEffect (self: SubscriptionRef<'A>) (f: 'A -> Effect<'B * 'A, 'E, 'R>) : Effect<'B, 'E, 'R> =
        modifyEffectWith self f

    /// (SubscriptionRef.getAndUpdateSomeEffect)
    let getAndUpdateSomeEffect (self: SubscriptionRef<'A>) (f: 'A -> Effect<'A option, 'E, 'R>) : Effect<'A, 'E, 'R> =
        modifySomeEffectWith self (fun current -> f current |> Effect.map (fun o -> (current, o)))

    /// (SubscriptionRef.updateSomeEffect)
    let updateSomeEffect (self: SubscriptionRef<'A>) (f: 'A -> Effect<'A option, 'E, 'R>) : Effect<unit, 'E, 'R> =
        modifySomeEffectWith self (fun current -> f current |> Effect.map (fun o -> ((), o)))

    /// (SubscriptionRef.modifySomeEffect)
    let modifySomeEffect (self: SubscriptionRef<'A>) (f: 'A -> Effect<'B * 'A option, 'E, 'R>) : Effect<'B, 'E, 'R> =
        modifySomeEffectWith self f

    /// (SubscriptionRef.updateSomeAndGetEffect)
    let updateSomeAndGetEffect (self: SubscriptionRef<'A>) (f: 'A -> Effect<'A option, 'E, 'R>) : Effect<'A, 'E, 'R> =
        modifySomeEffectWith self (fun current ->
            f current
            |> Effect.map (fun option ->
                match option with
                | Some n -> (n, Some n)
                | None -> (current, None)))
