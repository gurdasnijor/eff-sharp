namespace Effect

open System.Threading

/// Port of repos/effect-smol/packages/effect/src/SynchronizedRef.ts — mutable
/// state whose update/modify operations are serialized so each transition sees a
/// consistent current value, including effectful (`...Effect`) transitions.
///
/// Decoupling (per the w2-ref brief): upstream serializes with the `Semaphore`
/// module, which is NOT yet ported. We instead use a .NET
/// `System.Threading.SemaphoreSlim(1, 1)` for effectful mutual exclusion,
/// acquired/released around the protected effect (`withPermit`). No dependency
/// on a `Semaphore` module is introduced. Reads (`get`/`getUnsafe`) bypass the
/// permit, matching upstream.
///
/// Omissions vs upstream (noted per CONVENTIONS):
///   - HKT/variance plumbing — F# has no HKTs.
///   - JS-runtime machinery: `Proto`, `PipeInspectableProto`, `toJSON`, the
///     `pipe`/`dual` data-last currying. Every function takes `self` first.
///   - `Option` is F#'s native `option`; `Option.isNone` lowers into `match`.
///   - The unused `fallback` parameter on upstream `modifySomeEffect`'s
///     data-last overload is dropped (its implementation ignores it).
type SynchronizedRef<'A> =
    internal
        { Backing: Ref<'A>
          Semaphore: SemaphoreSlim }

[<RequireQualifiedAccess>]
module SynchronizedRef =

    /// Upstream brand constant (kept as a literal for parity).
    [<Literal>]
    let TypeId = "~effect/SynchronizedRef"

    // --- internal: effectful mutual exclusion ---

    /// Runs `eff` while holding the ref's single permit, releasing it afterwards
    /// (even on failure). The .NET stand-in for upstream `semaphore.withPermit`.
    let private withPermit (sem: SemaphoreSlim) (Effect run: Effect<'A, 'E, 'R>) : Effect<'A, 'E, 'R> =
        Effect(fun r ->
            async {
                do! sem.WaitAsync() |> Async.AwaitTask

                try
                    return! run r
                finally
                    sem.Release() |> ignore
            })

    /// Stores a value into the backing cell (deferred until run). Centralizes the
    /// one place we reach past `Ref`'s surface into its cell.
    let private setBacking (self: SynchronizedRef<'A>) (value: 'A) : unit = self.Backing.Cell.Current <- value

    // --- constructors ---

    /// Creates a `SynchronizedRef` holding `value`, outside the `Effect` context.
    /// (SynchronizedRef.makeUnsafe)
    let makeUnsafe (value: 'A) : SynchronizedRef<'A> =
        { Backing = Ref.makeUnsafe value
          Semaphore = new SemaphoreSlim(1, 1) }

    /// Creates a `SynchronizedRef` holding `value`, wrapped in an `Effect`.
    /// (SynchronizedRef.make)
    let make (value: 'A) : Effect<SynchronizedRef<'A>, 'E, 'R> = Effect.sync (fun () -> makeUnsafe value)

    // --- getters (bypass the permit, like upstream) ---

    /// Reads the current value, outside the `Effect` context and bypassing the
    /// permit. (SynchronizedRef.getUnsafe)
    let getUnsafe (self: SynchronizedRef<'A>) : 'A = Ref.getUnsafe self.Backing

    /// Reads the current value. (SynchronizedRef.get)
    let get (self: SynchronizedRef<'A>) : Effect<'A, 'E, 'R> = Effect.sync (fun () -> getUnsafe self)

    // --- pure mutations (serialized; delegate to Ref under the permit) ---

    /// Replaces the value, returning the previous one. (SynchronizedRef.getAndSet)
    let getAndSet (self: SynchronizedRef<'A>) (value: 'A) : Effect<'A, 'E, 'R> =
        withPermit self.Semaphore (Ref.getAndSet self.Backing value)

    /// Updates the value with `f`, returning the previous one.
    /// (SynchronizedRef.getAndUpdate)
    let getAndUpdate (self: SynchronizedRef<'A>) (f: 'A -> 'A) : Effect<'A, 'E, 'R> =
        withPermit self.Semaphore (Ref.getAndUpdate self.Backing f)

    /// Conditionally updates the value, returning the previous one.
    /// (SynchronizedRef.getAndUpdateSome)
    let getAndUpdateSome (self: SynchronizedRef<'A>) (pf: 'A -> 'A option) : Effect<'A, 'E, 'R> =
        withPermit self.Semaphore (Ref.getAndUpdateSome self.Backing pf)

    /// Computes `[result, newValue]`, stores `newValue`, returns `result`.
    /// (SynchronizedRef.modify)
    let modify (self: SynchronizedRef<'A>) (f: 'A -> 'B * 'A) : Effect<'B, 'E, 'R> =
        withPermit self.Semaphore (Ref.modify self.Backing f)

    /// Computes `[result, option]`; `Some` stores, `None` leaves unchanged.
    /// (SynchronizedRef.modifySome)
    let modifySome (self: SynchronizedRef<'A>) (pf: 'A -> 'B * 'A option) : Effect<'B, 'E, 'R> =
        withPermit self.Semaphore (Ref.modifySome self.Backing pf)

    /// Replaces the value. (SynchronizedRef.set)
    let set (self: SynchronizedRef<'A>) (value: 'A) : Effect<unit, 'E, 'R> =
        withPermit self.Semaphore (Ref.set self.Backing value)

    /// Replaces the value, returning the new one. (SynchronizedRef.setAndGet)
    let setAndGet (self: SynchronizedRef<'A>) (value: 'A) : Effect<'A, 'E, 'R> =
        withPermit self.Semaphore (Ref.setAndGet self.Backing value)

    /// Updates the value with `f`. (SynchronizedRef.update)
    let update (self: SynchronizedRef<'A>) (f: 'A -> 'A) : Effect<unit, 'E, 'R> =
        withPermit self.Semaphore (Ref.update self.Backing f)

    /// Updates the value with `f`, returning the new one.
    /// (SynchronizedRef.updateAndGet)
    let updateAndGet (self: SynchronizedRef<'A>) (f: 'A -> 'A) : Effect<'A, 'E, 'R> =
        withPermit self.Semaphore (Ref.updateAndGet self.Backing f)

    /// Conditionally updates the value. (SynchronizedRef.updateSome)
    let updateSome (self: SynchronizedRef<'A>) (pf: 'A -> 'A option) : Effect<unit, 'E, 'R> =
        withPermit self.Semaphore (Ref.updateSome self.Backing pf)

    /// Conditionally updates the value, returning the resulting current value.
    /// (SynchronizedRef.updateSomeAndGet)
    let updateSomeAndGet (self: SynchronizedRef<'A>) (pf: 'A -> 'A option) : Effect<'A, 'E, 'R> =
        withPermit self.Semaphore (Ref.updateSomeAndGet self.Backing pf)

    // --- effectful mutations (serialized; the transition itself is an Effect) ---

    /// Runs an effectful update under the permit; on success stores the new value
    /// and returns the previous one. (SynchronizedRef.getAndUpdateEffect)
    let getAndUpdateEffect (self: SynchronizedRef<'A>) (f: 'A -> Effect<'A, 'E, 'R>) : Effect<'A, 'E, 'R> =
        withPermit
            self.Semaphore
            (Effect.sync (fun () -> getUnsafe self)
             |> Effect.flatMap (fun value ->
                 f value
                 |> Effect.map (fun newValue ->
                     setBacking self newValue
                     value)))

    /// Runs an effectful conditional update under the permit, returning the
    /// previous value. (SynchronizedRef.getAndUpdateSomeEffect)
    let getAndUpdateSomeEffect
        (self: SynchronizedRef<'A>)
        (pf: 'A -> Effect<'A option, 'E, 'R>)
        : Effect<'A, 'E, 'R> =
        withPermit
            self.Semaphore
            (Effect.sync (fun () -> getUnsafe self)
             |> Effect.flatMap (fun value ->
                 pf value
                 |> Effect.map (fun option ->
                     match option with
                     | Some next -> setBacking self next
                     | None -> ()

                     value)))

    /// Runs an effectful modification under the permit; on success stores the new
    /// value and returns the computed result. (SynchronizedRef.modifyEffect)
    let modifyEffect (self: SynchronizedRef<'A>) (f: 'A -> Effect<'B * 'A, 'E, 'R>) : Effect<'B, 'E, 'R> =
        withPermit
            self.Semaphore
            (Effect.sync (fun () -> getUnsafe self)
             |> Effect.flatMap (fun value ->
                 f value
                 |> Effect.map (fun (b, a) ->
                     setBacking self a
                     b)))

    /// Runs an effectful conditional modification under the permit; `Some` stores
    /// the new value, `None` leaves it unchanged. Returns the computed result.
    /// (SynchronizedRef.modifySomeEffect)
    let modifySomeEffect
        (self: SynchronizedRef<'A>)
        (pf: 'A -> Effect<'B * 'A option, 'E, 'R>)
        : Effect<'B, 'E, 'R> =
        withPermit
            self.Semaphore
            (Effect.sync (fun () -> getUnsafe self)
             |> Effect.flatMap (fun value ->
                 pf value
                 |> Effect.map (fun (b, option) ->
                     match option with
                     | Some next -> setBacking self next
                     | None -> ()

                     b)))

    /// Runs an effectful update under the permit; on success stores the new value.
    /// (SynchronizedRef.updateEffect)
    let updateEffect (self: SynchronizedRef<'A>) (f: 'A -> Effect<'A, 'E, 'R>) : Effect<unit, 'E, 'R> =
        withPermit
            self.Semaphore
            (Effect.sync (fun () -> getUnsafe self)
             |> Effect.flatMap (fun value -> f value |> Effect.map (fun newValue -> setBacking self newValue)))

    /// Runs an effectful update under the permit; on success stores and returns
    /// the new value. (SynchronizedRef.updateAndGetEffect)
    let updateAndGetEffect (self: SynchronizedRef<'A>) (f: 'A -> Effect<'A, 'E, 'R>) : Effect<'A, 'E, 'R> =
        withPermit
            self.Semaphore
            (Effect.sync (fun () -> getUnsafe self)
             |> Effect.flatMap (fun value ->
                 f value
                 |> Effect.map (fun newValue ->
                     setBacking self newValue
                     newValue)))

    /// Runs an effectful conditional update under the permit; `Some` stores the
    /// new value, `None` leaves it unchanged. (SynchronizedRef.updateSomeEffect)
    let updateSomeEffect (self: SynchronizedRef<'A>) (pf: 'A -> Effect<'A option, 'E, 'R>) : Effect<unit, 'E, 'R> =
        withPermit
            self.Semaphore
            (Effect.sync (fun () -> getUnsafe self)
             |> Effect.flatMap (fun value ->
                 pf value
                 |> Effect.map (fun option ->
                     match option with
                     | Some next -> setBacking self next
                     | None -> ())))

    /// Runs an effectful conditional update under the permit, returning the
    /// resulting current value. (SynchronizedRef.updateSomeAndGetEffect)
    let updateSomeAndGetEffect
        (self: SynchronizedRef<'A>)
        (pf: 'A -> Effect<'A option, 'E, 'R>)
        : Effect<'A, 'E, 'R> =
        withPermit
            self.Semaphore
            (Effect.sync (fun () -> getUnsafe self)
             |> Effect.flatMap (fun value ->
                 pf value
                 |> Effect.map (fun option ->
                     match option with
                     | Some next ->
                         setBacking self next
                         next
                     | None -> value)))
