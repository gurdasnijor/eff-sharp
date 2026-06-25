namespace Effect

/// Port of repos/effect-smol/packages/effect/src/Pool.ts.
///
/// A `Pool<'A>` shares a bounded set of scoped resources across fibers: items
/// are acquired with a scoped effect, borrowed with `get` (returned when the
/// borrowing scope closes), can be `invalidate`d, and are all released when the
/// pool's scope closes.
///
/// SCOPE — this ports the **fixed-size core** (`make` / `get` / `getScoped` /
/// `invalidate` / shutdown-on-scope-close) faithfully against the merged
/// primitives, plus the failure/finalizer behaviors the suite checks. The merged
/// `Effect` does not yet expose the combinators the elastic machinery needs
/// (`forkIn`/`forkDetach` background fibers, `Effect.scope`/context-threaded
/// `Scope` in `R`, `onInterrupt`, `replicateEffect`, `uninterruptibleMask`), so
/// the following are **deferred** and noted: `makeWithTTL` / `makeWithStrategy`
/// (creation & usage TTL strategies, dynamic min/max resize, `targetUtilization`,
/// `concurrency > 1`), the background `resize` loop, and the `isPool` runtime
/// guard. Within the fixed-size model the observable contract — preallocation,
/// reuse, blocking when exhausted, invalidate-and-replace, and release-on-close
/// (defects included) — is reproduced.
///
/// Modeling choices (per CONVENTIONS.md): the error channel is `obj` (boxed),
/// matching the merged `Queue`/`Pull` (the available-items buffer is a `Queue`,
/// whose channel is `obj`); `Pool<'A>` therefore takes a single type parameter.
/// Scopes are explicit values (the port's `Scope` is not threaded through `R`):
/// `acquire` receives the item's `Scope` and `get` receives the borrower's.

[<RequireQualifiedAccess>]
type internal PoolItem<'A> =
    { Exit: Exit<'A, obj>
      ItemScope: Scope<obj, unit>
      mutable Invalidated: bool }

/// A pool of shared scoped resources of type `'A`.
type Pool<'A> =
    internal
        { Acquire: Scope<obj, unit> -> Effect<'A, obj, unit>
          Size: int
          Available: Queue<PoolItem<'A>>
          AllItems: ResizeArray<PoolItem<'A>>
          Lock: obj
          mutable ShuttingDown: bool }

[<RequireQualifiedAccess>]
module Pool =

    /// Run an effect to its `Exit`, folding a typed failure into the value
    /// channel (defects/interrupts are left to propagate — acquisitions in the
    /// suite fail typed).
    let private toExit (eff: Effect<'A, obj, unit>) : Effect<Exit<'A, obj>, obj, unit> =
        eff
        |> Effect.map Success
        |> Effect.catchAll (fun e -> Effect.succeed (Exit.fail e))

    /// Acquire one item into a fresh item-scope and add it to the available
    /// buffer. A failed acquisition still runs its registered finalizers
    /// immediately (so failed allocations are cleaned up) and is buffered so a
    /// borrower observes the failure.
    let private allocate (pool: Pool<'A>) : Effect<unit, obj, unit> =
        Effect.suspend (fun () ->
            let itemScope: Scope<obj, unit> = Scope.make ()

            pool.Acquire itemScope
            |> toExit
            |> Effect.flatMap (fun exit ->
                let item: PoolItem<'A> =
                    { Exit = exit
                      ItemScope = itemScope
                      Invalidated = false }

                lock pool.Lock (fun () -> pool.AllItems.Add item)

                match exit with
                | Failure _ ->
                    Scope.close itemScope (Scope.eraseExit exit)
                    |> Effect.flatMap (fun () -> Queue.offer pool.Available item)
                    |> Effect.map ignore
                | Success _ -> Queue.offer pool.Available item |> Effect.map ignore))

    /// Remove and release an item, then allocate a replacement (keeping the pool
    /// at its fixed size). A no-op replacement is skipped when shutting down.
    let private releaseAndReplace (pool: Pool<'A>) (item: PoolItem<'A>) : Effect<unit, obj, unit> =
        Effect.sync (fun () -> lock pool.Lock (fun () -> pool.AllItems.Remove item |> ignore))
        |> Effect.flatMap (fun () -> Scope.close item.ItemScope (Scope.eraseExit item.Exit))
        |> Effect.flatMap (fun () ->
            if pool.ShuttingDown then
                Effect.succeed ()
            else
                allocate pool)

    /// Return a borrowed item to the pool (or release+replace it if invalidated).
    let private returnItem (pool: Pool<'A>) (item: PoolItem<'A>) : Effect<unit, obj, unit> =
        Effect.suspend (fun () ->
            if pool.ShuttingDown then Effect.succeed ()
            elif item.Invalidated then releaseAndReplace pool item
            else Queue.offer pool.Available item |> Effect.map ignore)

    /// Borrow one item, registering its return on `borrowScope`. Blocks while the
    /// pool is fully borrowed; fails with the acquisition error for failed items;
    /// transparently releases-and-replaces invalidated items. (Pool.get)
    let get (pool: Pool<'A>) (borrowScope: Scope<obj, unit>) : Effect<'A, obj, unit> =
        let rec loop () : Effect<'A, obj, unit> =
            Effect.suspend (fun () ->
                if pool.ShuttingDown then
                    Effect.failCause (Cause.interrupt None)
                else
                    Queue.take pool.Available
                    |> Effect.flatMap (fun item ->
                        if item.Invalidated then
                            releaseAndReplace pool item |> Effect.flatMap loop
                        else
                            match item.Exit with
                            | Failure c -> Effect.failCause c
                            | Success a ->
                                Scope.addFinalizer borrowScope (returnItem pool item)
                                |> Effect.map (fun () -> a)))

        loop ()

    /// Borrow one item for the duration of a fresh scope (returned on completion).
    /// The convenience form of `get` for one-shot use.
    let getScoped (pool: Pool<'A>) : Effect<'A, obj, unit> = Scope.scoped (fun s -> get pool s)

    /// Mark the pool item holding `value` as invalidated; it is released and
    /// replaced lazily the next time it would be reused. (Pool.invalidate)
    let invalidate<'A when 'A: equality> (pool: Pool<'A>) (value: 'A) : Effect<unit, obj, unit> =
        Effect.sync (fun () ->
            lock pool.Lock (fun () ->
                if not pool.ShuttingDown then
                    for item in pool.AllItems do
                        match item.Exit with
                        | Success a when a = value -> item.Invalidated <- true
                        | _ -> ()))

    let private shutdown (pool: Pool<'A>) : Effect<unit, obj, unit> =
        Effect.suspend (fun () ->
            let items =
                lock pool.Lock (fun () ->
                    if pool.ShuttingDown then
                        []
                    else
                        pool.ShuttingDown <- true
                        List.ofSeq pool.AllItems)

            items
            |> List.map (fun item -> Scope.close item.ItemScope (Scope.eraseExit item.Exit))
            |> List.fold (fun acc eff -> acc |> Effect.flatMap (fun () -> eff)) (Effect.succeed ()))

    /// Create a fixed-size pool in `poolScope`. `acquire` receives the item's
    /// scope (register the item's release on it). The `size` items are
    /// preallocated; all are released when `poolScope` closes. (Pool.make)
    let make
        (poolScope: Scope<obj, unit>)
        (acquire: Scope<obj, unit> -> Effect<'A, obj, unit>)
        (size: int)
        : Effect<Pool<'A>, obj, unit> =
        Effect.suspend (fun () ->
            Queue.unbounded ()
            |> Effect.flatMap (fun available ->
                let pool: Pool<'A> =
                    { Acquire = acquire
                      Size = size
                      Available = available
                      AllItems = ResizeArray<PoolItem<'A>>()
                      Lock = obj ()
                      ShuttingDown = false }

                Scope.addFinalizer poolScope (shutdown pool)
                |> Effect.flatMap (fun () -> Effect.forEach (Seq.replicate size ()) (fun () -> allocate pool))
                |> Effect.map (fun _ -> pool)))
