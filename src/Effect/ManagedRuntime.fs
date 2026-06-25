namespace Effect

open System.Threading.Tasks

/// `ManagedRuntime` — runs many effects against services built once from a `Layer`.
///
/// Port of repos/effect-smol/packages/effect/src/ManagedRuntime.ts. `make` builds
/// the layer's `Context` lazily on first use, caches it for subsequent runs, owns
/// a `Scope` for the layer's acquired resources, and releases them on `dispose`/
/// `disposeEffect`.
///
/// Decoupling / omissions (per CONVENTIONS), driven by the simplified kernel:
///   * The layer's input requirement is fixed to `unit` (top-level layers need no
///     input). The single error channel collapses upstream's `E | ER` union into
///     one `'E` shared by the layer and the run effects.
///   * `MemoMap` is a minimal reference-keyed cache of built `Context`s, enough to
///     share a build across runtimes (`makeWith memoMap`); the richer
///     `Layer.MemoMap` (sub-layer dedup) is deferred — the port's `Layer` is not
///     memoized.
///   * `runFork` registers each forked fiber on the owned scope (via a finalizer)
///     so `dispose` interrupts them — standing in for upstream's `Fiber.runIn`.
///   * Dropped: `runCallback`, `RunOptions`/scheduler overrides, the `TypeId`
///     brand, and the JS `Promise`-flavoured `context()` (use `contextEffect`).
/// A reference-keyed cache of built contexts, shareable across runtimes.
type MemoMap =
    internal
        { Built: System.Collections.Generic.Dictionary<obj, Context> }

/// A runtime built from a `Layer`, executing effects against the layer's services.
type ManagedRuntime<'E> =
    internal
        { Layer: Layer<'E, unit>
          MemoMap: MemoMap
          Scope: Scope<'E, unit>
          mutable Cached: Context option
          mutable Disposed: bool
          Gate: obj }

[<RequireQualifiedAccess>]
module ManagedRuntime =

    /// Create an empty, shareable memo map. (Layer.makeMemoMapUnsafe)
    let makeMemoMap () : MemoMap =
        { Built = System.Collections.Generic.Dictionary<obj, Context>(HashIdentity.Reference) }

    /// Build a `ManagedRuntime` from a layer, sharing the given memo map.
    let makeWith (memoMap: MemoMap) (layer: Layer<'E, unit>) : ManagedRuntime<'E> =
        { Layer = layer
          MemoMap = memoMap
          Scope = Scope.make<'E, unit> ()
          Cached = None
          Disposed = false
          Gate = System.Object() }

    /// Build a `ManagedRuntime` from a layer. (ManagedRuntime.make)
    let make (layer: Layer<'E, unit>) : ManagedRuntime<'E> = makeWith (makeMemoMap ()) layer

    /// The runtime's memo map (shareable via `makeWith`).
    let memoMap (self: ManagedRuntime<'E>) : MemoMap = self.MemoMap

    /// Build (once) and cache the layer's context. (ManagedRuntime.contextEffect)
    let contextEffect (self: ManagedRuntime<'E>) : Effect<Context, 'E, unit> =
        Effect.suspend (fun () ->
            if self.Disposed then
                Effect.failCause (Cause.die (box "ManagedRuntime disposed"))
            else
                match self.Cached with
                | Some ctx -> Effect.succeed ctx
                | None ->
                    let shared =
                        lock self.Gate (fun () ->
                            match self.MemoMap.Built.TryGetValue(box self.Layer) with
                            | true, ctx -> Some ctx
                            | _ -> None)

                    match shared with
                    | Some ctx ->
                        self.Cached <- Some ctx
                        Effect.succeed ctx
                    | None ->
                        self.Layer.Build self.Scope
                        |> Effect.map (fun ctx ->
                            lock self.Gate (fun () ->
                                self.Cached <- Some ctx
                                self.MemoMap.Built.[box self.Layer] <- ctx)

                            ctx))

    let private provide (self: ManagedRuntime<'E>) (effect: Effect<'A, 'E, Context>) : Effect<'A, 'E, unit> =
        contextEffect self
        |> Effect.flatMap (fun ctx -> Effect.provideContext ctx effect)

    // --- runners ---

    /// Run an effect synchronously, returning its `Exit`. (ManagedRuntime.runSyncExit)
    let runSyncExit (self: ManagedRuntime<'E>) (effect: Effect<'A, 'E, Context>) : Exit<'A, 'E> =
        Effect.runSync () (provide self effect)

    /// Run an effect synchronously to its value, raising on failure. (ManagedRuntime.runSync)
    let runSync (self: ManagedRuntime<'E>) (effect: Effect<'A, 'E, Context>) : 'A =
        Effect.runAsync () (provide self effect) |> Async.RunSynchronously

    /// Run an effect as a `Task` of its value, rejecting on failure. (ManagedRuntime.runPromise)
    let runPromise (self: ManagedRuntime<'E>) (effect: Effect<'A, 'E, Context>) : Task<'A> =
        Effect.runAsync () (provide self effect) |> Async.StartAsTask

    /// Run an effect as a `Task` of its full `Exit`. (ManagedRuntime.runPromiseExit)
    let runPromiseExit (self: ManagedRuntime<'E>) (effect: Effect<'A, 'E, Context>) : Task<Exit<'A, 'E>> =
        Effect.runExit () (provide self effect) |> Async.StartAsTask

    /// Fork an effect against the runtime; the fiber is interrupted on `dispose`.
    /// (ManagedRuntime.runFork)
    let runFork (self: ManagedRuntime<'E>) (effect: Effect<'A, 'E, Context>) : Fiber<'A, 'E> =
        let program =
            contextEffect self
            |> Effect.flatMap (fun ctx ->
                Effect.fork (Effect.provideContext ctx effect)
                |> Effect.flatMap (fun fiber ->
                    Scope.addFinalizer self.Scope (Effect.interrupt fiber)
                    |> Effect.map (fun () -> fiber)))

        match Effect.runSync () program with
        | Success fiber -> fiber
        | Failure cause -> raise (EffectFailureException(box cause, Cause.render cause))

    // --- disposal ---

    /// Release the runtime's layer resources (closing the owned scope) and mark it
    /// disposed. (ManagedRuntime.disposeEffect)
    let disposeEffect (self: ManagedRuntime<'E>) : Effect<unit, 'E, unit> =
        Effect.suspend (fun () ->
            lock self.Gate (fun () ->
                self.Disposed <- true
                self.Cached <- None)

            Scope.close self.Scope (Success(box ())))

    /// Release the runtime's layer resources as a `Task`. (ManagedRuntime.dispose)
    let dispose (self: ManagedRuntime<'E>) : Task<unit> =
        Effect.runAsync () (disposeEffect self) |> Async.StartAsTask
