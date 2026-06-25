namespace Effect

/// `Scope` — a finalizer registry / lifetime boundary.
///
/// Port of repos/effect-smol/packages/effect/src/Scope.ts. A `Scope` collects
/// finalizers and runs them, **LIFO**, when it is closed, each as an `Effect`
/// with the closing `Exit` available. This is the substrate for
/// `Scope.acquireRelease` / `Scope.scoped`.
///
/// The finalizer list is held in a mutable cell behind a lock (the "atomic cell"
/// pattern from the brief) rather than depending on the `Ref` module, which
/// another agent owns. Finalizers are stored prepended, so iterating the stored
/// list front-to-back yields reverse-registration (LIFO) order.
///
/// Omissions vs upstream (per CONVENTIONS): the `"sequential" | "parallel"`
/// finalization strategy is fixed to sequential (no fiber runtime yet); the
/// `State.Empty`/`Open`/`Closed` three-way tag is collapsed to open/closed; the
/// `Closeable` brand and `[TypeId]` machinery are dropped. Finalizer failures are
/// swallowed on close (best-effort cleanup), noted at the call site.
/// The closing exit handed to finalizers carries erased value/error types, since
/// a scope's finalizers come from heterogeneous acquisitions.
type Finalizer<'E, 'R> = Exit<obj, obj> -> Effect<unit, 'E, 'R>

[<RequireQualifiedAccess>]
type internal ScopeState<'E, 'R> =
    | Open of Finalizer<'E, 'R> list
    | Closed

/// A lifetime boundary that runs registered finalizers on close.
type Scope<'E, 'R> =
    internal
        { mutable State: ScopeState<'E, 'R>
          Gate: obj }

[<RequireQualifiedAccess>]
module Scope =

    /// A fresh, open scope with no finalizers. (Scope.make)
    let make<'E, 'R> () : Scope<'E, 'R> =
        { State = ScopeState.Open []
          Gate = System.Object() }

    /// Register a finalizer that receives the closing `Exit`. Runs immediately as
    /// a no-op effect; the work happens at `close`. If the scope is already
    /// closed the finalizer is dropped (best-effort). (Scope.addFinalizerExit)
    let addFinalizerExit (scope: Scope<'E, 'R>) (finalizer: Finalizer<'E, 'R>) : Effect<unit, 'E, 'R> =
        Effect.sync (fun () ->
            lock scope.Gate (fun () ->
                match scope.State with
                | ScopeState.Open fs -> scope.State <- ScopeState.Open(finalizer :: fs)
                | ScopeState.Closed -> ()))

    /// Register a finalizer that ignores the closing `Exit`. (Scope.addFinalizer)
    let addFinalizer (scope: Scope<'E, 'R>) (finalizer: Effect<unit, 'E, 'R>) : Effect<unit, 'E, 'R> =
        addFinalizerExit scope (fun _ -> finalizer)

    /// Close the scope, running all finalizers in LIFO order with `exit`. Each
    /// finalizer runs even if a previous one failed (failures are swallowed).
    /// (Scope.close)
    let close (scope: Scope<'E, 'R>) (exit: Exit<obj, obj>) : Effect<unit, 'E, 'R> =
        Effect(fun fib r ->
            async {
                let finalizers =
                    lock scope.Gate (fun () ->
                        match scope.State with
                        | ScopeState.Open fs ->
                            scope.State <- ScopeState.Closed
                            fs
                        | ScopeState.Closed -> [])

                // `fs` is in reverse-registration order already => LIFO.
                for f in finalizers do
                    let (Effect run) = f exit

                    try
                        let! _ = run fib r
                        ()
                    with _ ->
                        ()

                return Success()
            })

    /// Erase an `Exit<'A,'E>` to the boxed `Exit<obj,obj>` finalizers receive.
    let internal eraseExit (exit: Exit<'A, 'E>) : Exit<obj, obj> =
        match exit with
        | Success a -> Success(box a)
        | Failure c ->
            Failure
                { Reasons =
                    c.Reasons
                    |> List.map (function
                        | Reason.Fail e -> Reason.Fail(box e)
                        | Reason.Die d -> Reason.Die d
                        | Reason.Interrupt i -> Reason.Interrupt i) }

    /// Acquire a resource and register its release on `scope`; the resource value
    /// is returned and stays alive until the scope closes. (Effect.acquireRelease
    /// — lives here because it is `Scope`-typed.)
    let acquireRelease
        (scope: Scope<'E, 'R>)
        (acquire: Effect<'A, 'E, 'R>)
        (release: 'A -> Exit<obj, obj> -> Effect<unit, 'E, 'R>)
        : Effect<'A, 'E, 'R> =
        acquire
        |> Effect.flatMap (fun a -> addFinalizerExit scope (fun exit -> release a exit) |> Effect.map (fun () -> a))

    /// Run `f` with a fresh scope that is closed (finalizers run) when `f`
    /// completes — on success, typed failure or defect. (Effect.scoped)
    let scoped (f: Scope<'E, 'R> -> Effect<'A, 'E, 'R>) : Effect<'A, 'E, 'R> =
        Effect.suspend (fun () ->
            let scope = make ()

            Effect.acquireUseRelease (Effect.succeed scope) f (fun s exit -> close s (eraseExit exit)))
