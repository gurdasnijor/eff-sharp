namespace Effect

/// Port of repos/effect-smol/packages/effect/src/Resource.ts — a refreshable
/// scoped value. A `Resource<'A,'E,'R>` keeps the latest acquisition result
/// (`Exit<'A,'E>`) in a `ScopedRef`, so it can be read repeatedly, refreshed
/// manually, or refreshed automatically on a `Schedule`. Because acquisition runs
/// in a scope, each replacement releases the resources owned by the previous
/// value.
///
/// Built on the merged `ScopedRef` (value + owning `Scope`) and `Schedule`
/// (decision machine). `auto` forks a background fiber that drives the schedule
/// and refreshes; the fiber is interrupted when the parent scope closes.
///
/// Porting adaptations vs upstream (noted per CONVENTIONS):
///   - `Scope` is threaded *explicitly* (matching this port's `ScopedRef`/`Scope`)
///     rather than as an environment requirement, so `manual`/`auto` take the
///     owning parent `Scope`.
///   - The Effect core here has no `Effect.exit`/`tap`/`repeat`/`forkScoped`, so
///     those are reconstructed locally: `toExit` (typed-error → `Exit`; defects
///     propagate — noted), a schedule-driven refresh loop, and `fork` + a parent
///     `Scope.addFinalizer` that interrupts the loop (the `forkScoped` analogue).
///   - `isResource` (a JS-runtime `unknown` guard) is omitted — F#'s static types
///     make it dead weight, and generics defeat a runtime type test.
///   - `Resource.acquire` is a plain `Effect` with no scope access, so an
///     acquisition cannot register its own finalizers on the fresh scope (noted);
///     scope replacement/cleanup of the *previous* value still works.
type Resource<'A, 'E, 'R> =
    internal
        { ScopedRef: ScopedRef<Exit<'A, 'E>, 'E, 'R>
          Acquire: Effect<'A, 'E, 'R> }

[<RequireQualifiedAccess>]
module Resource =

    /// Run `eff`, capturing a typed failure as `Exit.fail` instead of failing the
    /// effect (the `Effect.exit` analogue). Defects are not captured (noted).
    let private toExit (eff: Effect<'A, 'E, 'R>) : Effect<Exit<'A, 'E>, 'E2, 'R> =
        eff
        |> Effect.map Exit.succeed
        |> Effect.catchAll (fun e -> Effect.succeed (Exit.fail e))

    /// Creates a `Resource` that must be refreshed manually. Its current value's
    /// scope (and final cleanup) is owned by `parent`. (Resource.manual)
    let manual (parent: Scope<'E, 'R>) (acquire: Effect<'A, 'E, 'R>) : Effect<Resource<'A, 'E, 'R>, 'E, 'R> =
        ScopedRef.fromAcquire parent (fun _scope -> toExit acquire)
        |> Effect.map (fun scopedRef -> { ScopedRef = scopedRef; Acquire = acquire })

    /// Reads the current value. If the stored acquisition failed, fails with the
    /// stored error. (Resource.get)
    let get (self: Resource<'A, 'E, 'R>) : Effect<'A, 'E, 'R> =
        ScopedRef.get self.ScopedRef
        |> Effect.flatMap (fun exit ->
            match exit with
            | Success a -> Effect.succeed a
            | Failure cause -> Effect.failCause cause)

    /// Re-runs acquisition and replaces the stored value, releasing the previous
    /// value's resources. If acquisition fails, the effect fails and the previous
    /// stored value is left in place. (Resource.refresh)
    let refresh (self: Resource<'A, 'E, 'R>) : Effect<unit, 'E, 'R> =
        ScopedRef.set self.ScopedRef (fun _scope -> self.Acquire |> Effect.map Exit.succeed)

    /// Drives `policy` to repeatedly refresh `self`: sleep the scheduled delay,
    /// refresh (ignoring failures so the loop survives), repeat until the schedule
    /// completes. The `Effect.repeat(refresh, policy)` analogue.
    let private repeatRefresh (self: Resource<'A, 'E, 'R>) (policy: Schedule<'Out, unit>) : Effect<unit, 'E, 'R> =
        Effect.suspend (fun () ->
            let step = Schedule.toStep policy
            let refreshIgnore = refresh self |> Effect.catchAll (fun _ -> Effect.succeed ())

            let rec go (now: float) : Effect<unit, 'E, 'R> =
                match step now () with
                | ScheduleDecision.Done _ -> Effect.succeed ()
                | ScheduleDecision.Continue(_, delay) ->
                    let ms = Duration.toMillis delay

                    Effect.sleep (int ms)
                    |> Effect.flatMap (fun () -> refreshIgnore)
                    |> Effect.flatMap (fun () -> go (now + ms))

            go 0.0)

    /// Creates a `Resource` that refreshes automatically on `policy` for the
    /// lifetime of `parent`. The background refresh fiber is interrupted when
    /// `parent` closes. (Resource.auto)
    let auto
        (parent: Scope<'E, 'R>)
        (acquire: Effect<'A, 'E, 'R>)
        (policy: Schedule<'Out, unit>)
        : Effect<Resource<'A, 'E, 'R>, 'E, 'R> =
        manual parent acquire
        |> Effect.flatMap (fun self ->
            Effect.fork (repeatRefresh self policy)
            |> Effect.flatMap (fun fiber ->
                Scope.addFinalizer parent (Effect.interrupt fiber)
                |> Effect.map (fun () -> self)))
