namespace Effect

open System.Threading.Tasks

/// Port of repos/effect-smol/packages/effect/src/Runtime.ts — the runtime bundle
/// that carries the services an `Effect` runs with, plus the `run*` family that
/// executes an effect against it.
///
/// Model: a `Runtime` wraps the `Context` (our DI environment — the bundle of
/// services/refs). `make` builds one from a `Context`; `defaultRuntime` is the
/// empty-context runtime. The `run*` functions provide the runtime's context as
/// the effect's environment and run it — mirroring `Effect.runFork`/`runPromise`/
/// `runSync` "over a Runtime" (in effect-smol this surface lives on
/// `ManagedRuntime`; the classic `Runtime` bundle is reproduced here).
///
/// Faithfulness / omissions (per CONVENTIONS):
///   - effect-smol's `Runtime.ts` is, in this beta, the *runMain* layer
///     (`makeRunMain`/`Teardown`/`defaultTeardown`/`errorExitCode`/
///     `errorReported`). Those are a **platform adapter** concern: `makeRunMain`
///     needs fiber *observers* (`fiber.addObserver`) and a host keep-alive timer,
///     and the `errorExitCode`/`errorReported` markers are JS symbol-keyed
///     properties on `Error` objects — JS-runtime machinery dropped per the
///     conventions. They are deferred until a platform layer + fiber supervision
///     land; this module ports the runtime *bundle* the brief specifies.
///   - **Fiber supervision is deferred**: a `Runtime` carries no RuntimeFlags,
///     FiberRefs, scheduler override, or supervisor. `runFork` returns a bare
///     `Fiber` handle (no parent-scope registration); `defaultRuntime` bundles no
///     default services (Clock/Random/… are provided by the caller's `Context`,
///     and in any case compile after this module).
type Runtime = internal { Context: Context }

[<RequireQualifiedAccess>]
module Runtime =

    // --- constructors ---

    /// Build a `Runtime` from a `Context` of services. (Runtime.make)
    let make (context: Context) : Runtime = { Context = context }

    /// The default runtime: an empty service `Context`. Services are added with
    /// `make`/`provideService` on the effect. (Runtime.defaultRuntime)
    let defaultRuntime: Runtime = { Context = Context.empty }

    /// The services this runtime carries.
    let context (self: Runtime) : Context = self.Context

    // --- runners ---

    /// Run an effect against the runtime, returning its full `Exit` as an
    /// `Async`. The total, non-throwing runner. (Runtime.runExit)
    let runExit (self: Runtime) (effect: Effect<'A, 'E, Context>) : Async<Exit<'A, 'E>> =
        Effect.runExit self.Context effect

    /// Run an effect synchronously, returning its `Exit` (never throws for a typed
    /// error). (Runtime.runSyncExit)
    let runSyncExit (self: Runtime) (effect: Effect<'A, 'E, Context>) : Exit<'A, 'E> =
        Effect.runSync self.Context effect

    /// Run an effect synchronously to its success value, **raising** on failure (a
    /// defect that is itself an exception is re-raised; otherwise an
    /// `EffectFailureException`). (Runtime.runSync)
    // SPIKE: original is `Effect.runAsync ctx effect |> Async.RunSynchronously`.
    // Async.RunSynchronously is unsupported on JS. Reuse the spike's non-blocking
    // Effect.runSync driver (raises on a suspended effect, like Effect-TS).
    // (severity: refactor — JS-specific runner)
    let runSync (self: Runtime) (effect: Effect<'A, 'E, Context>) : 'A =
        match Effect.runSync self.Context effect with
        | Success a -> a
        | Failure cause ->
            match cause.Reasons with
            | [ Reason.Die(:? exn as ex) ] -> raise ex
            | _ -> raise (EffectFailureException(box cause, Cause.render cause))

    /// Run an effect to its success value, rejecting on failure. (Runtime.runPromise)
    // SPIKE: original returns Task<'A> via Async.StartAsTask (unsupported by Fable).
    // Return the Async directly; at the JS boundary it is bridged to a real Promise
    // with Async.StartAsPromise (Fable runtime exports `startAsPromise`). The public
    // surface for JS consumers should expose Promise here. (severity: refactor)
    let runPromise (self: Runtime) (effect: Effect<'A, 'E, Context>) : Async<'A> =
        Effect.runAsync self.Context effect

    /// Run an effect to its full `Exit` (never rejects for a typed error).
    /// (Runtime.runPromiseExit)
    let runPromiseExit (self: Runtime) (effect: Effect<'A, 'E, Context>) : Async<Exit<'A, 'E>> =
        Effect.runExit self.Context effect

    /// Start an effect on a fresh fiber and return its handle immediately. The
    /// fiber runs concurrently against the runtime's context. (Runtime.runFork)
    let runFork (self: Runtime) (effect: Effect<'A, 'E, Context>) : Fiber<'A, 'E> =
        match Effect.runSync self.Context (Effect.fork effect) with
        | Success fiber -> fiber
        | Failure cause -> raise (EffectFailureException(box cause, Cause.render cause))
