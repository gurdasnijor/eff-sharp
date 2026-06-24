namespace Effect

open System.Threading.Tasks

/// Native F# port of Effect's core `Effect<A, E, R>` type.
///
/// Slice 3: the core is now **asynchronous** and completes with an
/// `Exit<'A,'E>` (typed failure *or* defect), built on the slice-2 `Cause`/`Exit`
/// model. It still carries all three of Effect's channels:
///   'A  - success value
///   'E  - typed (expected) error
///   'R  - required environment / dependencies (DI)
///
/// Representation — a Reader over `Async<Exit>`:
///   `Effect of ('R -> Async<Exit<'A,'E>>)`
/// A single function unifies dependencies (`'R`), asynchrony (`Async`), typed
/// errors and defects (`Exit`/`Cause`). `Async` (rather than `Task`) is chosen
/// because it is F#-native, cold/referentially-transparent (an `Effect` is a
/// description, started only by a runner) and composes with `async { }`; .NET
/// `Task`s are bridged at the edges via `promise`/`tryPromise`.
///
/// Fibers, interruption and Layer arrive in later slices. The public surface
/// (succeed/fail/sync/map/flatMap/...) is kept close to Effect's and to the
/// previous synchronous slice so call sites change minimally.
///
/// Omissions vs upstream (noted per CONVENTIONS): the HKT/typeclass plumbing and
/// JS runtime type-guards are skipped; `Effect.match`-style combinators are
/// lowered into native `match ... with`.
type Effect<'A, 'E, 'R> = internal Effect of ('R -> Async<Exit<'A, 'E>>)

/// Raised by `runAsync`/`runSync` when an effect completes with a `Failure`
/// whose cause is a typed error (or a non-exception defect). A defect that is
/// itself a .NET exception is re-raised directly instead.
type EffectFailureException(cause: obj, render: string) =
    inherit System.Exception(render)
    /// The boxed `Cause<'E>` that produced this failure.
    member _.Cause = cause

[<RequireQualifiedAccess>]
module Effect =

    // --- internal helpers ---

    /// Bridging .NET tasks can wrap the real error in a single-inner
    /// `AggregateException`; surface the original exception as the defect.
    let private unwrap (ex: exn) : exn =
        match ex with
        | :? System.AggregateException as agg when agg.InnerExceptions.Count = 1 -> agg.InnerException
        | _ -> ex

    /// Re-tag a defect/interrupt-only cause from `'E` to `'E2`. Only the
    /// `Fail` reason carries `'E`; when none is present the cause is safely
    /// reinterpreted at the new error type.
    let private retagDefects (cause: Cause<'E>) : Cause<'E2> =
        { Reasons =
            cause.Reasons
            |> List.map (fun reason ->
                match reason with
                | Reason.Die d -> Reason.Die d
                | Reason.Interrupt id -> Reason.Interrupt id
                | Reason.Fail _ -> Reason.Die(box "unreachable: Fail in defect-only cause")) }

    // --- constructors ---

    /// An effect that always succeeds with `value`. (Effect.succeed)
    let succeed (value: 'A) : Effect<'A, 'E, 'R> = Effect(fun _ -> async { return Success value })

    /// An effect that always fails with the typed error `error`. (Effect.fail)
    let fail (error: 'E) : Effect<'A, 'E, 'R> = Effect(fun _ -> async { return Exit.fail error })

    /// An effect that fails with an arbitrary `Cause`. (Effect.failCause)
    let failCause (cause: Cause<'E>) : Effect<'A, 'E, 'R> = Effect(fun _ -> async { return Failure cause })

    /// An effect from a thunk. A thrown exception becomes a **defect** (Die),
    /// not a typed error. (Effect.sync)
    let sync (thunk: unit -> 'A) : Effect<'A, 'E, 'R> =
        Effect(fun _ ->
            async {
                try
                    return Success(thunk ())
                with ex ->
                    return Exit.die (box ex)
            })

    /// Lift an F# Result directly into an effect. (Effect.fromResult)
    let fromResult (result: Result<'A, 'E>) : Effect<'A, 'E, 'R> =
        Effect(fun _ ->
            async {
                return
                    match result with
                    | Ok a -> Success a
                    | Error e -> Exit.fail e
            })

    /// Wrap a .NET `Task`-producing thunk that is *not expected to fail*. A
    /// thrown/rejected task surfaces as a **defect** (Die) via the runner's
    /// safety net. (Effect.promise)
    let promise (f: unit -> Task<'A>) : Effect<'A, 'E, 'R> =
        Effect(fun _ ->
            async {
                let! a = Async.AwaitTask(f ())
                return Success a
            })

    /// Wrap a .NET `Task`-producing thunk that *may* throw/reject. Any thrown
    /// exception is caught and mapped to the **defect** (Die) channel via
    /// `Exit.die`. (Effect.tryPromise — note: this port routes failures to the
    /// defect channel rather than taking a catch mapper into the typed channel.)
    let tryPromise (f: unit -> Task<'A>) : Effect<'A, 'E, 'R> =
        Effect(fun _ ->
            async {
                try
                    let! a = Async.AwaitTask(f ())
                    return Success a
                with ex ->
                    return Exit.die (box (unwrap ex))
            })

    /// Access the whole environment. (Effect.context / service root)
    let environment<'R, 'E> : Effect<'R, 'E, 'R> = Effect(fun r -> async { return Success r })

    /// Project a value out of the environment. (Effect.map over context)
    let environmentWith (f: 'R -> 'A) : Effect<'A, 'E, 'R> = Effect(fun r -> async { return Success(f r) })

    // --- combinators ---

    /// Transform the success value. (Effect.map)
    let map (f: 'A -> 'B) (Effect run) : Effect<'B, 'E, 'R> =
        Effect(fun r ->
            async {
                let! exit = run r

                return
                    match exit with
                    | Success a -> Success(f a)
                    | Failure c -> Failure c
            })

    /// Transform the typed error, leaving defects/interrupts untouched.
    /// (Effect.mapError)
    let mapError (f: 'E -> 'E2) (Effect run) : Effect<'A, 'E2, 'R> =
        Effect(fun r ->
            async {
                let! exit = run r

                return
                    match exit with
                    | Success a -> Success a
                    | Failure cause ->
                        Failure
                            { Reasons =
                                cause.Reasons
                                |> List.map (fun reason ->
                                    match reason with
                                    | Reason.Fail e -> Reason.Fail(f e)
                                    | Reason.Die d -> Reason.Die d
                                    | Reason.Interrupt id -> Reason.Interrupt id) }
            })

    /// Sequence a dependent effect. The failure cause (typed *or* defect)
    /// short-circuits. (Effect.flatMap)
    let flatMap (f: 'A -> Effect<'B, 'E, 'R>) (Effect run) : Effect<'B, 'E, 'R> =
        Effect(fun r ->
            async {
                let! exit = run r

                match exit with
                | Success a ->
                    let (Effect run2) = f a
                    return! run2 r
                | Failure c -> return Failure c
            })

    /// Recover from a typed error. Defect/interrupt-only causes propagate
    /// unchanged (re-tagged to the new error type). (Effect.catchAll)
    let catchAll (handler: 'E -> Effect<'A, 'E2, 'R>) (Effect run) : Effect<'A, 'E2, 'R> =
        Effect(fun r ->
            async {
                let! exit = run r

                match exit with
                | Success a -> return Success a
                | Failure cause ->
                    match Cause.failures cause |> List.tryHead with
                    | Some e ->
                        let (Effect run2) = handler e
                        return! run2 r
                    | None -> return Failure(retagDefects cause)
            })

    /// Run `b`, ignoring `a`'s value but keeping its effect. (Effect.zipRight)
    let zipRight (b: Effect<'B, 'E, 'R>) (a: Effect<'A, 'E, 'R>) : Effect<'B, 'E, 'R> = a |> flatMap (fun _ -> b)

    /// Combine two effects' values with `f`. (Effect.zipWith)
    let zipWith (f: 'A -> 'B -> 'C) (b: Effect<'B, 'E, 'R>) (a: Effect<'A, 'E, 'R>) : Effect<'C, 'E, 'R> =
        a |> flatMap (fun av -> b |> map (fun bv -> f av bv))

    // --- runners ---

    /// Run an effect against its environment, returning its `Exit`. Never throws
    /// for a typed error (it lands in the `Exit`); any escaping exception is
    /// captured as a defect (Die), so this is the total, safe runner.
    /// (Effect.runPromiseExit analogue.)
    let runExit (env: 'R) (Effect f) : Async<Exit<'A, 'E>> =
        async {
            try
                return! f env
            with ex ->
                return Exit.die (box (unwrap ex))
        }

    /// Run an effect to its success value, *raising* on failure. A defect that
    /// is itself a .NET exception is re-raised directly; otherwise an
    /// `EffectFailureException` carrying the `Cause` is raised. (Effect.runPromise)
    let runAsync (env: 'R) (eff: Effect<'A, 'E, 'R>) : Async<'A> =
        async {
            let! exit = runExit env eff

            match exit with
            | Success a -> return a
            | Failure cause ->
                match cause.Reasons with
                | [ Reason.Die(:? exn as ex) ] -> return raise ex
                | _ -> return raise (EffectFailureException(box cause, Cause.render cause))
        }

    /// Run the async core to completion *synchronously*, returning the `Exit`.
    /// Convenient for tests and CLI entry points. NOTE: a genuinely-async effect
    /// blocks the calling thread until it completes. (Effect.runSyncExit analogue.)
    let runSync (env: 'R) (eff: Effect<'A, 'E, 'R>) : Exit<'A, 'E> =
        runExit env eff |> Async.RunSynchronously

/// Computation-expression builder: the `effect { ... }` do-notation,
/// analogous to `Effect.gen(function* () { ... })`.
type EffectBuilder() =
    member _.Return(value: 'A) : Effect<'A, 'E, 'R> = Effect.succeed value
    member _.ReturnFrom(eff: Effect<'A, 'E, 'R>) : Effect<'A, 'E, 'R> = eff

    member _.Bind(eff: Effect<'A, 'E, 'R>, f: 'A -> Effect<'B, 'E, 'R>) : Effect<'B, 'E, 'R> = Effect.flatMap f eff

    member _.Zero() : Effect<unit, 'E, 'R> = Effect.succeed ()

[<AutoOpen>]
module EffectBuilderModule =
    /// `effect { let! x = ...; return ... }`
    let effect = EffectBuilder()
