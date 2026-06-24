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

    /// Re-tag a defect/interrupt-only cause from `'E` to `'E2`. Callers must have
    /// already established the cause carries no `Fail` reason (e.g. `catchAll`'s
    /// no-typed-error branch). Delegates to `Cause.map`; the mapping function is
    /// unreachable by construction, so a `Fail` here is a caller bug and is
    /// surfaced as an exception rather than silently fabricated into a defect.
    let private retagDefects (cause: Cause<'E>) : Cause<'E2> =
        cause
        |> Cause.map (fun _ -> failwith "retagDefects: unexpected Fail reason in a defect-only cause")

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
                    | Failure cause -> Failure(Cause.map f cause)
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

    // --- CE / resource support (slices 3-4: powers `effect { }`'s for/while/try/use!) ---

    /// Defer construction of an effect until it is run. Lets the `effect { }`
    /// builder's `Delay` keep statement bodies cold. (Effect.suspend)
    let suspend (f: unit -> Effect<'A, 'E, 'R>) : Effect<'A, 'E, 'R> =
        Effect(fun r ->
            async {
                let (Effect run) = f ()
                return! run r
            })

    /// Run `finalizer` after `eff` completes, regardless of success, typed
    /// failure or defect (and even if `eff` raises). The finalizer cannot change
    /// a success value; if the finalizer itself fails its cause is appended.
    /// `try/finally` in `effect { }` lowers to this. (Effect.ensuring)
    let ensuring (finalizer: Effect<unit, 'E, 'R>) (Effect run) : Effect<'A, 'E, 'R> =
        Effect(fun r ->
            async {
                let! exit =
                    async {
                        try
                            return! run r
                        with ex ->
                            return Exit.die (box (unwrap ex))
                    }

                let (Effect runFin) = finalizer

                let! finExit =
                    async {
                        try
                            return! runFin r
                        with ex ->
                            return Exit.die (box (unwrap ex))
                    }

                return
                    match finExit with
                    | Success _ -> exit
                    | Failure fc ->
                        match exit with
                        | Failure c -> Failure(Cause.combine c fc)
                        | Success _ -> Failure fc
            })

    /// Sequence `f` over a collection, collecting the successes. The first
    /// failure (typed or defect) short-circuits. `for` in `effect { }` lowers to
    /// this. (Effect.forEach)
    let forEach (items: seq<'T>) (f: 'T -> Effect<'B, 'E, 'R>) : Effect<'B list, 'E, 'R> =
        Effect(fun r ->
            async {
                let results = System.Collections.Generic.List<'B>()
                use en = items.GetEnumerator()
                let mutable failure = None

                while failure.IsNone && en.MoveNext() do
                    let (Effect run) = f en.Current
                    let! exit = run r

                    match exit with
                    | Success b -> results.Add b
                    | Failure c -> failure <- Some c

                return
                    match failure with
                    | Some c -> Failure c
                    | None -> Success(List.ofSeq results)
            })

    /// Repeat `body` while `guard` holds. The first failure short-circuits.
    /// `while` in `effect { }` lowers to this. (Effect.whileLoop)
    let whileLoop (guard: unit -> bool) (body: Effect<unit, 'E, 'R>) : Effect<unit, 'E, 'R> =
        Effect(fun r ->
            async {
                let mutable result = Success()
                let mutable go = true

                while go && guard () do
                    let (Effect run) = body
                    let! exit = run r

                    match exit with
                    | Success() -> ()
                    | Failure c ->
                        result <- Failure c
                        go <- false

                return result
            })

    /// Catch a defect that is a .NET exception, routing it to `handler`. Typed
    /// failures and non-exception defects propagate unchanged. `try/with` in
    /// `effect { }` lowers to this (F#'s `with` binds an `exn`). (Effect.catchAllDefect)
    let catchDefect (handler: exn -> Effect<'A, 'E, 'R>) (Effect run) : Effect<'A, 'E, 'R> =
        Effect(fun r ->
            async {
                let! exit = run r

                match exit with
                | Success a -> return Success a
                | Failure cause ->
                    match cause.Reasons |> List.tryPick (function
                                                         | Reason.Die(:? exn as ex) -> Some ex
                                                         | _ -> None) with
                    | Some ex ->
                        let (Effect run2) = handler ex
                        return! run2 r
                    | None -> return Failure cause
            })

    /// Bracket: acquire a resource, use it, then *always* release it with the
    /// `Exit` of the use step available. Release runs on success, typed failure
    /// or defect; if acquire fails, `use`/`release` never run. (Effect.acquireUseRelease)
    let acquireUseRelease
        (acquire: Effect<'A, 'E, 'R>)
        (useResource: 'A -> Effect<'B, 'E, 'R>)
        (release: 'A -> Exit<'B, 'E> -> Effect<unit, 'E, 'R>)
        : Effect<'B, 'E, 'R> =
        Effect(fun r ->
            async {
                let (Effect runAcq) = acquire
                let! acqExit = runAcq r

                match acqExit with
                | Failure c -> return Failure c
                | Success a ->
                    let! useExit =
                        async {
                            try
                                let (Effect runUse) = useResource a
                                return! runUse r
                            with ex ->
                                return Exit.die (box (unwrap ex))
                        }

                    let (Effect runRel) = release a useExit

                    let! relExit =
                        async {
                            try
                                return! runRel r
                            with ex ->
                                return Exit.die (box (unwrap ex))
                        }

                    return
                        match relExit with
                        | Success _ -> useExit
                        | Failure fc ->
                            match useExit with
                            | Failure c -> Failure(Cause.combine c fc)
                            | Success _ -> Failure fc
            })

    // --- Context-based dependency injection (slice: w2-di) ---

    /// Read a service from the ambient `Context` (the effect's environment).
    /// A missing service surfaces as a **defect** (Die). `Effect.environment`
    /// already exposes the raw `'R`; this is the typed, `Tag`-keyed accessor.
    /// (Effect.service / Context.Tag-as-Effect.)
    let service (tag: Tag<'Service>) : Effect<'Service, 'E, Context> =
        Effect(fun ctx ->
            async {
                match Context.tryGet tag ctx with
                | Some s -> return Success s
                | None ->
                    return
                        Exit.die (
                            box (System.Collections.Generic.KeyNotFoundException(sprintf "Service not found: %s" tag.Key))
                        )
            })

    /// Provide an entire `Context`, discharging an effect's `Context`
    /// requirement. (Effect.provide)
    let provideContext (ctx: Context) (eff: Effect<'A, 'E, Context>) : Effect<'A, 'E, 'R> =
        let (Effect run) = eff
        Effect(fun _ -> run ctx)

    /// Provide a single service, discharging an effect's `Context` requirement
    /// with a one-service context. (Effect.provideService)
    let provideService (tag: Tag<'Service>) (service: 'Service) (eff: Effect<'A, 'E, Context>) : Effect<'A, 'E, 'R> =
        provideContext (Context.make tag service) eff

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

    /// Keep statement bodies cold until the effect is run.
    member _.Delay(f: unit -> Effect<'A, 'E, 'R>) : Effect<'A, 'E, 'R> = Effect.suspend f

    /// Sequence two statements: run `a` (a `unit` statement) then `b`.
    member _.Combine(a: Effect<unit, 'E, 'R>, b: Effect<'B, 'E, 'R>) : Effect<'B, 'E, 'R> = Effect.zipRight b a

    /// `for x in xs do ...` — sequence the body over the collection.
    member _.For(items: seq<'T>, f: 'T -> Effect<unit, 'E, 'R>) : Effect<unit, 'E, 'R> =
        Effect.forEach items f |> Effect.map ignore

    /// `while guard do ...` — repeat the body while the guard holds.
    member _.While(guard: unit -> bool, body: Effect<unit, 'E, 'R>) : Effect<unit, 'E, 'R> = Effect.whileLoop guard body

    /// `try body with ex -> handler` — recover from an exception defect.
    member _.TryWith(body: Effect<'A, 'E, 'R>, handler: exn -> Effect<'A, 'E, 'R>) : Effect<'A, 'E, 'R> =
        Effect.catchDefect handler body

    /// `try body finally compensation` — run the (synchronous) compensation
    /// after the body, success or failure. Effectful finalizers should use
    /// `Effect.acquireUseRelease` / `Scope.acquireRelease` directly (F#'s CE
    /// `finally` block is a non-monadic `unit -> unit`).
    member _.TryFinally(body: Effect<'A, 'E, 'R>, compensation: unit -> unit) : Effect<'A, 'E, 'R> =
        Effect.ensuring (Effect.sync compensation) body

    /// `use! r = acquire` — bind a disposable resource and `Dispose` it when the
    /// surrounding scope closes (success, failure or defect), lowered to
    /// `ensuring`. Resources with *effectful* finalizers use
    /// `Effect.acquireUseRelease` / `Scope.acquireRelease` instead.
    member _.Using(resource: 'T when 'T :> System.IDisposable, body: 'T -> Effect<'A, 'E, 'R>) : Effect<'A, 'E, 'R> =
        Effect.ensuring
            (Effect.sync (fun () ->
                if not (obj.ReferenceEquals(resource, null)) then
                    resource.Dispose()))
            (body resource)

[<AutoOpen>]
module EffectBuilderModule =
    /// `effect { let! x = ...; return ... }`
    let effect = EffectBuilder()
