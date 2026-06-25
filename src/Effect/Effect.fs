namespace Effect

open System.Threading.Tasks

#if FABLE_COMPILER
open Fable.Core
#endif

/// Per-fiber runtime state threaded through every effect. Interruption is
/// **cooperative**: `Interrupted` is a
/// flag set by `interrupt`, checked at yield points (`flatMap` boundaries,
/// `sleep`, fork/await); `Mask` is the uninterruptible-region depth — while it is
/// > 0 a pending interrupt is deferred and honored when the region exits. This
/// (vs. .NET `CancellationToken`) is what lets finalizers run on interrupt:
/// honoring an interrupt is a *normal* `Exit.Interrupt` return, not an exception
/// unwind that bypasses `try/finally`.
type FiberRuntime =
    { mutable Interrupted: bool
      mutable Mask: int }

    static member Root() : FiberRuntime = { Interrupted = false; Mask = 0 }

/// A handle to a forked, concurrently-running effect and the `FiberRuntime` whose
/// `Interrupted` flag interrupts it. (Fiber.Fiber)
///
/// On .NET the backing computation is a `Task<Exit>` (`Async.StartAsTask`). Fable's
/// runtime library implements neither `StartAsTask` nor `AwaitTask`, so under
/// `FABLE_COMPILER` the handle is an `Async.StartChild` join plus a completion cell
/// (`Result`) that `poll`/`isLive` read in place of `Task.IsCompleted`/`Result`.
/// The .NET representation is unchanged.
type Fiber<'A, 'E> =
    internal
        {
#if FABLE_COMPILER
          Join: Async<Exit<'A, 'E>>
          Result: Exit<'A, 'E> option ref
#else
          Task: Task<Exit<'A, 'E>>
#endif
          Runtime: FiberRuntime }

/// Native F# port of Effect's core `Effect<A, E, R>` type.
///
/// Representation — a Reader over the fiber runtime AND the environment, into
/// `Async<Exit>`:
///   `Effect of (FiberRuntime -> 'R -> Async<Exit<'A,'E>>)`
/// `Async` gives stack-safety/scheduling; the explicit `FiberRuntime` gives
/// faithful cooperative interruption + uninterruptible masking. The three
/// channels are unchanged: 'A success,
/// 'E typed error, 'R environment. The `FiberRuntime` is internal — it is created
/// by the runners and never appears in the public surface.
type Effect<'A, 'E, 'R> = internal Effect of (FiberRuntime -> 'R -> Async<Exit<'A, 'E>>)

/// Raised by `runAsync`/`runSync` when an effect completes with a `Failure`
/// whose cause is a typed error (or a non-exception defect).
type EffectFailureException(cause: obj, render: string) =
    inherit System.Exception(render)
    member _.Cause = cause

[<RequireQualifiedAccess>]
module Effect =

    // --- internal helpers ---

    let private unwrap (ex: exn) : exn =
#if FABLE_COMPILER
        // Fable shim: no System.AggregateException wrapping exists on JS to unravel,
        // and the type-test is unsupported — identity is correct. (severity: trivial)
        ex
#else
        match ex with
        | :? System.AggregateException as agg when agg.InnerExceptions.Count = 1 -> agg.InnerException
        | _ -> ex
#endif

    let private retagDefects (cause: Cause<'E>) : Cause<'E2> =
        cause
        |> Cause.map (fun _ -> failwith "retagDefects: unexpected Fail reason in a defect-only cause")

    /// Should a pending interrupt be honored right now? (set AND not masked)
    let inline private interruptible (fib: FiberRuntime) = fib.Interrupted && fib.Mask = 0

    /// The cause produced when an interrupt is honored (root fiber id = none).
    let private interruptedCause<'E> : Cause<'E> = Cause.interrupt None

    // --- constructors ---

    let succeed (value: 'A) : Effect<'A, 'E, 'R> =
        Effect(fun _ _ -> async { return Success value })

    let fail (error: 'E) : Effect<'A, 'E, 'R> =
        Effect(fun _ _ -> async { return Exit.fail error })

    let failCause (cause: Cause<'E>) : Effect<'A, 'E, 'R> =
        Effect(fun _ _ -> async { return Failure cause })

    let sync (thunk: unit -> 'A) : Effect<'A, 'E, 'R> =
        Effect(fun _ _ ->
            async {
                try
                    return Success(thunk ())
                with ex ->
                    return Exit.die (box ex)
            })

    let fromResult (result: Result<'A, 'E>) : Effect<'A, 'E, 'R> =
        Effect(fun _ _ ->
            async {
                return
                    (match result with
                     | Ok a -> Success a
                     | Error e -> Exit.fail e)
            })

#if FABLE_COMPILER
    // Fable shim: Fable's runtime lacks Async.AwaitTask. Accept an Async thunk; JS
    // hosts that yield a Promise bridge it with Async.AwaitPromise at the call site.
    // (severity: refactor — signature differs on the JS target only)
    let promise (f: unit -> Async<'A>) : Effect<'A, 'E, 'R> =
        Effect(fun _ _ ->
            async {
                let! a = f ()
                return Success a
            })

    let tryPromise (f: unit -> Async<'A>) : Effect<'A, 'E, 'R> =
        Effect(fun _ _ ->
            async {
                try
                    let! a = f ()
                    return Success a
                with ex ->
                    return Exit.die (box (unwrap ex))
            })

    /// Lift a native JavaScript Promise into an Effect. This is the direct bridge
    /// for Fable npm interop (`Promise<'A>` → `Effect<'A, _, _>`).
    let promiseJS (promise: Fable.Core.JS.Promise<'A>) : Effect<'A, 'E, 'R> =
        Effect(fun _ _ ->
            async {
                let! a = Async.AwaitPromise promise
                return Success a
            })

    /// Lazily create and await a native JavaScript Promise.
    let promiseJSWith (f: unit -> Fable.Core.JS.Promise<'A>) : Effect<'A, 'E, 'R> =
        Effect(fun _ _ ->
            async {
                let! a = Async.AwaitPromise(f ())
                return Success a
            })

    /// Lazily create and await a native JavaScript Promise, mapping rejection
    /// into the typed error channel instead of a defect.
    let tryPromiseJS (f: unit -> Fable.Core.JS.Promise<'A>) (onError: exn -> 'E) : Effect<'A, 'E, 'R> =
        Effect(fun _ _ ->
            async {
                try
                    let! a = Async.AwaitPromise(f ())
                    return Success a
                with ex ->
                    return Exit.fail (onError (unwrap ex))
            })
#else
    let promise (f: unit -> Task<'A>) : Effect<'A, 'E, 'R> =
        Effect(fun _ _ ->
            async {
                let! a = Async.AwaitTask(f ())
                return Success a
            })

    let tryPromise (f: unit -> Task<'A>) : Effect<'A, 'E, 'R> =
        Effect(fun _ _ ->
            async {
                try
                    let! a = Async.AwaitTask(f ())
                    return Success a
                with ex ->
                    return Exit.die (box (unwrap ex))
            })

    /// JavaScript Promise interop is only executable under Fable/JS.
    let promiseJS (_promise: Fable.Core.JS.Promise<'A>) : Effect<'A, 'E, 'R> =
        Effect.sync (fun () -> failwith "Effect.promiseJS is only available on the Fable/JS target")

    /// JavaScript Promise interop is only executable under Fable/JS.
    let promiseJSWith (_f: unit -> Fable.Core.JS.Promise<'A>) : Effect<'A, 'E, 'R> =
        Effect.sync (fun () -> failwith "Effect.promiseJSWith is only available on the Fable/JS target")

    /// JavaScript Promise interop is only executable under Fable/JS.
    let tryPromiseJS (_f: unit -> Fable.Core.JS.Promise<'A>) (_onError: exn -> 'E) : Effect<'A, 'E, 'R> =
        Effect.sync (fun () -> failwith "Effect.tryPromiseJS is only available on the Fable/JS target")
#endif

    let environment<'R, 'E> : Effect<'R, 'E, 'R> =
        Effect(fun _ r -> async { return Success r })

    let environmentWith (f: 'R -> 'A) : Effect<'A, 'E, 'R> =
        Effect(fun _ r -> async { return Success(f r) })

    // --- combinators ---

    let map (f: 'A -> 'B) (Effect run) : Effect<'B, 'E, 'R> =
        Effect(fun fib r ->
            async {
                let! exit = run fib r

                return
                    (match exit with
                     | Success a -> Success(f a)
                     | Failure c -> Failure c)
            })

    let mapError (f: 'E -> 'E2) (Effect run) : Effect<'A, 'E2, 'R> =
        Effect(fun fib r ->
            async {
                let! exit = run fib r

                return
                    (match exit with
                     | Success a -> Success a
                     | Failure cause -> Failure(Cause.map f cause))
            })

    /// Sequence a dependent effect. A yield point: between the two steps a pending
    /// interrupt is honored (unless masked), short-circuiting to `Exit.Interrupt`.
    let flatMap (f: 'A -> Effect<'B, 'E, 'R>) (Effect run) : Effect<'B, 'E, 'R> =
        Effect(fun fib r ->
            async {
                let! exit = run fib r

                match exit with
                | Success a ->
                    if interruptible fib then
                        return Failure interruptedCause
                    else
                        let (Effect run2) = f a
                        return! run2 fib r
                | Failure c -> return Failure c
            })

    let catchAll (handler: 'E -> Effect<'A, 'E2, 'R>) (Effect run) : Effect<'A, 'E2, 'R> =
        Effect(fun fib r ->
            async {
                let! exit = run fib r

                match exit with
                | Success a -> return Success a
                | Failure cause ->
                    match Cause.failures cause |> List.tryHead with
                    | Some e ->
                        let (Effect run2) = handler e
                        return! run2 fib r
                    | None -> return Failure(retagDefects cause)
            })

    let zipRight (b: Effect<'B, 'E, 'R>) (a: Effect<'A, 'E, 'R>) : Effect<'B, 'E, 'R> = a |> flatMap (fun _ -> b)

    let zipWith (f: 'A -> 'B -> 'C) (b: Effect<'B, 'E, 'R>) (a: Effect<'A, 'E, 'R>) : Effect<'C, 'E, 'R> =
        a |> flatMap (fun av -> b |> map (fun bv -> f av bv))

    // --- fibers & interruption (slice: wave3 kernel) ---

    /// Run `eff` and capture its full `Exit` (success / typed failure / defect /
    /// interruption) as a *success* — the resulting effect never fails. The dual of
    /// `failCause`/`done`; lets a caller inspect an outcome without short-circuiting
    /// (e.g. a retry loop polling `exit (HttpClient.get url)` for readiness).
    /// (Effect.exit)
    let exit (Effect run) : Effect<Exit<'A, 'E>, 'F, 'R> =
        Effect(fun fib r ->
            async {
                let! ex = run fib r
                return Success ex
            })

    /// Run `eff` on a fresh fiber concurrently; returns a `Fiber` handle to
    /// await/join/interrupt. The parent is not affected by the child's outcome.
    /// (Effect.fork)
    let fork (Effect run) : Effect<Fiber<'A, 'E>, 'E2, 'R> =
        Effect(fun _ r ->
            async {
                let child = FiberRuntime.Root()
#if FABLE_COMPILER
                // Fable shim: Async.StartAsTask is unimplemented. Async.StartChild runs
                // the child on the event loop and yields an awaitable handle; a Result
                // cell records completion for poll/isLive. (severity: refactor)
                let cell = ref None

                let! handle =
                    Async.StartChild(
                        async {
                            let! e = run child r
                            cell.Value <- Some e
                            return e
                        }
                    )

                return
                    Success
                        { Join = handle
                          Result = cell
                          Runtime = child }
#else
                let task = Async.StartAsTask(run child r)
                return Success { Task = task; Runtime = child }
#endif
            })

    /// Wait for a fiber to complete and observe its full `Exit`. (Fiber.await)
    let await (fib: Fiber<'A, 'E>) : Effect<Exit<'A, 'E>, 'E2, 'R> =
        Effect(fun _ _ ->
            async {
#if FABLE_COMPILER
                let! e = fib.Join
#else
                let! e = Async.AwaitTask fib.Task
#endif
                return Success e
            })

    /// Wait for a fiber and fold its failure back into this effect. (Fiber.join)
    let join (fib: Fiber<'A, 'E>) : Effect<'A, 'E, 'R> =
#if FABLE_COMPILER
        Effect(fun _ _ -> async { return! fib.Join })
#else
        Effect(fun _ _ -> async { return! Async.AwaitTask fib.Task })
#endif

    /// Interrupt a fiber and wait until it has actually stopped (its finalizers
    /// run). Completes with `unit`. (Fiber.interrupt)
    let interrupt (fib: Fiber<'A, 'E>) : Effect<unit, 'E2, 'R> =
        Effect(fun _ _ ->
            async {
                fib.Runtime.Interrupted <- true
#if FABLE_COMPILER
                let! _ = fib.Join
#else
                let! _ = Async.AwaitTask fib.Task
#endif
                return Success()
            })

    /// Signal an interrupt to a fiber **without** waiting for it to stop: set the
    /// `Interrupted` flag and return immediately. The fiber honors it at its next
    /// yield point (or when its mask lifts). Unlike `interrupt`, this never blocks
    /// the caller, so it is safe to set the flag and then release a fiber that is
    /// parked waiting on the caller. (Fiber.interruptFork)
    let interruptFork (fib: Fiber<'A, 'E>) : Effect<unit, 'E2, 'R> =
        sync (fun () -> fib.Runtime.Interrupted <- true)

    /// Mark a region uninterruptible: a pending interrupt is deferred until the
    /// region exits, then honored. Finalizers run under this so they cannot be
    /// interrupted. (Effect.uninterruptible)
    let uninterruptible (Effect run) : Effect<'A, 'E, 'R> =
        Effect(fun fib r ->
            async {
                fib.Mask <- fib.Mask + 1
                let! exit = run fib r
                fib.Mask <- fib.Mask - 1

                if interruptible fib then
                    return Failure interruptedCause
                else
                    return exit
            })

    /// A cancellable delay — a yield point that honors interruption. (Effect.sleep)
    let sleep (milliseconds: int) : Effect<unit, 'E, 'R> =
        Effect(fun fib _ ->
            async {
#if FABLE_COMPILER
                // Fable shim: no System.Diagnostics.Stopwatch. Count down the remaining
                // millis instead — same cooperative poll-for-interrupt loop. (trivial)
                let mutable remaining = milliseconds
                let mutable interrupted = false

                while remaining > 0 && not interrupted do
                    if interruptible fib then
                        interrupted <- true
                    else
                        let step = min 5 remaining
                        do! Async.Sleep step
                        remaining <- remaining - step

                return (if interrupted then Failure interruptedCause else Success())
#else
                let sw = System.Diagnostics.Stopwatch.StartNew()
                let mutable interrupted = false

                while sw.ElapsedMilliseconds < int64 milliseconds && not interrupted do
                    if interruptible fib then
                        interrupted <- true
                    else
                        do! Async.Sleep 5

                return (if interrupted then Failure interruptedCause else Success())
#endif
            })

    // --- CE / resource support ---

    let suspend (f: unit -> Effect<'A, 'E, 'R>) : Effect<'A, 'E, 'R> =
        Effect(fun fib r ->
            async {
                let (Effect run) = f ()
                return! run fib r
            })

    /// Run `finalizer` after `eff` regardless of outcome. The finalizer runs
    /// **under mask** so an in-flight interrupt cannot skip it.
    let ensuring (finalizer: Effect<unit, 'E, 'R>) (Effect run) : Effect<'A, 'E, 'R> =
        Effect(fun fib r ->
            async {
                let! exit =
                    async {
                        try
                            return! run fib r
                        with ex ->
                            return Exit.die (box (unwrap ex))
                    }

                fib.Mask <- fib.Mask + 1
                let (Effect runFin) = finalizer

                let! finExit =
                    async {
                        try
                            return! runFin fib r
                        with ex ->
                            return Exit.die (box (unwrap ex))
                    }

                fib.Mask <- fib.Mask - 1

                return
                    match finExit with
                    | Success _ -> exit
                    | Failure fc ->
                        match exit with
                        | Failure c -> Failure(Cause.combine c fc)
                        | Success _ -> Failure fc
            })

    let forEach (items: seq<'T>) (f: 'T -> Effect<'B, 'E, 'R>) : Effect<'B list, 'E, 'R> =
        Effect(fun fib r ->
            async {
                let results = System.Collections.Generic.List<'B>()
                use en = items.GetEnumerator()
                let mutable failure = None

                while failure.IsNone && en.MoveNext() do
                    if interruptible fib then
                        failure <- Some interruptedCause
                    else
                        let (Effect run) = f en.Current
                        let! exit = run fib r

                        match exit with
                        | Success b -> results.Add b
                        | Failure c -> failure <- Some c

                return
                    (match failure with
                     | Some c -> Failure c
                     | None -> Success(List.ofSeq results))
            })

    let whileLoop (guard: unit -> bool) (body: Effect<unit, 'E, 'R>) : Effect<unit, 'E, 'R> =
        Effect(fun fib r ->
            async {
                let mutable result = Success()
                let mutable go = true

                while go && guard () do
                    if interruptible fib then
                        result <- Failure interruptedCause
                        go <- false
                    else
                        let (Effect run) = body
                        let! exit = run fib r

                        match exit with
                        | Success() -> ()
                        | Failure c ->
                            result <- Failure c
                            go <- false

                return result
            })

    let catchDefect (handler: exn -> Effect<'A, 'E, 'R>) (Effect run) : Effect<'A, 'E, 'R> =
        Effect(fun fib r ->
            async {
                let! exit = run fib r

                match exit with
                | Success a -> return Success a
                | Failure cause ->
                    match
                        cause.Reasons
                        |> List.tryPick (function
                            | Reason.Die(:? exn as ex) -> Some ex
                            | _ -> None)
                    with
                    | Some ex ->
                        let (Effect run2) = handler ex
                        return! run2 fib r
                    | None -> return Failure cause
            })

    let acquireUseRelease
        (acquire: Effect<'A, 'E, 'R>)
        (useResource: 'A -> Effect<'B, 'E, 'R>)
        (release: 'A -> Exit<'B, 'E> -> Effect<unit, 'E, 'R>)
        : Effect<'B, 'E, 'R> =
        Effect(fun fib r ->
            async {
                let (Effect runAcq) = acquire
                let! acqExit = runAcq fib r

                match acqExit with
                | Failure c -> return Failure c
                | Success a ->
                    let! useExit =
                        async {
                            try
                                let (Effect runUse) = useResource a
                                return! runUse fib r
                            with ex ->
                                return Exit.die (box (unwrap ex))
                        }

                    fib.Mask <- fib.Mask + 1
                    let (Effect runRel) = release a useExit

                    let! relExit =
                        async {
                            try
                                return! runRel fib r
                            with ex ->
                                return Exit.die (box (unwrap ex))
                        }

                    fib.Mask <- fib.Mask - 1

                    return
                        match relExit with
                        | Success _ -> useExit
                        | Failure fc ->
                            match useExit with
                            | Failure c -> Failure(Cause.combine c fc)
                            | Success _ -> Failure fc
            })

    // --- Context-based dependency injection ---

    let service (tag: Tag<'Service>) : Effect<'Service, 'E, Context> =
        Effect(fun _ ctx ->
            async {
                match Context.tryGet tag ctx with
                | Some s -> return Success s
                | None ->
                    return
                        Exit.die (
                            box (
                                System.Collections.Generic.KeyNotFoundException(
                                    sprintf "Service not found: %s" tag.Key
                                )
                            )
                        )
            })

    let provideContext (ctx: Context) (eff: Effect<'A, 'E, Context>) : Effect<'A, 'E, 'R> =
        let (Effect run) = eff
        Effect(fun fib _ -> run fib ctx)

    let provideService (tag: Tag<'Service>) (service: 'Service) (eff: Effect<'A, 'E, Context>) : Effect<'A, 'E, 'R> =
        provideContext (Context.make tag service) eff

    // --- runners (create the root fiber) ---

    let runExit (env: 'R) (Effect f) : Async<Exit<'A, 'E>> =
        async {
            try
                return! f (FiberRuntime.Root()) env
            with ex ->
                return Exit.die (box (unwrap ex))
        }

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

    let runSync (env: 'R) (eff: Effect<'A, 'E, 'R>) : Exit<'A, 'E> =
#if FABLE_COMPILER
        // Fable shim: Async.RunSynchronously cannot block JS's single-threaded event
        // loop. Kick the workflow off synchronously (Async.StartImmediate) and read the
        // result cell — for a fully-synchronous effect it is already populated; if the
        // effect actually suspended, raise. This is exactly Effect-TS's runSync contract
        // ("dies if the effect suspends"). (severity: refactor — JS-specific runner)
        let mutable result = None

        Async.StartImmediate(
            async {
                let! x = runExit env eff
                result <- Some x
            }
        )

        match result with
        | Some x -> x
        | None -> failwith "runSync: effect suspended asynchronously; use runPromiseExit on JS"
#else
        runExit env eff |> Async.RunSynchronously
#endif

/// Computation-expression builder: the `effect { ... }` do-notation.
type EffectBuilder() =
    member _.Return(value: 'A) : Effect<'A, 'E, 'R> = Effect.succeed value
    member _.ReturnFrom(eff: Effect<'A, 'E, 'R>) : Effect<'A, 'E, 'R> = eff
    member _.Bind(eff: Effect<'A, 'E, 'R>, f: 'A -> Effect<'B, 'E, 'R>) : Effect<'B, 'E, 'R> = Effect.flatMap f eff
    member _.Zero() : Effect<unit, 'E, 'R> = Effect.succeed ()
    member _.Delay(f: unit -> Effect<'A, 'E, 'R>) : Effect<'A, 'E, 'R> = Effect.suspend f
    member _.Combine(a: Effect<unit, 'E, 'R>, b: Effect<'B, 'E, 'R>) : Effect<'B, 'E, 'R> = Effect.zipRight b a

    member _.For(items: seq<'T>, f: 'T -> Effect<unit, 'E, 'R>) : Effect<unit, 'E, 'R> =
        Effect.forEach items f |> Effect.map ignore

    member _.While(guard: unit -> bool, body: Effect<unit, 'E, 'R>) : Effect<unit, 'E, 'R> = Effect.whileLoop guard body

    member _.TryWith(body: Effect<'A, 'E, 'R>, handler: exn -> Effect<'A, 'E, 'R>) : Effect<'A, 'E, 'R> =
        Effect.catchDefect handler body

    member _.TryFinally(body: Effect<'A, 'E, 'R>, compensation: unit -> unit) : Effect<'A, 'E, 'R> =
        Effect.ensuring (Effect.sync compensation) body

    member _.Using(resource: 'T :> System.IDisposable, body: 'T -> Effect<'A, 'E, 'R>) : Effect<'A, 'E, 'R> =
        Effect.ensuring
            (Effect.sync (fun () ->
                if not (obj.ReferenceEquals(resource, null)) then
                    resource.Dispose()))
            (body resource)

[<AutoOpen>]
module EffectBuilderModule =
    let effect = EffectBuilder()
