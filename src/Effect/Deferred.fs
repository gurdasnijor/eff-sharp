namespace Effect

open System.Threading.Tasks

/// Wave 2: `Deferred` — a one-shot, awaitable completion cell.
///
/// Port of repos/effect-smol/packages/effect/src/Deferred.ts. A `Deferred<'A,'E>`
/// starts empty, can be completed **exactly once** (with a success, typed
/// failure, defect, or interruption), and lets any number of waiters observe the
/// same completion. Awaiting an incomplete `Deferred` suspends the calling
/// `Effect` (via `Async`) rather than blocking an OS thread.
///
/// Representation — backed by a `System.Threading.Tasks.TaskCompletionSource`
/// bridged into the async core, exactly as the brief prescribes. The slot does
/// not store a bare `Exit`; it stores the *completion `Effect`* upstream keeps in
/// `self.effect`. This is what makes `completeWith` non-memoizing: each awaiter
/// re-runs the stored effect (see `completeWith` tests), while `complete`/`done`
/// store an already-evaluated `Exit` reproduced as an effect. `TrySetResult`
/// gives the one-shot, thread-safe "first writer wins" semantics for free, and
/// returns the `true`/`false` that every completion combinator reports.
///
/// Omissions vs upstream (per CONVENTIONS): the `isDeferred` runtime guard, the
/// HKT/`Variance` plumbing and the `TypeId` brand are dropped (F#'s static types
/// make them dead weight). `interrupt` uses `None` for the fiber id because this
/// wave does not own `Fiber`; `interruptWith` carries the explicit id.
type Deferred<'A, 'E> =
    internal
        { Source: TaskCompletionSource<Effect<'A, 'E, unit>> }

[<RequireQualifiedAccess>]
module Deferred =

    // --- internal helpers ---

    /// Reproduce an already-computed `Exit` as a completion effect. Awaiters that
    /// run this observe the same outcome every time (the memoizing path).
    let private ofExit (exit: Exit<'A, 'E>) : Effect<'A, 'E, unit> =
        match exit with
        | Success a -> Effect.succeed a
        | Failure c -> Effect.failCause c

    // --- unsafe / synchronous core ---

    /// Allocate an empty `Deferred` synchronously, outside the `Effect` runtime.
    /// (Deferred.makeUnsafe) Continuations resume asynchronously so a completer
    /// never runs an awaiter's continuation inline on its own stack.
    let makeUnsafe<'A, 'E> () : Deferred<'A, 'E> =
        { Source = TaskCompletionSource<Effect<'A, 'E, unit>>(TaskCreationOptions.RunContinuationsAsynchronously) }

    /// Whether the `Deferred` has already been completed. (Deferred.isDoneUnsafe)
    let isDoneUnsafe (self: Deferred<'A, 'E>) : bool = self.Source.Task.IsCompleted

    /// Attempt to complete the `Deferred` with a completion effect directly.
    /// Returns `true` if this call completed it, `false` if it was already
    /// completed. (Deferred.doneUnsafe)
    let doneUnsafe (self: Deferred<'A, 'E>) (effect: Effect<'A, 'E, unit>) : bool = self.Source.TrySetResult effect

    // --- constructors ---

    /// Allocate an empty `Deferred` inside an `Effect`. (Deferred.make)
    let make<'A, 'E> () = Effect.sync (fun () -> makeUnsafe<'A, 'E> ())

    // --- await ---

    /// Retrieve the value of the `Deferred`, suspending until it is completed.
    /// Every awaiter observes the stored completion effect. (Deferred.await)
    let await (self: Deferred<'A, 'E>) : Effect<'A, 'E, 'R> =
        Effect(fun _ ->
            async {
                let! completion = Async.AwaitTask self.Source.Task
                let (Effect run) = completion
                return! run ()
            })

    // --- completion (each returns `true` iff this call completed it) ---

    /// Store a completion `Effect` directly, **without** running or memoizing it;
    /// each awaiter runs the stored effect independently. (Deferred.completeWith)
    let completeWith (self: Deferred<'A, 'E>) (effect: Effect<'A, 'E, unit>) : Effect<bool, 'F, 'R> =
        Effect.sync (fun () -> doneUnsafe self effect)

    /// Complete from an already-computed `Exit`. (Deferred.done)
    let ``done`` (self: Deferred<'A, 'E>) (exit: Exit<'A, 'E>) : Effect<bool, 'F, 'R> = completeWith self (ofExit exit)

    /// Complete with a success value. (Deferred.succeed)
    let succeed (self: Deferred<'A, 'E>) (value: 'A) : Effect<bool, 'F, 'R> = ``done`` self (Success value)

    /// Complete with a typed error. (Deferred.fail)
    let fail (self: Deferred<'A, 'E>) (error: 'E) : Effect<bool, 'F, 'R> = ``done`` self (Exit.fail error)

    /// Complete with a full failure cause. (Deferred.failCause)
    let failCause (self: Deferred<'A, 'E>) (cause: Cause<'E>) : Effect<bool, 'F, 'R> = ``done`` self (Failure cause)

    /// Complete with an unexpected defect. (Deferred.die)
    let die (self: Deferred<'A, 'E>) (defect: obj) : Effect<bool, 'F, 'R> = ``done`` self (Exit.die defect)

    /// Complete as interrupted by the given fiber id. (Deferred.interruptWith)
    let interruptWith (self: Deferred<'A, 'E>) (fiberId: int) : Effect<bool, 'F, 'R> =
        ``done`` self (Exit.interrupt (Some fiberId))

    /// Complete as interrupted. This wave does not own `Fiber`, so no current
    /// fiber id is available and `None` is used. (Deferred.interrupt)
    let interrupt (self: Deferred<'A, 'E>) : Effect<bool, 'F, 'R> = ``done`` self (Exit.interrupt None)

    /// Lazily compute a success value when this effect runs, then complete with
    /// it. (Deferred.sync)
    let sync (self: Deferred<'A, 'E>) (evaluate: unit -> 'A) : Effect<bool, 'F, 'R> =
        Effect.sync (fun () -> doneUnsafe self (Effect.succeed (evaluate ())))

    /// Lazily compute a typed error when this effect runs, then complete with it.
    /// (Deferred.failSync)
    let failSync (self: Deferred<'A, 'E>) (evaluate: unit -> 'E) : Effect<bool, 'F, 'R> =
        Effect.sync (fun () -> doneUnsafe self (Effect.fail (evaluate ())))

    // --- completion from a running effect ---

    /// Run `effect`, then attempt to complete `self` with its (memoized) result —
    /// success, typed failure, or defect all flow through. Cannot fail; succeeds
    /// with `true` if it completed the `Deferred`, else `false`. Short-circuits
    /// without running `effect` if already completed. (Deferred.complete)
    let complete (self: Deferred<'A, 'E>) (effect: Effect<'A, 'E, 'R>) : Effect<bool, 'F, 'R> =
        Effect(fun r ->
            async {
                match isDoneUnsafe self with
                | true -> return Success false
                | false ->
                    let! exit = Effect.runExit r effect
                    return Success(doneUnsafe self (ofExit exit))
            })

    /// `complete` with the arguments flipped, for piping an effect into a
    /// `Deferred`: `effect |> Deferred.into deferred`. (Deferred.into)
    let into (self: Deferred<'A, 'E>) (effect: Effect<'A, 'E, 'R>) : Effect<bool, 'F, 'R> = complete self effect

    // --- inspection ---

    /// Whether the `Deferred` has already been completed. (Deferred.isDone)
    let isDone (self: Deferred<'A, 'E>) : Effect<bool, 'F, 'R> = Effect.sync (fun () -> isDoneUnsafe self)

    /// The stored completion effect as an `option`: `Some` when completed, `None`
    /// otherwise. (Deferred.poll — Effect's `Option` is lowered to F# `option`.)
    let poll (self: Deferred<'A, 'E>) : Effect<Effect<'A, 'E, unit> option, 'F, 'R> =
        Effect.sync (fun () ->
            match isDoneUnsafe self with
            | true -> Some self.Source.Task.Result
            | false -> None)
