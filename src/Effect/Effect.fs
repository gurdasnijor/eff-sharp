namespace Effect

/// Native F# port of Effect's core `Effect<A, E, R>` type.
///
/// Slice 1: a synchronous Reader-over-Result encoding. It carries all three of
/// Effect's channels:
///   'A  - success value
///   'E  - typed (expected) error
///   'R  - required environment / dependencies (DI)
///
/// Async, fibers, interruption, Cause/Exit and Layer arrive in later slices.
/// The public surface (succeed/fail/sync/map/flatMap/...) is kept deliberately
/// close to Effect's so later slices can swap the internal representation
/// without churning call sites.
type Effect<'A, 'E, 'R> = internal Effect of ('R -> Result<'A, 'E>)

[<RequireQualifiedAccess>]
module Effect =

    /// Run an effect against its required environment, returning the
    /// success/error as an F# `Result`. (Effect.runSync analogue.)
    let run (env: 'R) (Effect f) : Result<'A, 'E> = f env

    // --- constructors ---

    /// An effect that always succeeds with `value`. (Effect.succeed)
    let succeed (value: 'A) : Effect<'A, 'E, 'R> = Effect(fun _ -> Ok value)

    /// An effect that always fails with the typed error `error`. (Effect.fail)
    let fail (error: 'E) : Effect<'A, 'E, 'R> = Effect(fun _ -> Error error)

    /// An effect from a thunk that cannot fail in the typed channel. (Effect.sync)
    let sync (thunk: unit -> 'A) : Effect<'A, 'E, 'R> = Effect(fun _ -> Ok(thunk ()))

    /// Lift an F# Result directly into an effect.
    let fromResult (result: Result<'A, 'E>) : Effect<'A, 'E, 'R> = Effect(fun _ -> result)

    /// Access the whole environment. (Effect.context / service root)
    let environment<'R, 'E> : Effect<'R, 'E, 'R> = Effect(fun r -> Ok r)

    /// Project a value out of the environment. (Effect.map over context)
    let environmentWith (f: 'R -> 'A) : Effect<'A, 'E, 'R> = Effect(fun r -> Ok(f r))

    // --- combinators ---

    /// Transform the success value. (Effect.map)
    let map (f: 'A -> 'B) (Effect run) : Effect<'B, 'E, 'R> =
        Effect(fun r -> run r |> Result.map f)

    /// Transform the typed error. (Effect.mapError)
    let mapError (f: 'E -> 'E2) (Effect run) : Effect<'A, 'E2, 'R> =
        Effect(fun r -> run r |> Result.mapError f)

    /// Sequence a dependent effect. (Effect.flatMap)
    let flatMap (f: 'A -> Effect<'B, 'E, 'R>) (Effect run) : Effect<'B, 'E, 'R> =
        Effect(fun r ->
            match run r with
            | Ok a -> let (Effect run2) = f a in run2 r
            | Error e -> Error e)

    /// Recover from a typed error. (Effect.catchAll)
    let catchAll (handler: 'E -> Effect<'A, 'E2, 'R>) (Effect run) : Effect<'A, 'E2, 'R> =
        Effect(fun r ->
            match run r with
            | Ok a -> Ok a
            | Error e -> let (Effect run2) = handler e in run2 r)

    /// Run `b`, ignoring `a`'s value but keeping its effect. (Effect.zipRight)
    let zipRight (b: Effect<'B, 'E, 'R>) (a: Effect<'A, 'E, 'R>) : Effect<'B, 'E, 'R> =
        a |> flatMap (fun _ -> b)

    /// Combine two effects' values with `f`. (Effect.zipWith)
    let zipWith (f: 'A -> 'B -> 'C) (b: Effect<'B, 'E, 'R>) (a: Effect<'A, 'E, 'R>) : Effect<'C, 'E, 'R> =
        a |> flatMap (fun av -> b |> map (fun bv -> f av bv))

/// Computation-expression builder: the `effect { ... }` do-notation,
/// analogous to `Effect.gen(function* () { ... })`.
type EffectBuilder() =
    member _.Return(value: 'A) : Effect<'A, 'E, 'R> = Effect.succeed value
    member _.ReturnFrom(eff: Effect<'A, 'E, 'R>) : Effect<'A, 'E, 'R> = eff
    member _.Bind(eff: Effect<'A, 'E, 'R>, f: 'A -> Effect<'B, 'E, 'R>) : Effect<'B, 'E, 'R> =
        Effect.flatMap f eff
    member _.Zero() : Effect<unit, 'E, 'R> = Effect.succeed ()

[<AutoOpen>]
module EffectBuilderModule =
    /// `effect { let! x = ...; return ... }`
    let effect = EffectBuilder()
