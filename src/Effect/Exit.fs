namespace Effect

/// Slice 2: `Exit` — the result of running an effect to completion.
///
/// Port of repos/effect-smol/packages/effect/src/Exit.ts. An `Exit<'A,'E>` is
/// either a `Success` carrying the value, or a `Failure` carrying the `Cause`
/// of the failure. It is the bridge between the running world (`Effect`) and a
/// fully-evaluated outcome.
/// The completed outcome of an effect.
type Exit<'A, 'E> =
    | Success of value: 'A
    | Failure of cause: Cause<'E>

[<RequireQualifiedAccess>]
module Exit =

    // --- constructors ---

    /// A successful exit. (Exit.succeed)
    let succeed (value: 'A) : Exit<'A, 'E> = Success value

    /// A failed exit from a typed error. (Exit.fail)
    let fail (error: 'E) : Exit<'A, 'E> = Failure(Cause.fail error)

    /// A failed exit from a defect. (Exit.die)
    let die (defect: obj) : Exit<'A, 'E> = Failure(Cause.die defect)

    /// A failed exit from fiber interruption. (Exit.interrupt)
    let interrupt (fiberId: int option) : Exit<'A, 'E> = Failure(Cause.interrupt fiberId)

    /// A failed exit from an arbitrary cause. (Exit.failCause)
    let failCause (cause: Cause<'E>) : Exit<'A, 'E> = Failure cause

    // --- guards ---

    let isSuccess (exit: Exit<'A, 'E>) =
        match exit with
        | Success _ -> true
        | Failure _ -> false

    let isFailure (exit: Exit<'A, 'E>) =
        match exit with
        | Failure _ -> true
        | Success _ -> false

    // --- elimination ---

    /// Fold an exit into a single value.
    let matchExit (onFailure: Cause<'E> -> 'B) (onSuccess: 'A -> 'B) (exit: Exit<'A, 'E>) : 'B =
        match exit with
        | Success a -> onSuccess a
        | Failure c -> onFailure c

    /// Bridge to/from an F# `Result`, collapsing a failure to its first typed
    /// error (defects/interrupts have no `Result` representation).
    let toResult (exit: Exit<'A, 'E>) : Result<'A, 'E option> =
        match exit with
        | Success a -> Ok a
        | Failure c -> Error(Cause.failures c |> List.tryHead)

    // --- rendering (matches Effect's toString) ---

    /// `Success(1)` / `Failure(Cause([Fail("error")]))`
    let render (exit: Exit<'A, 'E>) : string =
        match exit with
        | Success a -> sprintf "Success(%s)" (Cause.renderValue (box a))
        | Failure c -> sprintf "Failure(%s)" (Cause.render c)
