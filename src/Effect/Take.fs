namespace Effect

/// Port of repos/effect-smol/packages/effect/src/Take.ts.
///
/// A `Take<'A,'E,'Done>` is the stored representation of one pull result from a
/// stream-like producer: either a non-empty batch of emitted values, or an
/// `Exit` (a failure, or a successful completion carrying a `Done` value).
///
/// Upstream models this as the untagged union `NonEmptyReadonlyArray<A> |
/// Exit<Done, E>`; F# has no anonymous union, so it is a two-case DU. The
/// emitted batch is a plain `'A list` (expected non-empty, mirroring upstream's
/// `NonEmptyReadonlyArray`).
type Take<'A, 'E, 'Done> =
    /// A non-empty batch of emitted values.
    | Emit of values: 'A list
    /// A failure, or a successful completion carrying the `Done` value.
    | End of exit: Exit<'Done, 'E>

[<RequireQualifiedAccess>]
module Take =

    /// A take of a single emitted value.
    let single (value: 'A) : Take<'A, 'E, 'Done> = Emit [ value ]

    /// A take of a non-empty batch of values.
    let chunk (values: 'A list) : Take<'A, 'E, 'Done> = Emit values

    /// A completion take carrying the `Done` value.
    let endWith (doneValue: 'Done) : Take<'A, 'E, 'Done> = End(Exit.succeed doneValue)

    /// A failure take from a cause.
    let failCause (cause: Cause<'E>) : Take<'A, 'E, 'Done> = End(Exit.failCause cause)

    /// Converts a `Take` into a `Pull` step: emitted batches succeed, failure
    /// exits fail, and successful exits become pull completion (`Done`).
    let toPull (take: Take<'A, 'E, 'Done>) : Pull.Pull<'A list, 'R> =
        match take with
        | Emit values -> Effect.succeed values
        | End exit ->
            match exit with
            | Success doneValue -> Pull.halt doneValue
            | Failure cause -> Effect.failCause (Cause.map box cause)
