namespace Effect

/// Typed request values for data loading.
///
/// Port of repos/effect-smol/packages/effect/src/Request.ts. A `Request<'A,'E,'R>`
/// describes one logical piece of work — its success type `'A`, typed error `'E`,
/// and service requirements `'R` — without performing it. A `RequestResolver`
/// (not yet ported) later performs the work and completes each pending `Entry`
/// with a success, failure, cause, exit, or effect; this module ports that
/// completion surface.
///
/// Porting notes (see CONVENTIONS.md):
///   * `Request<'A,'E,'R>` is a phantom-typed marker *interface*. A concrete
///     request is any F# type that implements it (a record/union), which replaces
///     upstream's `of` / `tagged` / `Class` / `TaggedClass` object-prototype
///     builders — those exist only to synthesize tagged objects in a dynamically
///     typed runtime, which F#'s nominal types make unnecessary (§1, §3).
///   * `isRequest` (an `unknown`-narrowing guard), the `Variance` / `Constructor`
///     markers, the `RequestPrototype` value, and the type-level `Success` /
///     `Error` / `Services` / `Result` extractors are JS-runtime / type-level
///     plumbing and are omitted (§3, §4).
///   * The dual data-first/data-last completion signatures collapse to plain
///     curried, self-first functions.
/// A request for a value of type `'A` that may fail with `'E` and needs services
/// `'R`. Implemented (as an empty marker) by concrete request types. (Request.Request)
type Request<'A, 'E, 'R> = interface end

/// A pending request handed to a resolver: the original request, the captured
/// context, an `Uninterruptible` flag used by batching internals, and the
/// `CompleteUnsafe` callback a resolver calls with the final `Exit`. (Request.Entry)
type Entry<'A, 'E, 'R> =
    { Request: Request<'A, 'E, 'R>
      Context: Context
      mutable Uninterruptible: bool
      CompleteUnsafe: Exit<'A, 'E> -> unit }

[<RequireQualifiedAccess>]
module Request =

    /// Upstream brand constant (kept for parity).
    [<Literal>]
    let TypeId = "~effect/Request"

    /// Creates an `Entry` from its component fields. A low-level helper for
    /// resolver infrastructure. (Request.makeEntry)
    let makeEntry
        (request: Request<'A, 'E, 'R>)
        (context: Context)
        (uninterruptible: bool)
        (completeUnsafe: Exit<'A, 'E> -> unit)
        : Entry<'A, 'E, 'R> =
        { Request = request
          Context = context
          Uninterruptible = uninterruptible
          CompleteUnsafe = completeUnsafe }

    /// Completes an entry with a prebuilt `Exit`. (Request.complete)
    let complete (self: Entry<'A, 'E, 'R>) (result: Exit<'A, 'E>) : Effect<unit, 'E2, 'R2> =
        Effect.sync (fun () -> self.CompleteUnsafe result)

    /// Completes an entry successfully with `value`. (Request.succeed)
    let succeed (self: Entry<'A, 'E, 'R>) (value: 'A) : Effect<unit, 'E2, 'R2> = complete self (Exit.succeed value)

    /// Completes an entry with a typed failure. (Request.fail)
    let fail (self: Entry<'A, 'E, 'R>) (error: 'E) : Effect<unit, 'E2, 'R2> = complete self (Exit.fail error)

    /// Completes an entry with a failure `Cause`. (Request.failCause)
    let failCause (self: Entry<'A, 'E, 'R>) (cause: Cause<'E>) : Effect<unit, 'E2, 'R2> =
        complete self (Exit.failCause cause)

    /// Completes an entry from the result of an effect: success becomes a
    /// successful completion, a typed failure becomes a failed completion. The
    /// returned effect itself never fails with the request error. (Request.completeEffect)
    let completeEffect (self: Entry<'A, 'E, 'R>) (effect: Effect<'A, 'E, 'REnv>) : Effect<unit, 'E2, 'REnv> =
        effect
        |> Effect.map Exit.succeed
        |> Effect.catchAll (fun error -> Effect.succeed (Exit.fail error))
        |> Effect.flatMap (complete self)
