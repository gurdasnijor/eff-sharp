namespace Effect

/// Slice 2: `Cause` — the full reason an effect failed.
///
/// Port of repos/effect-smol/packages/effect/src/Cause.ts. A `Cause` is a list
/// of `Reason`s, each being one of:
///   Fail      - a typed, expected error of type 'E
///   Die       - an unexpected defect (Effect models this as `unknown`; here a
///               boxed value / exception)
///   Interrupt - fiber interruption, carrying an optional fiber id
///
/// The JS runtime type-guards (`isCause`/`isReason` over arbitrary values) are
/// intentionally omitted: they exist because TS is structurally typed at
/// runtime, whereas F#'s static types make them unnecessary. The reason-level
/// guards (`isFailReason` etc.) are kept since they narrow within a `Cause`.

/// A single reason contributing to a `Cause`.
[<RequireQualifiedAccess>]
type Reason<'E> =
    | Fail of error: 'E
    | Die of defect: obj
    | Interrupt of fiberId: int option

/// The aggregate of all reasons an effect failed.
type Cause<'E> = { Reasons: Reason<'E> list }

[<RequireQualifiedAccess>]
module Cause =

    /// Upstream brand constants (kept for parity with Cause.test.ts).
    [<Literal>]
    let TypeId = "~effect/Cause"

    [<Literal>]
    let ReasonTypeId = "~effect/Cause/Reason"

    // --- reason constructors ---

    let makeFailReason (error: 'E) : Reason<'E> = Reason.Fail error
    let makeDieReason (defect: obj) : Reason<'E> = Reason.Die defect
    let makeInterruptReason (fiberId: int option) : Reason<'E> = Reason.Interrupt fiberId

    // --- cause constructors ---

    /// A cause with no reasons.
    let empty<'E> : Cause<'E> = { Reasons = [] }

    /// A cause from a single typed failure.
    let fail (error: 'E) : Cause<'E> = { Reasons = [ Reason.Fail error ] }

    /// A cause from a single defect.
    let die (defect: obj) : Cause<'E> = { Reasons = [ Reason.Die defect ] }

    /// A cause from a single fiber interruption.
    let interrupt (fiberId: int option) : Cause<'E> = { Reasons = [ Reason.Interrupt fiberId ] }

    // --- reason guards ---

    let isFailReason (reason: Reason<'E>) = match reason with Reason.Fail _ -> true | _ -> false
    let isDieReason (reason: Reason<'E>) = match reason with Reason.Die _ -> true | _ -> false
    let isInterruptReason (reason: Reason<'E>) = match reason with Reason.Interrupt _ -> true | _ -> false

    // --- projections ---

    /// The typed failures within a cause, in order.
    let failures (cause: Cause<'E>) : 'E list =
        cause.Reasons |> List.choose (function Reason.Fail e -> Some e | _ -> None)

    /// The defects within a cause, in order.
    let defects (cause: Cause<'E>) : obj list =
        cause.Reasons |> List.choose (function Reason.Die d -> Some d | _ -> None)

    // --- transformations ---

    /// Transform the typed errors within a cause, leaving Die/Interrupt reasons
    /// untouched. A cause with no `Fail` reason is returned with its reasons
    /// intact (only the phantom error type changes). (Cause.map — upstream
    /// `causeMap`.)
    let map (f: 'E -> 'E2) (cause: Cause<'E>) : Cause<'E2> =
        { Reasons =
            cause.Reasons
            |> List.map (fun reason ->
                match reason with
                | Reason.Fail e -> Reason.Fail(f e)
                | Reason.Die d -> Reason.Die d
                | Reason.Interrupt id -> Reason.Interrupt id) }

    /// Merge two causes: `first`'s reasons followed by `second`'s, de-duplicated
    /// by value equality (upstream `Arr.union`). Combining with `empty` returns
    /// the other cause unchanged. (Cause.combine — upstream `causeCombine`.)
    let combine (first: Cause<'E>) (second: Cause<'E>) : Cause<'E> =
        match first.Reasons, second.Reasons with
        | [], _ -> second
        | _, [] -> first
        | a, b ->
            // Value-equality union without imposing a static `equality`
            // constraint on 'E, so `combine` stays usable for any error type
            // (upstream dedups via runtime equality). Causes are tiny, so the
            // quadratic scan is irrelevant.
            let union =
                (a @ b)
                |> List.fold
                    (fun acc reason ->
                        if acc |> List.exists (fun seen -> Unchecked.equals seen reason) then
                            acc
                        else
                            acc @ [ reason ])
                    []

            { Reasons = union }

    // --- rendering (matches Effect's toString) ---

    /// Render a value the way Effect's toString does: strings quoted, null as
    /// `undefined`, everything else via its own string form.
    let internal renderValue (value: obj) : string =
        match value with
        | null -> "undefined"
        | :? string as s -> "\"" + s + "\""
        | other -> string other

    let internal renderReason (reason: Reason<'E>) : string =
        match reason with
        | Reason.Fail e -> sprintf "Fail(%s)" (renderValue (box e))
        | Reason.Die d -> sprintf "Die(%s)" (renderValue d)
        | Reason.Interrupt id ->
            let inner = match id with Some i -> string i | None -> "undefined"
            sprintf "Interrupt(%s)" inner

    /// `Cause([Fail("error"), ...])`
    let render (cause: Cause<'E>) : string =
        cause.Reasons |> List.map renderReason |> String.concat ", " |> sprintf "Cause([%s])"
