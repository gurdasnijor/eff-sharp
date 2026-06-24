namespace Effect

/// Port of repos/effect-smol/packages/effect/src/Filter.ts.
///
/// A `Filter<'Input,'Pass,'Fail>` is a function `'Input -> Result<'Pass,'Fail>`:
/// `Ok pass` means the value passed (possibly narrowed/transformed) and
/// `Error fail` means it was filtered out. Upstream's `Result.succeed`/`fail`
/// map to FSharp.Core `Ok`/`Error`, so no separate `Result` module is needed.
///
/// House style: combinators are subject-LAST so they compose under `|>`
/// (`left |> Filter.compose right`).
///
/// Omissions (noted per CONVENTIONS.md — JS-runtime-specific guards that don't
/// translate to F#'s static types, or a numeric guard that would be ambiguous):
///   * `equalsStrict` (JS `===` reference identity — structural `equals` is the
///     native analog), `has`, `instanceOf`, `tagged`, `reason` (JS `_tag` /
///     `instanceof` / duck-typed `has`), `number`/`bigint`/`symbol`/`date`
///     (`number` is ambiguous across .NET int/float/decimal; the others are
///     JS primitive guards). `string`/`boolean` are kept as representative
///     `obj` narrowings.
///   * The effectful `FilterEffect`/`makeEffect` variant (deferred; it only
///     wraps the pure filter in an `Effect`).
type Filter<'Input, 'Pass, 'Fail> = 'Input -> Result<'Pass, 'Fail>

[<RequireQualifiedAccess>]
module Filter =

    // --- constructors ---

    /// A filter from a function returning `Ok pass` / `Error fail`.
    let make (f: 'Input -> Result<'Pass, 'Fail>) : Filter<'Input, 'Pass, 'Fail> = f

    /// A filter that applies `f`, passing its result, or failing with the input
    /// if `f` throws (upstream `try`; `try` is an F# keyword).
    let tryCatch (f: 'Input -> 'Output) : Filter<'Input, 'Output, 'Input> =
        fun input ->
            try Ok(f input)
            with _ -> Error input

    /// A filter from a predicate: passes the input unchanged when true, else
    /// fails with the input.
    let fromPredicate (predicate: 'A -> bool) : Filter<'A, 'A, 'A> =
        fun input -> if predicate input then Ok input else Error input

    /// A filter from an option-returning function: `Some b` passes with `b`,
    /// `None` fails with the original input.
    let fromPredicateOption (predicate: 'A -> 'B option) : Filter<'A, 'B, 'A> =
        fun input ->
            match predicate input with
            | Some b -> Ok b
            | None -> Error input

    /// A filter that passes values structurally equal to `value` (upstream
    /// `equals`, which delegates to `Equal.equals`; here FSharp.Core `=`).
    let equals (value: 'A) : Filter<'A, 'A, 'A> =
        fun input -> if input = value then Ok value else Error input

    /// Passes only `string` inputs (upstream `Filter.string`).
    let string: Filter<obj, string, obj> =
        fun u ->
            match u with
            | :? string as s -> Ok s
            | _ -> Error u

    /// Passes only `bool` inputs (upstream `Filter.boolean`).
    let boolean: Filter<obj, bool, obj> =
        fun u ->
            match u with
            | :? bool as b -> Ok b
            | _ -> Error u

    // --- mapping ---

    /// Transform the failure value, leaving passes unchanged.
    let mapFail (f: 'Fail -> 'Fail2) (self: Filter<'Input, 'Pass, 'Fail>) : Filter<'Input, 'Pass, 'Fail2> =
        fun input -> self input |> Result.mapError f

    // --- combinators (subject-last) ---

    /// Logical OR: the left filter's pass, or the right filter's result.
    let orElse (that: Filter<'Input, 'Pass, 'Fail2>) (self: Filter<'Input, 'Pass, 'Fail1>) : Filter<'Input, 'Pass, 'Fail2> =
        fun input ->
            match self input with
            | Ok p -> Ok p
            | Error _ -> that input

    /// Both filters must pass; their passes are combined with `f`. Fails with
    /// the first failure. (Both sides share a `'Fail` type, simplifying
    /// upstream's `FailL | FailR` union.)
    let zipWith
        (right: Filter<'Input, 'PassR, 'Fail>)
        (f: 'PassL -> 'PassR -> 'A)
        (left: Filter<'Input, 'PassL, 'Fail>)
        : Filter<'Input, 'A, 'Fail> =
        fun input ->
            match left input with
            | Error e -> Error e
            | Ok l ->
                match right input with
                | Error e -> Error e
                | Ok r -> Ok(f l r)

    /// Both filters must pass; their passes are combined into a tuple.
    let zip (right: Filter<'Input, 'PassR, 'Fail>) (left: Filter<'Input, 'PassL, 'Fail>) : Filter<'Input, 'PassL * 'PassR, 'Fail> =
        zipWith right (fun l r -> l, r) left

    /// Both must pass; keeps the left filter's pass.
    let andLeft (right: Filter<'Input, 'PassR, 'Fail>) (left: Filter<'Input, 'PassL, 'Fail>) : Filter<'Input, 'PassL, 'Fail> =
        zipWith right (fun l _ -> l) left

    /// Both must pass; keeps the right filter's pass.
    let andRight (right: Filter<'Input, 'PassR, 'Fail>) (left: Filter<'Input, 'PassL, 'Fail>) : Filter<'Input, 'PassR, 'Fail> =
        zipWith right (fun _ r -> r) left

    /// Sequential composition: feed the left filter's pass into the right.
    let compose (right: Filter<'PassL, 'PassR, 'Fail>) (left: Filter<'Input, 'PassL, 'Fail>) : Filter<'Input, 'PassR, 'Fail> =
        fun input ->
            match left input with
            | Error e -> Error e
            | Ok l -> right l

    /// Like `compose`, but either failure fails with the **original input**
    /// rather than the intermediate failure value.
    let composePassthrough
        (right: Filter<'PassL, 'PassR, 'FailR>)
        (left: Filter<'Input, 'PassL, 'FailL>)
        : Filter<'Input, 'PassR, 'Input> =
        fun input ->
            match left input with
            | Error _ -> Error input
            | Ok l ->
                match right l with
                | Error _ -> Error input
                | Ok r -> Ok r

    // --- converting ---

    /// `true` for passing inputs, `false` for filtered-out inputs.
    let toPredicate (self: Filter<'A, 'Pass, 'Fail>) : 'A -> bool =
        fun input ->
            match self input with
            | Ok _ -> true
            | Error _ -> false

    /// `Some pass` for passing inputs, `None` for filtered-out inputs.
    let toOption (self: Filter<'A, 'Pass, 'Fail>) : 'A -> 'Pass option =
        fun input ->
            match self input with
            | Ok p -> Some p
            | Error _ -> None

    /// The underlying `Result` for each input (a `Filter` already is one).
    let toResult (self: Filter<'A, 'Pass, 'Fail>) : 'A -> Result<'Pass, 'Fail> = self
