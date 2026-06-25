namespace Effect

/// Reusable strategies for reducing many values into one value.
///
/// Port of repos/effect-smol/packages/effect/src/Reducer.ts. A `Reducer<'A>`
/// extends `Combiner` with an `initialValue` (the identity/neutral element) and
/// a `combineAll` that folds an entire collection from `initialValue`.
///
/// Porting notes (see CONVENTIONS.md):
///   * F# records do not support interface-style extension, so `Reducer<'A>`
///     re-declares `combine` (rather than inheriting `Combiner`). Use
///     `Reducer.toCombiner` to view a reducer as a `Combiner`.
///   * Upstream `make`'s optional `combineAll` parameter is split into two
///     constructors: `make` (default left-to-right fold) and `makeWith` (custom
///     `combineAll`, e.g. short-circuiting). This is more idiomatic than an
///     `option` argument.
type Reducer<'A> =
    {
        /// Combines two values into a new value.
        combine: 'A -> 'A -> 'A
        /// Neutral starting value (combining with it changes nothing).
        initialValue: 'A
        /// Combines all values in the collection, starting from `initialValue`.
        combineAll: seq<'A> -> 'A
    }

[<RequireQualifiedAccess>]
module Reducer =

    /// Creates a `Reducer` from a `combine` function and `initialValue`, using a
    /// default left-to-right fold for `combineAll`.
    let make (combine: 'a -> 'a -> 'a) (initialValue: 'a) : Reducer<'a> =
        { combine = combine
          initialValue = initialValue
          combineAll = fun collection -> Seq.fold combine initialValue collection }

    /// Creates a `Reducer` with an explicit `combineAll` (e.g. one that
    /// short-circuits on an absorbing element). It replaces the default fold.
    let makeWith (combine: 'a -> 'a -> 'a) (initialValue: 'a) (combineAll: seq<'a> -> 'a) : Reducer<'a> =
        { combine = combine
          initialValue = initialValue
          combineAll = combineAll }

    /// Reverses the argument order of a reducer's `combine`. `initialValue` is
    /// preserved; `combineAll` is re-derived from the flipped `combine`.
    let flip (reducer: Reducer<'a>) : Reducer<'a> =
        make (fun self that -> reducer.combine that self) reducer.initialValue

    /// Views a reducer as a `Combiner` (drops `initialValue`/`combineAll`).
    let toCombiner (reducer: Reducer<'a>) : Combiner<'a> = Combiner.make reducer.combine
