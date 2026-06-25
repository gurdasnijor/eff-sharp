namespace Effect

/// Reusable rules for merging two values of the same type.
///
/// Port of repos/effect-smol/packages/effect/src/Combiner.ts. A `Combiner<'A>`
/// carries a single `combine` operation that merges two values into one. It does
/// not define an identity/empty value — use `Reducer` when you need that.
///
/// Porting notes (see CONVENTIONS.md):
///   * The TS `interface Combiner<A>` is modelled as an F# record holding the
///     single (curried) `combine` function — idiomatic and structurally usable.
///   * `combine` is curried (`'A -> 'A -> 'A`) rather than tupled, matching the
///     rest of the port (cf. `Order`).
type Combiner<'A> =
    {
        /// Combines two values into a new value.
        combine: 'A -> 'A -> 'A
    }

[<RequireQualifiedAccess>]
module Combiner =

    /// Creates a `Combiner` from a binary function.
    let make (combine: 'a -> 'a -> 'a) : Combiner<'a> = { combine = combine }

    /// Reverses the argument order of a combiner's `combine`.
    let flip (combiner: Combiner<'a>) : Combiner<'a> =
        make (fun self that -> combiner.combine that self)

    /// A combiner that returns the smaller of two values per the given `Order`.
    /// When values compare equal it returns `that` (the second argument).
    let min (order: Order<'a>) : Combiner<'a> =
        make (fun self that -> if order self that = -1 then self else that)

    /// A combiner that returns the larger of two values per the given `Order`.
    /// When values compare equal it returns `that` (the second argument).
    let max (order: Order<'a>) : Combiner<'a> =
        make (fun self that -> if order self that = 1 then self else that)

    /// A combiner that always returns the first (left) argument.
    let first<'a> () : Combiner<'a> = make (fun self _ -> self)

    /// A combiner that always returns the last (right) argument.
    let last<'a> () : Combiner<'a> = make (fun _ that -> that)

    /// A combiner that ignores both arguments and always returns `a`.
    let constant (a: 'a) : Combiner<'a> = make (fun _ _ -> a)

    /// Wraps a combiner so `middle` is inserted between every combined pair:
    /// `combine self that = combiner.combine self (combiner.combine middle that)`.
    let intercalate (middle: 'a) (combiner: Combiner<'a>) : Combiner<'a> =
        make (fun self that -> combiner.combine self (combiner.combine middle that))
