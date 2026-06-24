namespace Effect

/// A sequence guaranteed to contain at least one element.
///
/// Port of repos/effect-smol/packages/effect/src/NonEmptyIterable.ts.
///
/// Porting notes (see CONVENTIONS.md):
///   * The type-level brand (`unique symbol nonEmpty`) is JS-runtime/type-level
///     machinery with no F# equivalent and is dropped — `NonEmptyIterable<'A>`
///     is a plain `seq<'A>` abbreviation. The non-emptiness guarantee is by
///     convention (as it ultimately is upstream, where the brand is erased at
///     runtime); `unprepend` enforces it dynamically.
///   * `unprepend` returns the head and the remaining elements as a `seq`
///     (lazily drained from the same enumerator), matching the upstream
///     `[firstElement, remainingIterator]` shape.
type NonEmptyIterable<'A> = seq<'A>

[<RequireQualifiedAccess>]
module NonEmptyIterable =

    /// Splits a non-empty sequence into its first element and the remaining
    /// elements. Throws if the sequence is empty (a bug in the caller, since the
    /// type is meant to be non-empty).
    let unprepend (self: NonEmptyIterable<'a>) : 'a * seq<'a> =
        let enumerator = self.GetEnumerator()
        if not (enumerator.MoveNext()) then
            failwith
                "BUG: NonEmptyIterable should not be empty - please report an issue at https://github.com/Effect-TS/effect/issues"
        let head = enumerator.Current
        let rest =
            seq {
                while enumerator.MoveNext() do
                    yield enumerator.Current
            }
        head, rest
