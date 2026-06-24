namespace Effect

/// Trait: `Equal` — Effect's structural equality.
///
/// Port of repos/effect-smol/packages/effect/src/Equal.ts. Upstream `equals`
/// is a hand-written deep walker over arbitrary JS `unknown` values (objects,
/// arrays, typed arrays, `Map`/`Set`, `Date`, `RegExp`, the `Equal` interface),
/// with a `WeakMap` result cache, circular-reference tracking, and a
/// reference-equality opt-out (`byReference`).
///
/// Per the porting decoupling rule, we reuse F#'s built-in structural equality
/// instead of reimplementing that engine. `LanguagePrimitives.GenericEqualityER`
/// gives deep, value-based equality over records, tuples, lists, arrays,
/// options, `Map`/`Set`, and `DateTime`, and — being the "ER" (equivalence
/// relation) variant — treats `NaN` as equal to `NaN`, matching Effect's
/// observable behaviour.
///
/// Porting notes (see CONVENTIONS.md):
///   * The `Equal`/`Hash` symbol-method interface, `isEqual` guard, the result
///     cache, circular tracking, and `byReference`/`byReferenceUnsafe` opt-outs
///     are JS-runtime machinery and are dropped.
///   * `Map`/`Set`/`Date`/array comparisons fall out of F#'s native structural
///     equality rather than the bespoke per-type comparators.
[<RequireQualifiedAccess>]
module Equal =

    /// Upstream symbol id (kept for parity).
    [<Literal>]
    let symbol = "~effect/interfaces/Equal"

    /// Structural equality with `NaN = NaN`, delegating to F#'s built-in
    /// equivalence-relation generic equality.
    let equals (self: 'a) (that: 'a) : bool =
        LanguagePrimitives.GenericEqualityERComparer.Equals(box self, box that)

    /// Wraps `equals` as an `Equivalence<'a>`.
    let asEquivalence<'a> () : Equivalence<'a> = equals
