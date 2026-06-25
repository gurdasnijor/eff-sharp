namespace Effect

#if FABLE_COMPILER
open Fable.Core
#endif

/// Trait: `Equal` — Effect's structural equality.
///
/// Port of repos/effect-smol/packages/effect/src/Equal.ts. Upstream `equals`
/// is a hand-written deep walker over arbitrary JS `unknown` values (objects,
/// arrays, typed arrays, `Map`/`Set`, `Date`, `RegExp`, the `Equal` interface),
/// with a `WeakMap` result cache, circular-reference tracking, and a
/// reference-equality opt-out (`byReference`).
///
/// Per the porting decoupling rule, we reuse F#'s built-in structural equality
/// instead of reimplementing that engine. On JS, `equals` uses a small
/// Fable-compatible deep equality walker so it preserves Effect's `NaN = NaN`
/// semantics without depending on unsupported FSharp.Core runtime helpers.
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

#if FABLE_COMPILER
    [<Emit("""(function equal(a, b) {
  const seen = new WeakMap();
  function ownKeys(value) {
    return Reflect.ownKeys(value).filter((key) => Object.prototype.propertyIsEnumerable.call(value, key));
  }
  function inner(x, y) {
    if (Object.is(x, y)) return true;
    if (typeof x === "number" && typeof y === "number") return Number.isNaN(x) && Number.isNaN(y);
    if (x == null || y == null) return false;
    if (typeof x !== "object" || typeof y !== "object") return false;
    if (x instanceof Date && y instanceof Date) return inner(x.getTime(), y.getTime());
    const cached = seen.get(x);
    if (cached === y) return true;
    seen.set(x, y);
    const xIsArray = Array.isArray(x) || ArrayBuffer.isView(x);
    const yIsArray = Array.isArray(y) || ArrayBuffer.isView(y);
    if (xIsArray || yIsArray) {
      if (!xIsArray || !yIsArray || x.length !== y.length) return false;
      for (let i = 0; i < x.length; i++) if (!inner(x[i], y[i])) return false;
      return true;
    }
    if (x instanceof Map && y instanceof Map) {
      if (x.size !== y.size) return false;
      for (const [key, value] of x) {
        if (!y.has(key) || !inner(value, y.get(key))) return false;
      }
      return true;
    }
    if (x instanceof Set && y instanceof Set) {
      if (x.size !== y.size) return false;
      for (const value of x) if (!y.has(value)) return false;
      return true;
    }
    const xKeys = ownKeys(x);
    const yKeys = ownKeys(y);
    if (xKeys.length !== yKeys.length) return false;
    for (const key of xKeys) {
      if (!Object.prototype.propertyIsEnumerable.call(y, key) || !inner(x[key], y[key])) return false;
    }
    return true;
  }
  return inner(a, b);
})($0, $1)""")>]
    let private equalJs (self: obj) (that: obj) : bool = jsNative

    /// Structural equality with Effect's `NaN = NaN` semantics on JS.
    let equals (self: 'a) (that: 'a) : bool =
        Unchecked.equals self that || equalJs (box self) (box that)
#else
    /// Structural equality with `NaN = NaN`, delegating to F#'s built-in
    /// equivalence-relation generic equality.
    let equals (self: 'a) (that: 'a) : bool =
        LanguagePrimitives.GenericEqualityERComparer.Equals(box self, box that)
#endif

    /// Wraps `equals` as an `Equivalence<'a>`.
    let asEquivalence<'a> () : Equivalence<'a> = equals
