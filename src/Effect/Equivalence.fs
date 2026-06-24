namespace Effect

/// Trait: `Equivalence` — reusable equality relations over a type.
///
/// Port of repos/effect-smol/packages/effect/src/Equivalence.ts. An
/// `Equivalence<'A>` is simply a curried `'A -> 'A -> bool` that must be
/// reflexive, symmetric, and transitive.
///
/// Porting notes (see CONVENTIONS.md):
///   * The HKT plumbing (`EquivalenceTypeLambda`) is skipped — F# has no HKTs.
///   * The JS `self === that` reference fast-path inside `make` is a runtime
///     micro-optimization; instances that need value equality (e.g. `Number`)
///     express it directly, so `make` is the plain function.
///   * `Struct`/`Record` over `Reflect.ownKeys` are JS-object specific. The
///     idiomatic F# replacement is `combine` + `mapInput` over record fields;
///     a `record` combinator over `Map<'K,'V>` is provided for the dynamic-key
///     case. Variadic `Tuple` is specialised to `tuple2`/`tuple3` because F#
///     tuples are heterogeneous and statically typed.
///   * `makeReducer` (depends on the unported `Reducer` module) is skipped.
type Equivalence<'A> = 'A -> 'A -> bool

[<RequireQualifiedAccess>]
module Equivalence =

    /// Creates a custom equivalence from a comparison function.
    let make (isEquivalent: 'a -> 'a -> bool) : Equivalence<'a> = isEquivalent

    /// Strict (structural value) equality — the F# analogue of JS `===` for the
    /// primitive/value types this trait is used with.
    let strictEqual<'a when 'a: equality> () : Equivalence<'a> = fun self that -> self = that

    /// Case-sensitive string equality.
    let String: Equivalence<string> = fun self that -> self = that

    /// Numeric equality that, unlike `=`, treats `NaN` as equal to `NaN`.
    let Number: Equivalence<float> =
        fun self that -> self = that || (System.Double.IsNaN self && System.Double.IsNaN that)

    /// Boolean equality.
    let Boolean: Equivalence<bool> = fun self that -> self = that

    /// Arbitrary-precision integer equality.
    let BigInt: Equivalence<bigint> = fun self that -> self = that

    /// Combines two equivalences with logical AND (short-circuiting).
    let combine (that: Equivalence<'a>) (self: Equivalence<'a>) : Equivalence<'a> =
        fun x y -> self x y && that x y

    /// Combines a collection of equivalences with logical AND. An empty
    /// collection yields the always-true equivalence.
    let combineAll (collection: Equivalence<'a> seq) : Equivalence<'a> =
        fun x y -> collection |> Seq.forall (fun eq -> eq x y)

    /// Derives an equivalence for `'b` by mapping inputs into `'a` first.
    let mapInput (f: 'b -> 'a) (self: Equivalence<'a>) : Equivalence<'b> =
        fun x y -> self (f x) (f y)

    /// Equivalence for arrays/lists: same length and element-wise equivalent.
    let Array (item: Equivalence<'a>) : Equivalence<'a list> =
        fun self that ->
            List.length self = List.length that && List.forall2 item self that

    /// Equivalence for a 2-tuple from per-position equivalences.
    let tuple2 (eqA: Equivalence<'a>) (eqB: Equivalence<'b>) : Equivalence<'a * 'b> =
        fun (a1, b1) (a2, b2) -> eqA a1 a2 && eqB b1 b2

    /// Equivalence for a 3-tuple from per-position equivalences.
    let tuple3 (eqA: Equivalence<'a>) (eqB: Equivalence<'b>) (eqC: Equivalence<'c>) : Equivalence<'a * 'b * 'c> =
        fun (a1, b1, c1) (a2, b2, c2) -> eqA a1 a2 && eqB b1 b2 && eqC c1 c2

    /// Equivalence for maps (dynamic-key records): same key set and the value
    /// equivalence holding for every key. Mirrors upstream `Record`.
    let record (value: Equivalence<'v>) : Equivalence<Map<'k, 'v>> =
        fun self that ->
            Map.count self = Map.count that
            && self |> Map.forall (fun k v -> match Map.tryFind k that with Some w -> value v w | None -> false)

    /// Equivalence for `DateTime` values, compared by value (full tick resolution).
    let Date: Equivalence<System.DateTime> = fun self that -> self = that
