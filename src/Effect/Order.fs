namespace Effect

/// Trait: `Order` — total comparison relations over a type.
///
/// Port of repos/effect-smol/packages/effect/src/Order.ts. An `Order<'A>` is a
/// curried `'A -> 'A -> int` returning `-1`, `0`, or `1` (Effect's `Ordering`).
///
/// Porting notes (see CONVENTIONS.md):
///   * The HKT plumbing (`OrderTypeLambda`) is skipped.
///   * `make` keeps the `self === that -> 0` short-circuit, expressed with F#
///     structural equality (`=`). The built-in instances (`Number`, `String`,
///     ...) rely on it for the equal case, exactly like upstream.
///   * Variadic `Tuple`/`Struct` over arbitrary arity / `Reflect`ed keys are
///     specialised to `tuple2`/`tuple3`; multi-field struct ordering is the
///     idiomatic `combine` of `mapInput`s (demonstrated in the tests).
///   * `makeReducer` (unported `Reducer` module) is skipped.
type Order<'A> = 'A -> 'A -> int

[<RequireQualifiedAccess>]
module Order =

    /// Creates an order from a comparison function, short-circuiting to `0`
    /// when the two values are (structurally) equal.
    let make<'a when 'a: equality> (compare: 'a -> 'a -> int) : Order<'a> =
        fun self that -> if self = that then 0 else compare self that

    /// Lexicographic string order.
    let String: Order<string> = fun self that -> sign (compare self that)

    /// Numeric order. `NaN` is treated as equal to itself and less than any
    /// other number.
    let Number: Order<float> =
        fun self that ->
            match System.Double.IsNaN self, System.Double.IsNaN that with
            | true, true -> 0
            | true, false -> -1
            | false, true -> 1
            | false, false -> sign (compare self that)

    /// Boolean order where `false < true`.
    let Boolean: Order<bool> = fun self that -> sign (compare self that)

    /// Arbitrary-precision integer order.
    let BigInt: Order<bigint> = fun self that -> sign (compare self that)

    /// Reverses an order.
    let flip (o: Order<'a>) : Order<'a> = fun self that -> o that self

    /// Combines two orders: compares with the first, breaking ties with the second.
    let combine (that: Order<'a>) (self: Order<'a>) : Order<'a> =
        fun a1 a2 ->
            match self a1 a2 with
            | 0 -> that a1 a2
            | out -> out

    /// An order that treats all values as equal.
    let alwaysEqual<'a> () : Order<'a> = fun _ _ -> 0

    /// Combines a collection of orders, short-circuiting on the first non-zero result.
    let combineAll (collection: Order<'a> seq) : Order<'a> =
        fun a1 a2 ->
            let mutable out = 0
            let mutable e = (collection :> _ seq).GetEnumerator()

            while out = 0 && e.MoveNext() do
                out <- e.Current a1 a2

            out

    /// Derives an order for `'b` by mapping inputs into `'a` first.
    let mapInput (f: 'b -> 'a) (self: Order<'a>) : Order<'b> = fun b1 b2 -> self (f b1) (f b2)

    /// Order for `DateTime`, chronological. Under Fable this follows JavaScript
    /// `Date` precision, so observable comparisons are millisecond-resolution.
    let Date: Order<System.DateTime> = fun a b -> sign (compare a b)

    /// Order for a 2-tuple, compared position-by-position.
    let tuple2 (oA: Order<'a>) (oB: Order<'b>) : Order<'a * 'b> =
        fun (a1, b1) (a2, b2) ->
            match oA a1 a2 with
            | 0 -> oB b1 b2
            | o -> o

    /// Order for a 3-tuple, compared position-by-position.
    let tuple3 (oA: Order<'a>) (oB: Order<'b>) (oC: Order<'c>) : Order<'a * 'b * 'c> =
        fun (a1, b1, c1) (a2, b2, c2) ->
            match oA a1 a2 with
            | 0 ->
                (match oB b1 b2 with
                 | 0 -> oC c1 c2
                 | o -> o)
            | o -> o

    /// Lexicographic order for lists: element-wise, then by length.
    let Array (o: Order<'a>) : Order<'a list> =
        fun self that ->
            let rec go xs ys =
                match xs, ys with
                | [], [] -> 0
                | [], _ -> -1
                | _, [] -> 1
                | x :: xs', y :: ys' ->
                    match o x y with
                    | 0 -> go xs' ys'
                    | r -> r

            go self that

    // --- predicates ---

    /// `true` when `self` is strictly less than `that`.
    let isLessThan (o: Order<'a>) (self: 'a) (that: 'a) : bool = o self that = -1

    /// `true` when `self` is strictly greater than `that`.
    let isGreaterThan (o: Order<'a>) (self: 'a) (that: 'a) : bool = o self that = 1

    /// `true` when `self` is less than or equal to `that`.
    let isLessThanOrEqualTo (o: Order<'a>) (self: 'a) (that: 'a) : bool = o self that <> 1

    /// `true` when `self` is greater than or equal to `that`.
    let isGreaterThanOrEqualTo (o: Order<'a>) (self: 'a) (that: 'a) : bool = o self that <> -1

    // --- comparisons ---

    /// The smaller of two values; the first when equal.
    let min (o: Order<'a>) (self: 'a) (that: 'a) : 'a = if o self that < 1 then self else that

    /// The larger of two values; the first when equal.
    let max (o: Order<'a>) (self: 'a) (that: 'a) : 'a = if o self that > -1 then self else that

    /// Clamps `self` into the inclusive `[minimum, maximum]` range.
    let clamp (o: Order<'a>) (minimum: 'a) (maximum: 'a) (self: 'a) : 'a = min o maximum (max o minimum self)

    /// `true` when `self` is within the inclusive `[minimum, maximum]` range.
    let isBetween (o: Order<'a>) (minimum: 'a) (maximum: 'a) (self: 'a) : bool =
        not (isLessThan o self minimum) && not (isGreaterThan o self maximum)
