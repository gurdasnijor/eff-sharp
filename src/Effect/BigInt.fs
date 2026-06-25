namespace Effect

open System.Numerics

/// Helpers for arbitrary-precision integers.
///
/// Port of repos/effect-smol/packages/effect/src/BigInt.ts. JavaScript's `bigint`
/// maps onto F#'s `bigint` (= `System.Numerics.BigInteger`), so this module only
/// ports the Effect-specific surface — safe arithmetic, range checks, parsing,
/// conversions, aggregation — and reuses the `Order` / `Equivalence` / `Combiner`
/// / `Reducer` traits already in the port.
///
/// Porting notes (see CONVENTIONS.md):
///   * `isBigInt` (an `unknown`-narrowing runtime guard) is omitted — F# is
///     statically typed (§3).
///   * The `BigInt` global-constructor re-export is omitted — native F# uses
///     `bigint` literals (`123I`) and the `fromString`/`fromNumber` parsers below.
///   * `gcd` reuses `BigInteger.GreatestCommonDivisor` (always non-negative)
///     instead of the hand-rolled Euclid loop.
[<RequireQualifiedAccess>]
module BigInt =

    let private zero = BigInteger.Zero
    let private one = BigInteger.One
    let private two = BigInteger(2)

    /// JavaScript's safe-integer bounds (±(2^53 − 1)), used by the number conversions.
    let private maxSafeInteger = BigInteger 9007199254740991L
    let private minSafeInteger = BigInteger -9007199254740991L
    let private maxSafeFloat = 9007199254740991.0
    let private minSafeFloat = -9007199254740991.0

    let private ord: Order<bigint> = Order.BigInt

    // --- math ---

    /// Adds two integers. (BigInt.sum)
    let sum (self: bigint) (that: bigint) : bigint = self + that

    /// Multiplies two integers. (BigInt.multiply)
    let multiply (self: bigint) (that: bigint) : bigint = self * that

    /// Subtracts `that` from `self`. (BigInt.subtract)
    let subtract (self: bigint) (that: bigint) : bigint = self - that

    /// Divides `self` by `that`, truncating toward zero; `None` when `that` is 0. (BigInt.divide)
    let divide (self: bigint) (that: bigint) : bigint option =
        if that = zero then None else Some(self / that)

    /// Divides `self` by `that`, truncating toward zero; throws when `that` is 0. (BigInt.divideUnsafe)
    let divideUnsafe (self: bigint) (that: bigint) : bigint = self / that

    /// The native (signed) remainder of `self / divisor`; throws when `divisor` is 0. (BigInt.remainder)
    let remainder (self: bigint) (divisor: bigint) : bigint = self % divisor

    /// `self + 1`. (BigInt.increment)
    let increment (n: bigint) : bigint = n + one

    /// `self - 1`. (BigInt.decrement)
    let decrement (n: bigint) : bigint = n - one

    /// The sign of `n` as `-1` / `0` / `1`. (BigInt.sign)
    let sign (n: bigint) : int = n.Sign

    /// The absolute value of `n`. (BigInt.abs)
    let abs (n: bigint) : bigint = if n < zero then -n else n

    /// Greatest common divisor (non-negative). (BigInt.gcd)
    let gcd (self: bigint) (that: bigint) : bigint =
        BigInteger.GreatestCommonDivisor(self, that)

    /// Least common multiple. (BigInt.lcm)
    let lcm (self: bigint) (that: bigint) : bigint = (self * that) / gcd self that

    /// Integer square root of a non-negative integer; throws on negative input. (BigInt.sqrtUnsafe)
    let sqrtUnsafe (n: bigint) : bigint =
        if n < zero then
            invalidArg "n" "Cannot take the square root of a negative number"
        elif n < two then
            n
        else
            let rec loop x =
                if x * x > n then loop (((n / x) + x) / two) else x

            loop (n / two)

    /// Integer square root; `None` for negative input. (BigInt.sqrt)
    let sqrt (n: bigint) : bigint option =
        if n >= zero then Some(sqrtUnsafe n) else None

    /// Sum of an iterable; `0` for empty. (BigInt.sumAll)
    let sumAll (collection: seq<bigint>) : bigint = Seq.fold (+) zero collection

    /// Product of an iterable; `1` for empty. Short-circuits to `0` on the first
    /// zero element. (BigInt.multiplyAll)
    let multiplyAll (collection: seq<bigint>) : bigint =
        let mutable acc = one
        let mutable stop = false
        use e = collection.GetEnumerator()

        while not stop && e.MoveNext() do
            if e.Current = zero then
                acc <- zero
                stop <- true
            else
                acc <- acc * e.Current

        acc

    // --- comparison predicates ---

    /// `true` when `self < that`. (BigInt.isLessThan)
    let isLessThan (self: bigint) (that: bigint) : bool = Order.isLessThan ord self that

    /// `true` when `self <= that`. (BigInt.isLessThanOrEqualTo)
    let isLessThanOrEqualTo (self: bigint) (that: bigint) : bool = Order.isLessThanOrEqualTo ord self that

    /// `true` when `self > that`. (BigInt.isGreaterThan)
    let isGreaterThan (self: bigint) (that: bigint) : bool = Order.isGreaterThan ord self that

    /// `true` when `self >= that`. (BigInt.isGreaterThanOrEqualTo)
    let isGreaterThanOrEqualTo (self: bigint) (that: bigint) : bool =
        Order.isGreaterThanOrEqualTo ord self that

    /// `true` when `self` is within the inclusive `[minimum, maximum]` range. (BigInt.between)
    let between (minimum: bigint) (maximum: bigint) (self: bigint) : bool =
        Order.isBetween ord minimum maximum self

    /// Clamps `self` into the inclusive `[minimum, maximum]` range. (BigInt.clamp)
    let clamp (minimum: bigint) (maximum: bigint) (self: bigint) : bigint = Order.clamp ord minimum maximum self

    /// The smaller of two integers. (BigInt.min)
    let min (self: bigint) (that: bigint) : bigint = Order.min ord self that

    /// The larger of two integers. (BigInt.max)
    let max (self: bigint) (that: bigint) : bigint = Order.max ord self that

    // --- converting ---

    /// Converts to a `float`; `None` outside JavaScript's safe-integer range. (BigInt.toNumber)
    let toNumber (b: bigint) : float option =
        if b > maxSafeInteger || b < minSafeInteger then
            None
        else
            Some(float b)

    /// Parses a string into an integer; `None` for blank or invalid input. (BigInt.fromString)
    let fromString (s: string) : bigint option =
        let trimmed = s.Trim()

        if trimmed = "" then
            None
        else
            match BigInteger.TryParse trimmed with
            | true, value -> Some value
            | false, _ -> None

    /// Converts a `float` to an integer; `None` if out of the safe-integer range
    /// or not integral. (BigInt.fromNumber)
    let fromNumber (n: float) : bigint option =
        if n > maxSafeFloat || n < minSafeFloat || System.Double.IsNaN n || n <> floor n then
            None
        else
            Some(BigInteger n)

    // --- instances ---

    /// `Order` instance for integers. (BigInt.Order)
    let Order: Order<bigint> = ord

    /// `Equivalence` instance for integers (strict equality). (BigInt.Equivalence)
    let Equivalence: Equivalence<bigint> = Equivalence.BigInt

    /// `Reducer` that sums integers (identity `0`). (BigInt.ReducerSum)
    let ReducerSum: Reducer<bigint> = Reducer.make (+) zero

    /// `Reducer` that multiplies integers (identity `1`), short-circuiting on `0`. (BigInt.ReducerMultiply)
    let ReducerMultiply: Reducer<bigint> = Reducer.makeWith (*) one multiplyAll

    /// `Combiner` that keeps the larger integer. (BigInt.CombinerMax)
    let CombinerMax: Combiner<bigint> = Combiner.max ord

    /// `Combiner` that keeps the smaller integer. (BigInt.CombinerMin)
    let CombinerMin: Combiner<bigint> = Combiner.min ord
