namespace Effect

open System.Numerics
open System.Globalization
open System.Text.RegularExpressions

/// Port of repos/effect-smol/packages/effect/src/BigDecimal.ts.
///
/// Arbitrary-precision decimal numbers: an unscaled `bigint` value plus a
/// decimal `scale` (`value * 10^-scale`). Used where binary floating point
/// rounding is not precise enough (money, quantities, measurements).
///
/// Port notes / omissions (per CONVENTIONS.md):
///   * The upstream `normalized` caching field is a JS micro-optimization and is
///     dropped; F# structural equality on `{ Value; Scale }` is what we need.
///   * JS-runtime machinery (`TypeId` brand, `pipe`, `toJSON`, `NodeInspectSymbol`,
///     `Hash`/`Equal` symbol methods) is dropped — F#'s static types make the
///     guards dead weight, and we are decoupled from the `Equal`/`Hash` modules.
///   * `isBigDecimal` is kept (a test asserts it) but narrows a known `obj` via a
///     type test rather than a structural `TypeId` probe.
///   * `Order`/`Equivalence` instances lower to plain `order`/`equivalence`
///     functions returning `int` (-1/0/1) / `bool`.
type BigDecimal = { Value: bigint; Scale: int }

/// Rounding modes for `round`. Mirrors upstream `RoundingMode`.
type RoundingMode =
    | Ceil
    | Floor
    | ToZero
    | FromZero
    | HalfCeil
    | HalfFloor
    | HalfToZero
    | HalfFromZero
    | HalfEven
    | HalfOdd

[<RequireQualifiedAccess>]
module BigDecimal =

    [<Literal>]
    let private DEFAULT_PRECISION = 100

    let private bigint10 = BigInteger 10
    let private finiteIntRegex = Regex(@"^[+-]?\d+$", RegexOptions.Compiled)
    let private inv = CultureInfo.InvariantCulture

    // --- constructors ---

    /// Construct a `BigDecimal` from an unscaled value and a scale.
    let make (value: bigint) (scale: int) : BigDecimal = { Value = value; Scale = scale }

    /// Construct a pre-normalized `BigDecimal`; throws if `value` is not already
    /// normalized (a non-zero multiple of ten). Used as a parity helper in tests.
    let makeNormalizedUnsafe (value: bigint) (scale: int) : BigDecimal =
        if value <> BigInteger.Zero && value % bigint10 = BigInteger.Zero then
            raise (System.ArgumentException "Value must be normalized")

        { Value = value; Scale = scale }

    let zero: BigDecimal = makeNormalizedUnsafe BigInteger.Zero 0
    let one: BigDecimal = makeNormalizedUnsafe BigInteger.One 0

    /// Remove trailing zeros, canonicalizing the representation.
    let normalize (self: BigDecimal) : BigDecimal =
        if self.Value = BigInteger.Zero then
            zero
        else
            let digits = string self.Value
            let mutable trail = 0
            let mutable i = digits.Length - 1
            let mutable stop = false

            while i >= 0 && not stop do
                if digits.[i] = '0' then
                    trail <- trail + 1
                    i <- i - 1
                else
                    stop <- true

            let value = BigInteger.Parse(digits.Substring(0, digits.Length - trail))
            makeNormalizedUnsafe value (self.Scale - trail)

    /// Change a `BigDecimal` to the given scale (truncating toward zero when the
    /// scale shrinks).
    let scale (s: int) (self: BigDecimal) : BigDecimal =
        if s > self.Scale then
            make (self.Value * BigInteger.Pow(bigint10, s - self.Scale)) s
        elif s < self.Scale then
            make (self.Value / BigInteger.Pow(bigint10, self.Scale - s)) s
        else
            self

    // --- sign / magnitude ---

    /// -1, 0, or 1 according to the sign.
    let sign (n: BigDecimal) : int = n.Value.Sign

    let isZero (n: BigDecimal) : bool = n.Value = BigInteger.Zero
    let isNegative (n: BigDecimal) : bool = n.Value < BigInteger.Zero
    let isPositive (n: BigDecimal) : bool = n.Value > BigInteger.Zero

    let abs (n: BigDecimal) : BigDecimal =
        if n.Value < BigInteger.Zero then
            make (-n.Value) n.Scale
        else
            n

    let negate (n: BigDecimal) : BigDecimal = make (-n.Value) n.Scale

    // --- equality / ordering ---

    /// Numeric equivalence (scale-insensitive).
    let equivalence (self: BigDecimal) (that: BigDecimal) : bool =
        if self.Scale > that.Scale then
            (scale self.Scale that).Value = self.Value
        elif self.Scale < that.Scale then
            (scale that.Scale self).Value = that.Value
        else
            self.Value = that.Value

    let equals (self: BigDecimal) (that: BigDecimal) : bool = equivalence self that

    let private cmpBig (a: bigint) (b: bigint) : int = Operators.sign (compare a b)

    /// Total order returning -1/0/1.
    let order (self: BigDecimal) (that: BigDecimal) : int =
        let scmp = compare (sign self) (sign that)

        if scmp <> 0 then
            scmp
        elif self.Scale > that.Scale then
            cmpBig self.Value (scale self.Scale that).Value
        elif self.Scale < that.Scale then
            cmpBig (scale that.Scale self).Value that.Value
        else
            cmpBig self.Value that.Value

    let isLessThan (self: BigDecimal) (that: BigDecimal) : bool = order self that < 0
    let isLessThanOrEqualTo (self: BigDecimal) (that: BigDecimal) : bool = order self that <= 0
    let isGreaterThan (self: BigDecimal) (that: BigDecimal) : bool = order self that > 0
    let isGreaterThanOrEqualTo (self: BigDecimal) (that: BigDecimal) : bool = order self that >= 0

    /// Inclusive range test.
    let between (minimum: BigDecimal) (maximum: BigDecimal) (self: BigDecimal) : bool =
        order self minimum >= 0 && order self maximum <= 0

    /// Restrict to the inclusive range `[minimum, maximum]`.
    let clamp (minimum: BigDecimal) (maximum: BigDecimal) (self: BigDecimal) : BigDecimal =
        if order self minimum < 0 then minimum
        elif order self maximum > 0 then maximum
        else self

    let min (self: BigDecimal) (that: BigDecimal) : BigDecimal =
        if order self that <= 0 then self else that

    let max (self: BigDecimal) (that: BigDecimal) : BigDecimal =
        if order self that >= 0 then self else that

    // --- arithmetic ---

    let sum (self: BigDecimal) (that: BigDecimal) : BigDecimal =
        if that.Value = BigInteger.Zero then
            self
        elif self.Value = BigInteger.Zero then
            that
        elif self.Scale > that.Scale then
            make ((scale self.Scale that).Value + self.Value) self.Scale
        elif self.Scale < that.Scale then
            make ((scale that.Scale self).Value + that.Value) that.Scale
        else
            make (self.Value + that.Value) self.Scale

    let sumAll (collection: BigDecimal seq) : BigDecimal = Seq.fold sum zero collection

    let multiply (self: BigDecimal) (that: BigDecimal) : BigDecimal =
        if that.Value = BigInteger.Zero || self.Value = BigInteger.Zero then
            zero
        else
            make (self.Value * that.Value) (self.Scale + that.Scale)

    let multiplyAll (collection: BigDecimal seq) : BigDecimal =
        Seq.fold (fun acc n -> if n.Value = BigInteger.Zero then zero else multiply acc n) one collection

    let subtract (self: BigDecimal) (that: BigDecimal) : BigDecimal =
        if that.Value = BigInteger.Zero then
            self
        elif self.Value = BigInteger.Zero then
            make (-that.Value) that.Scale
        elif self.Scale > that.Scale then
            make (self.Value - (scale self.Scale that).Value) self.Scale
        elif self.Scale < that.Scale then
            make ((scale that.Scale self).Value - that.Value) that.Scale
        else
            make (self.Value - that.Value) self.Scale

    // --- division ---

    /// 1 if the most significant digit is >= 5, else 0.
    let roundTerminal (n: bigint) : bigint =
        let s = string n
        let pos = if n >= BigInteger.Zero then 0 else 1

        if int (string s.[pos]) < 5 then
            BigInteger.Zero
        else
            BigInteger.One

    let private divideWithPrecision (num0: bigint) (den0: bigint) (scale0: int) (precision: int) : BigDecimal =
        let numNeg = num0 < BigInteger.Zero
        let denNeg = den0 < BigInteger.Zero
        let negateResult = numNeg <> denNeg
        let mutable num = if numNeg then -num0 else num0
        let den = if denNeg then -den0 else den0
        let mutable scaleAcc = scale0

        while num < den do
            num <- num * bigint10
            scaleAcc <- scaleAcc + 1

        let mutable quotient = num / den
        let mutable remainder = num % den

        if remainder = BigInteger.Zero then
            make (if negateResult then -quotient else quotient) scaleAcc
        else
            let mutable count = (string quotient).Length
            remainder <- remainder * bigint10

            while remainder <> BigInteger.Zero && count < precision do
                let q = remainder / den
                let r = remainder % den
                quotient <- quotient * bigint10 + q
                remainder <- r * bigint10
                count <- count + 1
                scaleAcc <- scaleAcc + 1

            if remainder <> BigInteger.Zero then
                quotient <- quotient + roundTerminal (remainder / den)

            make (if negateResult then -quotient else quotient) scaleAcc

    /// Safe division; `None` when dividing by zero.
    let divide (self: BigDecimal) (that: BigDecimal) : BigDecimal option =
        if that.Value = BigInteger.Zero then
            None
        elif self.Value = BigInteger.Zero then
            Some zero
        else
            let sc = self.Scale - that.Scale

            if self.Value = that.Value then
                Some(make BigInteger.One sc)
            else
                Some(divideWithPrecision self.Value that.Value sc DEFAULT_PRECISION)

    /// Division that throws on a zero divisor.
    let divideUnsafe (self: BigDecimal) (that: BigDecimal) : BigDecimal =
        if that.Value = BigInteger.Zero then
            invalidOp "Division by zero"
        elif self.Value = BigInteger.Zero then
            zero
        else
            let sc = self.Scale - that.Scale

            if self.Value = that.Value then
                make BigInteger.One sc
            else
                divideWithPrecision self.Value that.Value sc DEFAULT_PRECISION

    /// Safe decimal remainder; `None` when dividing by zero.
    let remainder (self: BigDecimal) (divisor: BigDecimal) : BigDecimal option =
        if divisor.Value = BigInteger.Zero then
            None
        else
            let m =
                if self.Scale > divisor.Scale then
                    self.Scale
                else
                    divisor.Scale

            Some(make ((scale m self).Value % (scale m divisor).Value) m)

    /// Decimal remainder that throws on a zero divisor.
    let remainderUnsafe (self: BigDecimal) (divisor: BigDecimal) : BigDecimal =
        if divisor.Value = BigInteger.Zero then
            invalidOp "Division by zero"
        else
            let m =
                if self.Scale > divisor.Scale then
                    self.Scale
                else
                    divisor.Scale

            make ((scale m self).Value % (scale m divisor).Value) m

    // --- constructors from other representations ---

    let fromBigInt (n: bigint) : BigDecimal = make n 0

    let fromString (s: string) : BigDecimal option =
        if s = "" then
            Some zero
        else
            let sepIdx =
                let i = s.IndexOfAny([| 'e'; 'E' |])
                if i < 0 then None else Some i

            let parsed =
                match sepIdx with
                | Some i ->
                    let trail = s.Substring(i + 1)
                    let baseStr = s.Substring(0, i)

                    match System.Int32.TryParse(trail, NumberStyles.AllowLeadingSign, inv) with
                    | true, exp when baseStr <> "" && finiteIntRegex.IsMatch(trail) -> Some(baseStr, exp)
                    | _ -> None
                | None -> Some(s, 0)

            match parsed with
            | None -> None
            | Some(baseStr, exp) ->
                let digits, offset =
                    let dot = baseStr.IndexOf '.'

                    if dot >= 0 then
                        let lead = baseStr.Substring(0, dot)
                        let trail = baseStr.Substring(dot + 1)
                        lead + trail, trail.Length
                    else
                        baseStr, 0

                if not (finiteIntRegex.IsMatch digits) then
                    None
                else
                    Some(make (BigInteger.Parse(digits, NumberStyles.AllowLeadingSign, inv)) (offset - exp))

    let fromStringUnsafe (s: string) : BigDecimal =
        match fromString s with
        | Some d -> d
        | None -> raise (System.Exception(sprintf "Invalid numerical string: %s" s))

    let fromNumber (n: float) : BigDecimal option =
        if not (System.Double.IsFinite n) then
            None
        else
            let str = n.ToString("R", inv)

            if str.IndexOfAny([| 'e'; 'E' |]) >= 0 then
                fromString str
            else
                let dot = str.IndexOf '.'

                if dot >= 0 then
                    let lead = str.Substring(0, dot)
                    let trail = str.Substring(dot + 1)
                    Some(make (BigInteger.Parse(lead + trail, NumberStyles.AllowLeadingSign, inv)) trail.Length)
                else
                    Some(make (BigInteger.Parse(str, NumberStyles.AllowLeadingSign, inv)) 0)

    let fromNumberUnsafe (n: float) : BigDecimal =
        match fromNumber n with
        | Some d -> d
        | None -> raise (System.ArgumentException(sprintf "Number must be finite, got %f" n))

    // --- formatting ---

    let rec format (n: BigDecimal) : string =
        let normalized = normalize n

        if System.Math.Abs normalized.Scale >= 16 then
            toExponential normalized
        else
            let negative = normalized.Value < BigInteger.Zero

            let absolute =
                if negative then
                    (string normalized.Value).Substring(1)
                else
                    string normalized.Value

            let before, after =
                if normalized.Scale >= absolute.Length then
                    "0", System.String('0', normalized.Scale - absolute.Length) + absolute
                else
                    let location = absolute.Length - normalized.Scale

                    if location > absolute.Length then
                        absolute + System.String('0', location - absolute.Length), ""
                    else
                        absolute.Substring(0, location), absolute.Substring(location)

            let complete = if after = "" then before else before + "." + after
            if negative then "-" + complete else complete

    and toExponential (n: BigDecimal) : string =
        if isZero n then
            "0e+0"
        else
            let normalized = normalize n
            let digits = string (abs normalized).Value
            let head = digits.Substring(0, 1)
            let tail = digits.Substring 1
            let signStr = if isNegative normalized then "-" else ""

            let baseOut =
                if tail <> "" then
                    signStr + head + "." + tail
                else
                    signStr + head

            let exp = tail.Length - normalized.Scale
            baseOut + (if exp >= 0 then sprintf "e+%d" exp else sprintf "e%d" exp)

    /// Convert to a `float` (lossy interop boundary).
    let toNumberUnsafe (n: BigDecimal) : float = System.Double.Parse(format n, inv)

    /// `BigDecimal(<format>)`
    let toString (n: BigDecimal) : string = sprintf "BigDecimal(%s)" (format n)

    // --- predicates ---

    let isInteger (n: BigDecimal) : bool = (normalize n).Scale <= 0

    /// Narrows a boxed value to `BigDecimal`.
    let isBigDecimal (u: obj) : bool =
        match u with
        | :? BigDecimal -> true
        | _ -> false

    /// The digit at the given scale position (used by `half-even`/`half-odd`).
    let digitAt (s: int) (self: BigDecimal) : bigint =
        if self.Scale < s then
            BigInteger.Zero
        else
            (self.Value / BigInteger.Pow(bigint10, self.Scale - s)) % bigint10

    // --- rounding ---

    /// Truncate toward zero at the given scale.
    let truncate (s: int) (self: BigDecimal) : BigDecimal =
        if self.Scale <= s then
            self
        else
            make (self.Value / BigInteger.Pow(bigint10, self.Scale - s)) s

    /// Round toward positive infinity at the given scale.
    let ceil (s: int) (self: BigDecimal) : BigDecimal =
        let truncated = truncate s self

        if isPositive self && isLessThan truncated self then
            sum truncated (make BigInteger.One s)
        else
            truncated

    /// Round toward negative infinity at the given scale.
    let floor (s: int) (self: BigDecimal) : BigDecimal =
        let truncated = truncate s self

        if isNegative self && isGreaterThan truncated self then
            sum truncated (make BigInteger.MinusOne s)
        else
            truncated

    let private big5 = BigInteger 5
    let private bigMinus5 = BigInteger -5

    /// Round at `scale` with the given `mode`.
    let round (mode: RoundingMode) (s: int) (self: BigDecimal) : BigDecimal =
        match mode with
        | Ceil -> ceil s self
        | Floor -> floor s self
        | ToZero -> truncate s self
        | FromZero -> if isPositive self then ceil s self else floor s self
        | HalfCeil -> floor s (sum self (make big5 (s + 1)))
        | HalfFloor -> ceil s (sum self (make bigMinus5 (s + 1)))
        | HalfToZero ->
            if isNegative self then
                floor s (sum self (make big5 (s + 1)))
            else
                ceil s (sum self (make bigMinus5 (s + 1)))
        | HalfFromZero ->
            if isNegative self then
                ceil s (sum self (make bigMinus5 (s + 1)))
            else
                floor s (sum self (make big5 (s + 1)))
        | HalfEven
        | HalfOdd ->
            let halfCeil = floor s (sum self (make big5 (s + 1)))
            let halfFloor = ceil s (sum self (make bigMinus5 (s + 1)))
            let digit = digitAt s halfCeil
            let even = digit % BigInteger 2 = BigInteger.Zero

            match mode with
            | HalfEven ->
                if equals halfCeil halfFloor then halfCeil
                elif even then halfCeil
                else halfFloor
            | _ ->
                if equals halfCeil halfFloor then halfCeil
                elif even then halfFloor
                else halfCeil
