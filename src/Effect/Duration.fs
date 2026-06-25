namespace Effect

open System.Numerics
open System.Globalization
open System.Text.RegularExpressions

/// Port of repos/effect-smol/packages/effect/src/Duration.ts.
///
/// Immutable spans of time, finite or signed-infinite. A finite duration is
/// stored either as integer milliseconds (`Millis`) or as nanoseconds
/// (`Nanos`, a `bigint`) — matching upstream's tagged `DurationValue`.
///
/// Port notes / omissions (per CONVENTIONS.md):
///   * JS-runtime machinery (`TypeId` brand, `pipe`, `toJSON`, `NodeInspectSymbol`,
///     `Hash`/`Equal` symbol methods) is dropped; F# structural equality on the
///     `DurationValue` DU is the equivalence we need, and we are decoupled from
///     the `Equal`/`Hash` modules.
///   * The `Order`/`Equivalence` instances lower to plain `order`/`equivalence`
///     functions; `match` lowers to native `match` plus a `matchWith` helper.
///   * `Input` (a TS union accepting numbers/bigints/strings/tuples/objects) is
///     modelled as an explicit DU; `DurationObject` as a record with `Empty`.
///   * Conversions (`toMillis`, …) take a `Duration` directly rather than the
///     broader `Input` (only ever called with a `Duration` in practice).
///   * `divideUnsafe` throws (rather than the doc-comment's zero fallback) when a
///     nanosecond divisor is not integer-convertible — this matches the upstream
///     *test* (`throws(() => divideUnsafe(nanos(1n), 0.5))`).
///   * `Reducer`/`Combiner` lower to small records exposing `Combine`/`Initial`.
/// Tagged representation of a duration value.
type DurationValue =
    | Millis of millis: float
    | Nanos of nanos: bigint
    | Infinity
    | NegativeInfinity

/// A span of time.
type Duration = { Value: DurationValue }

/// Object-shaped duration input (all components optional and additive).
type DurationObject =
    { Weeks: float option
      Days: float option
      Hours: float option
      Minutes: float option
      Seconds: float option
      Milliseconds: float option
      Microseconds: float option
      Nanoseconds: float option }

    static member Empty =
        { Weeks = None
          Days = None
          Hours = None
          Minutes = None
          Seconds = None
          Milliseconds = None
          Microseconds = None
          Nanoseconds = None }

/// Values that can be decoded into a `Duration`.
type Input =
    | FromDuration of Duration
    | FromMillisNumber of float
    | FromNanosBigInt of bigint
    | FromHrTime of seconds: float * nanos: float
    | FromString of string
    | FromObject of DurationObject

/// The signed, normalized components of a duration.
type DurationParts =
    { Days: float
      Hours: float
      Minutes: float
      Seconds: float
      Millis: float
      Nanos: float }

/// A binary combiner over durations.
type DurationCombiner =
    { Combine: Duration -> Duration -> Duration }

/// A combiner with an identity element.
type DurationReducer =
    { Combine: Duration -> Duration -> Duration
      Initial: Duration }

[<RequireQualifiedAccess>]
module Duration =

    let private inv = CultureInfo.InvariantCulture

#if FABLE_COMPILER
    // Fable shims: Double.IsFinite/IsInteger/IsNegative (static) are unsupported.
    // Reimplemented with Fable-safe ops; -0.0 is detected via 1.0/x = -infinity.
    // (severity: trivial)
    let inline private numIsFinite (x: float) : bool =
        not (System.Double.IsNaN x || System.Double.IsInfinity x)

    let inline private numIsInteger (x: float) : bool = numIsFinite x && floor x = x
    let inline private numIsNegative (x: float) : bool = x < 0.0 || (x = 0.0 && (1.0 / x) < 0.0)
#else
    let inline private numIsFinite (x: float) : bool = System.Double.IsFinite x
    let inline private numIsInteger (x: float) : bool = System.Double.IsInteger x
    let inline private numIsNegative (x: float) : bool = System.Double.IsNegative x
#endif

    let private bigint24 = BigInteger 24
    let private bigint60 = BigInteger 60
    let private bigint1e3 = BigInteger 1_000
    let private bigint1e6 = BigInteger 1_000_000
    let private bigint1e9 = BigInteger 1_000_000_000

    let private durationRegex =
        Regex(
            @"^(-?\d+(?:\.\d+)?)\s+(nanos?|micros?|millis?|seconds?|minutes?|hours?|days?|weeks?)$",
            RegexOptions.Compiled
        )

    let private roundTiesAwayFromZero (input: float) : bigint =
        BigInteger(System.Math.Round(input, System.MidpointRounding.AwayFromZero))

    let private roundMillisToNanos (millis: float) : bigint =
        roundTiesAwayFromZero (millis * 1_000_000.0)

    let private parseNanos (input: string) (scaleB: bigint) : bigint =
        if input.Contains "." then
            roundTiesAwayFromZero (System.Double.Parse(input, inv) * float scaleB)
        else
            BigInteger.Parse(input, inv) * scaleB

    let private nanosToHrTime (nanos: bigint) : float * float =
        let s =
            if nanos < BigInteger.Zero then
                BigInteger.MinusOne
            else
                BigInteger.One

        let absolute = if nanos < BigInteger.Zero then -nanos else nanos
        float (s * (absolute / bigint1e9)), float (s * (absolute % bigint1e9))

    // --- core constructors ---

    let private makeFromNumber (input: float) : Duration =
        if System.Double.IsNaN input || input = 0.0 then
            { Value = Millis 0.0 }
        elif not (numIsFinite input) then
            { Value = (if input > 0.0 then Infinity else NegativeInfinity) }
        elif not (numIsInteger input) then
            { Value = Nanos(roundMillisToNanos input) }
        else
            { Value = Millis input }

    let private makeFromBigInt (input: bigint) : Duration =
        if input = BigInteger.Zero then
            { Value = Millis 0.0 }
        else
            { Value = Nanos input }

    /// Creates a duration from nanoseconds.
    let nanos (nanos: bigint) : Duration = makeFromBigInt nanos

    /// Creates a duration from microseconds.
    let micros (micros: bigint) : Duration = makeFromBigInt (micros * bigint1e3)

    /// Creates a duration from milliseconds.
    let millis (millis: float) : Duration = makeFromNumber millis

    /// Creates a duration from seconds.
    let seconds (seconds: float) : Duration = makeFromNumber (seconds * 1000.0)

    /// Creates a duration from minutes.
    let minutes (minutes: float) : Duration = makeFromNumber (minutes * 60_000.0)

    /// Creates a duration from hours.
    let hours (hours: float) : Duration = makeFromNumber (hours * 3_600_000.0)

    /// Creates a duration from days.
    let days (days: float) : Duration = makeFromNumber (days * 86_400_000.0)

    /// Creates a duration from weeks.
    let weeks (weeks: float) : Duration = makeFromNumber (weeks * 604_800_000.0)

    let zero: Duration = makeFromNumber 0.0
    let infinity: Duration = { Value = Infinity }
    let negativeInfinity: Duration = { Value = NegativeInfinity }

    // --- input decoding ---

    let fromInputUnsafe (input: Input) : Duration =
        match input with
        | FromDuration d -> d
        | FromMillisNumber n -> millis n
        | FromNanosBigInt b -> nanos b
        | FromString s ->
            if s = "Infinity" then
                infinity
            elif s = "-Infinity" then
                negativeInfinity
            else
                let m = durationRegex.Match s

                if not m.Success then
                    raise (System.Exception(sprintf "Invalid Input: %s" s))
                else
                    let valueStr = m.Groups.[1].Value
                    let unit = m.Groups.[2].Value

                    match unit with
                    | "nano"
                    | "nanos" -> nanos (parseNanos valueStr BigInteger.One)
                    | "micro"
                    | "micros" -> nanos (parseNanos valueStr bigint1e3)
                    | _ ->
                        let value = System.Double.Parse(valueStr, inv)

                        match unit with
                        | "milli"
                        | "millis" -> millis value
                        | "second"
                        | "seconds" -> seconds value
                        | "minute"
                        | "minutes" -> minutes value
                        | "hour"
                        | "hours" -> hours value
                        | "day"
                        | "days" -> days value
                        | "week"
                        | "weeks" -> weeks value
                        | _ -> raise (System.Exception(sprintf "Invalid Input: %s" s))
        | FromHrTime(a, b) ->
            if System.Double.IsNaN a || System.Double.IsNaN b then
                zero
            elif a = System.Double.NegativeInfinity || b = System.Double.NegativeInfinity then
                negativeInfinity
            elif a = System.Double.PositiveInfinity || b = System.Double.PositiveInfinity then
                infinity
            else
                makeFromBigInt (roundTiesAwayFromZero (a * 1_000_000_000.0 + b))
        | FromObject obj ->
            let truthy (o: float option) =
                match o with
                | Some v when v <> 0.0 && not (System.Double.IsNaN v) -> v
                | _ -> 0.0

            let mutable ms = 0.0
            ms <- ms + truthy obj.Weeks * 604_800_000.0
            ms <- ms + truthy obj.Days * 86_400_000.0
            ms <- ms + truthy obj.Hours * 3_600_000.0
            ms <- ms + truthy obj.Minutes * 60_000.0
            ms <- ms + truthy obj.Seconds * 1_000.0
            ms <- ms + truthy obj.Milliseconds
            let hasMicros = truthy obj.Microseconds <> 0.0
            let hasNanos = truthy obj.Nanoseconds <> 0.0

            if not hasMicros && not hasNanos then
                makeFromNumber ms
            else
                makeFromBigInt (
                    roundTiesAwayFromZero (
                        ms * 1_000_000.0 + truthy obj.Microseconds * 1_000.0 + truthy obj.Nanoseconds
                    )
                )

    let fromInput (input: Input) : Duration option =
        try
            Some(fromInputUnsafe input)
        with _ ->
            None

    // --- guards ---

    /// Narrows a boxed value to `Duration`.
    let isDuration (u: obj) : bool =
        match u with
        | :? Duration -> true
        | _ -> false

    let isFinite (self: Duration) : bool =
        match self.Value with
        | Infinity
        | NegativeInfinity -> false
        | _ -> true

    let isZero (self: Duration) : bool =
        match self.Value with
        | Millis m -> m = 0.0
        | Nanos n -> n = BigInteger.Zero
        | Infinity
        | NegativeInfinity -> false

    let isNegative (self: Duration) : bool =
        match self.Value with
        | Millis m -> m < 0.0
        | Nanos n -> n < BigInteger.Zero
        | NegativeInfinity -> true
        | Infinity -> false

    let isPositive (self: Duration) : bool =
        match self.Value with
        | Millis m -> m > 0.0
        | Nanos n -> n > BigInteger.Zero
        | Infinity -> true
        | NegativeInfinity -> false

    // --- magnitude ---

    let abs (self: Duration) : Duration =
        match self.Value with
        | Infinity
        | NegativeInfinity -> infinity
        | Millis m -> if m < 0.0 then millis (-m) else self
        | Nanos n -> if n < BigInteger.Zero then nanos (-n) else self

    let negate (self: Duration) : Duration =
        match self.Value with
        | Infinity -> negativeInfinity
        | NegativeInfinity -> infinity
        | Millis m -> if m = 0.0 then self else millis (-m)
        | Nanos n -> if n = BigInteger.Zero then self else nanos (-n)

    // --- getters ---

    let toMillis (self: Duration) : float =
        match self.Value with
        | Millis m -> m
        | Nanos n -> float n / 1_000_000.0
        | Infinity -> System.Double.PositiveInfinity
        | NegativeInfinity -> System.Double.NegativeInfinity

    let toSeconds (self: Duration) : float =
        match self.Value with
        | Millis m -> m / 1_000.0
        | Nanos n -> float n / 1_000_000_000.0
        | Infinity -> System.Double.PositiveInfinity
        | NegativeInfinity -> System.Double.NegativeInfinity

    let toMinutes (self: Duration) : float =
        match self.Value with
        | Millis m -> m / 60_000.0
        | Nanos n -> float n / 60_000_000_000.0
        | Infinity -> System.Double.PositiveInfinity
        | NegativeInfinity -> System.Double.NegativeInfinity

    let toHours (self: Duration) : float =
        match self.Value with
        | Millis m -> m / 3_600_000.0
        | Nanos n -> float n / 3_600_000_000_000.0
        | Infinity -> System.Double.PositiveInfinity
        | NegativeInfinity -> System.Double.NegativeInfinity

    let toDays (self: Duration) : float =
        match self.Value with
        | Millis m -> m / 86_400_000.0
        | Nanos n -> float n / 86_400_000_000_000.0
        | Infinity -> System.Double.PositiveInfinity
        | NegativeInfinity -> System.Double.NegativeInfinity

    let toWeeks (self: Duration) : float =
        match self.Value with
        | Millis m -> m / 604_800_000.0
        | Nanos n -> float n / 604_800_000_000_000.0
        | Infinity -> System.Double.PositiveInfinity
        | NegativeInfinity -> System.Double.NegativeInfinity

    let toNanosUnsafe (self: Duration) : bigint =
        match self.Value with
        | Infinity
        | NegativeInfinity -> raise (System.Exception "Cannot convert infinite duration to nanos")
        | Nanos n -> n
        | Millis m -> roundMillisToNanos m

    let toNanos (self: Duration) : bigint option =
        match self.Value with
        | Infinity
        | NegativeInfinity -> None
        | Nanos n -> Some n
        | Millis m -> Some(roundMillisToNanos m)

    let toHrTime (self: Duration) : float * float =
        match self.Value with
        | Infinity -> System.Double.PositiveInfinity, 0.0
        | NegativeInfinity -> System.Double.NegativeInfinity, 0.0
        | Nanos n -> nanosToHrTime n
        | Millis m -> nanosToHrTime (roundMillisToNanos m)

    // --- pattern matching ---

    /// Pattern matches on the representation of a duration.
    let matchWith
        (onMillis: float -> 'a)
        (onNanos: bigint -> 'a)
        (onInfinity: unit -> 'a)
        (onNegativeInfinity: unit -> 'a)
        (self: Duration)
        : 'a =
        match self.Value with
        | Millis m -> onMillis m
        | Nanos n -> onNanos n
        | Infinity -> onInfinity ()
        | NegativeInfinity -> onNegativeInfinity ()

    // --- ordering / equality ---

    /// Total order returning -1/0/1; `NegativeInfinity` < finite < `Infinity`.
    let order (self: Duration) (that: Duration) : int =
        let isInf v =
            match v with
            | Infinity
            | NegativeInfinity -> true
            | _ -> false

        if isInf self.Value || isInf that.Value then
            match self.Value, that.Value with
            | Infinity, Infinity -> 0
            | NegativeInfinity, NegativeInfinity -> 0
            | Infinity, _ -> 1
            | NegativeInfinity, _ -> -1
            | _, Infinity -> -1
            | _ -> 1
        else
            match self.Value, that.Value with
            | Millis a, Millis b ->
                if a < b then -1
                elif a > b then 1
                else 0
            | _ ->
                let a = toNanosUnsafe self
                let b = toNanosUnsafe that

                if a < b then -1
                elif a > b then 1
                else 0

    /// Numeric equivalence (scale-insensitive across `Millis`/`Nanos`).
    let equivalence (self: Duration) (that: Duration) : bool =
        match self.Value, that.Value with
        | (Infinity | NegativeInfinity), _
        | _, (Infinity | NegativeInfinity) -> self.Value = that.Value
        | Millis a, Millis b -> a = b
        | _ -> toNanosUnsafe self = toNanosUnsafe that

    let equals (self: Duration) (that: Duration) : bool = equivalence self that

    let isLessThan (self: Duration) (that: Duration) : bool = order self that < 0
    let isLessThanOrEqualTo (self: Duration) (that: Duration) : bool = order self that <= 0
    let isGreaterThan (self: Duration) (that: Duration) : bool = order self that > 0
    let isGreaterThanOrEqualTo (self: Duration) (that: Duration) : bool = order self that >= 0

    let min (self: Duration) (that: Duration) : Duration =
        if order self that <= 0 then self else that

    let max (self: Duration) (that: Duration) : Duration =
        if order self that >= 0 then self else that

    let clamp (minimum: Duration) (maximum: Duration) (self: Duration) : Duration =
        if order self minimum < 0 then minimum
        elif order self maximum > 0 then maximum
        else self

    let between (minimum: Duration) (maximum: Duration) (self: Duration) : bool =
        order self minimum >= 0 && order self maximum <= 0

    // --- arithmetic ---

    let private tryBigIntOfFloat (x: float) : bigint option =
        if numIsInteger x then Some(BigInteger x) else None

    let private bigIntOfFloatExn (x: float) : bigint =
        if numIsInteger x then
            BigInteger x
        else
            raise (System.Exception(sprintf "Cannot convert %f to a BigInt" x))

    let divide (self: Duration) (by: float) : Duration option =
        if not (numIsFinite by) then
            None
        elif by = 0.0 then
            None
        else
            match self.Value with
            | Millis m -> Some(millis (m / by))
            | Nanos n ->
                match tryBigIntOfFloat by with
                | Some bi -> Some(nanos (n / bi))
                | None -> None
            | Infinity -> Some(if by > 0.0 then infinity else negativeInfinity)
            | NegativeInfinity -> Some(if by > 0.0 then negativeInfinity else infinity)

    let divideUnsafe (self: Duration) (by: float) : Duration =
        if not (numIsFinite by) then
            zero
        else
            match self.Value with
            | Millis m -> millis (m / by)
            | Nanos n ->
                if by = 0.0 then
                    if n = BigInteger.Zero then
                        zero
                    else
                        let positiveNanos = n > BigInteger.Zero
                        let positiveZero = not (numIsNegative by)

                        if positiveNanos = positiveZero then
                            infinity
                        else
                            negativeInfinity
                else
                    nanos (n / bigIntOfFloatExn by)
            | Infinity ->
                if by > 0.0 then infinity
                elif by < 0.0 then negativeInfinity
                else zero
            | NegativeInfinity ->
                if by > 0.0 then negativeInfinity
                elif by < 0.0 then infinity
                else zero

    let times (self: Duration) (factor: float) : Duration =
        match self.Value with
        | Millis m -> millis (m * factor)
        | Nanos n -> nanos (n * bigIntOfFloatExn factor)
        | Infinity ->
            if factor > 0.0 then infinity
            elif factor < 0.0 then negativeInfinity
            else zero
        | NegativeInfinity ->
            if factor > 0.0 then negativeInfinity
            elif factor < 0.0 then infinity
            else zero

    let subtract (self: Duration) (that: Duration) : Duration =
        match self.Value, that.Value with
        | (Infinity | NegativeInfinity), _
        | _, (Infinity | NegativeInfinity) ->
            match self.Value with
            | Infinity ->
                (match that.Value with
                 | Infinity -> zero
                 | _ -> infinity)
            | NegativeInfinity ->
                (match that.Value with
                 | NegativeInfinity -> zero
                 | _ -> negativeInfinity)
            | _ ->
                (match that.Value with
                 | Infinity -> negativeInfinity
                 | _ -> infinity)
        | Millis a, Millis b -> millis (a - b)
        | _ -> nanos (toNanosUnsafe self - toNanosUnsafe that)

    let sum (self: Duration) (that: Duration) : Duration =
        match self.Value, that.Value with
        | (Infinity | NegativeInfinity), _
        | _, (Infinity | NegativeInfinity) ->
            match self.Value, that.Value with
            | Infinity, NegativeInfinity -> zero
            | NegativeInfinity, Infinity -> zero
            | Infinity, _
            | _, Infinity -> infinity
            | NegativeInfinity, _
            | _, NegativeInfinity -> negativeInfinity
            | _ -> zero
        | Millis a, Millis b -> millis (a + b)
        | _ -> nanos (toNanosUnsafe self + toNanosUnsafe that)

    // --- converting ---

    let parts (self: Duration) : DurationParts =
        match self.Value with
        | Infinity ->
            let inf = System.Double.PositiveInfinity

            { Days = inf
              Hours = inf
              Minutes = inf
              Seconds = inf
              Millis = inf
              Nanos = inf }
        | NegativeInfinity ->
            let ninf = System.Double.NegativeInfinity

            { Days = ninf
              Hours = ninf
              Minutes = ninf
              Seconds = ninf
              Millis = ninf
              Nanos = ninf }
        | _ ->
            let n = toNanosUnsafe self
            let neg = n < BigInteger.Zero
            let a = if neg then -n else n
            let ms = a / bigint1e6
            let sec = ms / bigint1e3
            let mn = sec / bigint60
            let hr = mn / bigint60
            let d = hr / bigint24
            let s = if neg then -1.0 else 1.0

            { Days = s * float d
              Hours = s * float (hr % bigint24)
              Minutes = s * float (mn % bigint60)
              Seconds = s * float (sec % bigint60)
              Millis = s * float (ms % bigint1e3)
              Nanos = s * float (a % bigint1e6) }

    /// Converts a duration to a human-readable string (e.g. "1s 1ms").
    let rec format (self: Duration) : string =
        match self.Value with
        | Infinity -> "Infinity"
        | NegativeInfinity -> "-Infinity"
        | _ ->
            if isZero self then
                "0"
            elif isNegative self then
                "-" + format (abs self)
            else
                let f = parts self

                let piece (v: float) (suffix: string) =
                    if v <> 0.0 then
                        Some(sprintf "%d%s" (int64 v) suffix)
                    else
                        None

                [ piece f.Days "d"
                  piece f.Hours "h"
                  piece f.Minutes "m"
                  piece f.Seconds "s"
                  piece f.Millis "ms"
                  piece f.Nanos "ns" ]
                |> List.choose id
                |> String.concat " "

    /// `String()`-style rendering of the underlying representation.
    let toString (self: Duration) : string =
        match self.Value with
        | Infinity -> "Infinity"
        | NegativeInfinity -> "-Infinity"
        | Nanos n -> string n + " nanos"
        | Millis m -> string m + " millis"

    // --- reducers / combiners ---

    let ReducerSum: DurationReducer = { Combine = sum; Initial = zero }
    let CombinerMax: DurationCombiner = { Combine = max }
    let CombinerMin: DurationCombiner = { Combine = min }
