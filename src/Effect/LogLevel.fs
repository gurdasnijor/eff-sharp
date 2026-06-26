namespace Effect

/// Slice: wave-4 kernel 2. Log severity levels, ordered by an internal sort key
/// (port of LogLevel.ts; ordinals from internal/effect.ts `logLevelToOrder`).
/// `All` admits everything, `None` admits nothing.
///
/// `[<RequireQualifiedAccess>]`: the cases (`None`/`Error`/`All`/…) would otherwise
/// pollute the `Effect` namespace and shadow `Option.None`/`Result.Error`.
[<RequireQualifiedAccess>]
type LogLevel =
    | All
    | Fatal
    | Error
    | Warn
    | Info
    | Debug
    | Trace
    | None

[<RequireQualifiedAccess>]
module LogLevel =

    /// All levels in the upstream declaration order. (LogLevel.values)
    let values: LogLevel list =
        [ LogLevel.All
          LogLevel.Fatal
          LogLevel.Error
          LogLevel.Warn
          LogLevel.Info
          LogLevel.Debug
          LogLevel.Trace
          LogLevel.None ]

    /// Internal sort key — NOT an external severity scale. (LogLevel.getOrdinal)
    let getOrdinal (self: LogLevel) : int =
        match self with
        | LogLevel.All -> System.Int32.MinValue
        | LogLevel.Trace -> 0
        | LogLevel.Debug -> 10_000
        | LogLevel.Info -> 20_000
        | LogLevel.Warn -> 30_000
        | LogLevel.Error -> 40_000
        | LogLevel.Fatal -> 50_000
        | LogLevel.None -> System.Int32.MaxValue

    /// Total order on the ordinal. (LogLevel.Order)
    let Order: Order<LogLevel> = fun a b -> sign (compare (getOrdinal a) (getOrdinal b))

    let Equivalence: Equivalence<LogLevel> = fun a b -> a = b

    let isGreaterThan (self: LogLevel) (that: LogLevel) = getOrdinal self > getOrdinal that
    let isGreaterThanOrEqualTo (self: LogLevel) (that: LogLevel) = getOrdinal self >= getOrdinal that
    let isLessThan (self: LogLevel) (that: LogLevel) = getOrdinal self < getOrdinal that
    let isLessThanOrEqualTo (self: LogLevel) (that: LogLevel) = getOrdinal self <= getOrdinal that

    /// Whether `self` would be emitted at the given `minimum` threshold. The
    /// context-reading `Effect<bool>` variant can build on `References.MinimumLogLevel`.
    /// (LogLevel.isEnabled)
    let isEnabled (self: LogLevel) (minimum: LogLevel) : bool = isGreaterThanOrEqualTo self minimum

    /// Upstream label string. (LogLevel toString)
    let label (self: LogLevel) : string =
        (sprintf "%A" self).Replace("LogLevel.", "").ToUpperInvariant()
