namespace Effect

/// The well-known ambient reference keys (port of References.ts). In Effect v4
/// these are `Context.Reference`s: typed Context keys with defaults, replacing
/// the old FiberRef/FiberRefs/Differ family.
///
/// `CurrentLoggers` lives in `Logger.fs` to break the References↔Logger file cycle.

[<RequireQualifiedAccess>]
module References =

    /// Below this level, logs are dropped. (References.MinimumLogLevel)
    let MinimumLogLevel: Reference<LogLevel> =
        Reference.make "effect/References/MinimumLogLevel" LogLevel.Info

    /// The level of the currently-running log call. (References.CurrentLogLevel)
    let CurrentLogLevel: Reference<LogLevel> =
        Reference.make "effect/References/CurrentLogLevel" LogLevel.Info

    /// Default concurrency for `Effect.forEach`/`all` — `None` = unbounded.
    /// (References.CurrentConcurrency)
    let CurrentConcurrency: Reference<int option> =
        Reference.make "effect/References/CurrentConcurrency" Option.None

    /// Structured annotations attached to log entries. (References.CurrentLogAnnotations)
    let CurrentLogAnnotations: Reference<Map<string, obj>> =
        Reference.make "effect/References/CurrentLogAnnotations" Map.empty

    /// Active log spans (label, start-millis). (References.CurrentLogSpans)
    let CurrentLogSpans: Reference<(string * int64) list> =
        Reference.make "effect/References/CurrentLogSpans" []

    /// Whether tracing spans are recorded. (References.TracerEnabled)
    let TracerEnabled: Reference<bool> =
        Reference.make "effect/References/TracerEnabled" true

    /// The level used for unhandled (defect) logging. (References.UnhandledLogLevel)
    let UnhandledLogLevel: Reference<LogLevel option> =
        Reference.make "effect/References/UnhandledLogLevel" (Some LogLevel.Debug)
