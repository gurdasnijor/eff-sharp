namespace Effect

/// Slice: wave-4 kernel 2b. The well-known fiber-local reference keys (port of
/// References.ts). A `Reference<'T>` is a typed key with a default; upstream reads
/// and overrides them per-fiber (`Effect.withConcurrency`, `Logger.withMinimumLogLevel`).
///
/// NOTE: full fiber-local *storage/override* is deferred (the `FiberRuntime` does
/// not yet carry a ref map); `Reference.defaultValue` returns the default. This
/// module exists so the consumers (Logger/Tracer/Layer/Stream) have the keys.
/// `CurrentLoggers` lives in `Logger.fs` to break the References↔Logger file cycle.
type Reference<'T> = { Key: string; Default: 'T }

[<RequireQualifiedAccess>]
module Reference =
    let make (key: string) (def: 'T) : Reference<'T> = { Key = key; Default = def }
    let defaultValue (r: Reference<'T>) : 'T = r.Default

[<RequireQualifiedAccess>]
module References =

    /// Below this level, logs are dropped. (References.MinimumLogLevel)
    let MinimumLogLevel: Reference<LogLevel> = Reference.make "effect/References/MinimumLogLevel" LogLevel.Info

    /// The level of the currently-running log call. (References.CurrentLogLevel)
    let CurrentLogLevel: Reference<LogLevel> = Reference.make "effect/References/CurrentLogLevel" LogLevel.Info

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
    let TracerEnabled: Reference<bool> = Reference.make "effect/References/TracerEnabled" true

    /// The level used for unhandled (defect) logging. (References.UnhandledLogLevel)
    let UnhandledLogLevel: Reference<LogLevel option> =
        Reference.make "effect/References/UnhandledLogLevel" (Some LogLevel.Debug)
