namespace Effect

/// Slice: wave-4 kernel 2b. Logging (port of Logger.ts). Minimal-but-faithful: a
/// `Logger<'Message,'Output>`, a console `defaultLogger`, simple formatting, and
/// `Logger.log`. The active logger set / minimum level are `Context.Reference`s,
/// matching Effect v4's replacement for FiberRef-backed logging state.
type LogEntry =
    { Message: obj
      LogLevel: LogLevel
      Date: System.DateTimeOffset
      Annotations: Map<string, obj> }

/// A sink from log entries of `'Message` to `'Output`. (Logger.Logger)
type Logger<'Message, 'Output> = { Log: LogEntry -> 'Output }

[<RequireQualifiedAccess>]
module Logger =

    /// Build a logger from a log function. (Logger.make)
    let make (f: LogEntry -> 'Output) : Logger<'Message, 'Output> = { Log = f }

    /// Transform a logger's output. (Logger.map)
    let map (f: 'A -> 'B) (self: Logger<'Message, 'A>) : Logger<'Message, 'B> = { Log = self.Log >> f }

    /// `2024-03-15T12:00:00.000Z level=INFO message=...` (Logger.formatSimple)
    let formatSimple (entry: LogEntry) : string =
        let ts =
            entry.Date.ToString("yyyy-MM-ddTHH:mm:ss.fffZ", System.Globalization.CultureInfo.InvariantCulture)

        sprintf "%s level=%s message=%O" ts (LogLevel.label entry.LogLevel) entry.Message

    /// The default logger: format simply and write to stdout. (Logger.defaultLogger)
    let defaultLogger: Logger<obj, unit> =
        make (fun entry -> System.Console.WriteLine(formatSimple entry))

    /// The ambient set of active loggers (default = `[defaultLogger]`). Lives
    /// here (not in `References`) to break the References↔Logger file cycle.
    /// (References.CurrentLoggers)
    let CurrentLoggers: Reference<Logger<obj, unit> list> =
        Reference.make "effect/References/CurrentLoggers" [ defaultLogger ]

    let private contextFromEnv (env: 'R) : Context =
        match box env with
        | :? Context as ctx -> ctx
        | _ -> Context.empty

    /// Emit a message at `level` through the active loggers, if enabled by the
    /// active minimum level. (Logger.log)
    let log (level: LogLevel) (message: obj) : Effect<unit, 'E, 'R> =
        Effect(fun _ env ->
            async {
                let ctx = contextFromEnv env
                let minimumLevel = Context.getReference References.MinimumLogLevel ctx

                if LogLevel.isEnabled level minimumLevel then
                    let loggers = Context.getReference CurrentLoggers ctx

                    let entry =
                        { Message = message
                          LogLevel = level
                          Date = System.DateTimeOffset.UtcNow
                          Annotations = Context.getReference References.CurrentLogAnnotations ctx }

                    for logger in loggers do
                        logger.Log entry

                return Success()
            })
