module Kernel2bSpec

open Effect
open Effect.Vitest

describe "Kernel2b" (fun () ->
    test "References expose typed defaults" (fun () ->
        toEqual (Reference.defaultValue References.MinimumLogLevel) LogLevel.Info
        toBe (Reference.defaultValue References.TracerEnabled) true
        toEqual (Reference.defaultValue References.CurrentConcurrency) (None: int option))

    test "Tracer span has ids and a child inherits the parent's traceId" (fun () ->
        let t = Tracer.Default
        let root = t.Span("root", Tracer.defaultSpanOptions)
        toBe root.Name "root"
        toBe (root.SpanId.Length > 0) true

        let child =
            t.Span(
                "child",
                { Tracer.defaultSpanOptions with
                    Parent = Some(LiveSpan root) }
            )

        toBe child.TraceId root.TraceId
        toBe (child.SpanId <> root.SpanId) true)

    test "Logger.formatSimple renders timestamp, level and message" (fun () ->
        let entry =
            { Message = box "hi"
              LogLevel = LogLevel.Warn
              Date = System.DateTimeOffset(2024, 3, 15, 12, 0, 0, System.TimeSpan.Zero)
              Annotations = Map.empty }

        let s = Logger.formatSimple entry
        toContain s "2024-03-15T12:00:00.000Z"
        toContain s "level=WARN"
        toContain s "message=hi")

    test "a custom logger receives entries" (fun () ->
        let captured = System.Collections.Generic.List<string>()
        let logger = Logger.make (fun e -> captured.Add(Logger.formatSimple e))

        logger.Log
            { Message = box "x"
              LogLevel = LogLevel.Error
              Date = System.DateTimeOffset.UtcNow
              Annotations = Map.empty }

        toBe captured.Count 1
        toContain captured.[0] "level=ERROR")

    test "Logger.log runs as an effect" (fun () ->
        toEqual (Effect.runSync () (Logger.log LogLevel.Info (box "hello"))) (Exit.succeed ())))
