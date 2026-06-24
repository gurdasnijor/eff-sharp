module Effect.Tests.Kernel2bTests

open Xunit
open Effect

[<Fact>]
let ``References expose typed defaults`` () =
    Assert.Equal(LogLevel.Info, Reference.defaultValue References.MinimumLogLevel)
    Assert.True(Reference.defaultValue References.TracerEnabled)
    Assert.Equal<int option>(None, Reference.defaultValue References.CurrentConcurrency)

[<Fact>]
let ``Tracer span has ids and a child inherits the parent's traceId`` () =
    let t = Tracer.Default
    let root = t.Span("root", Tracer.defaultSpanOptions)
    Assert.Equal("root", root.Name)
    Assert.True(root.SpanId.Length > 0)
    let child = t.Span("child", { Tracer.defaultSpanOptions with Parent = Some(LiveSpan root) })
    Assert.Equal(root.TraceId, child.TraceId) // same trace
    Assert.NotEqual<string>(root.SpanId, child.SpanId) // distinct spans

[<Fact>]
let ``Logger.formatSimple renders timestamp, level and message`` () =
    let entry =
        { Message = box "hi"
          LogLevel = LogLevel.Warn
          Date = System.DateTimeOffset(2024, 3, 15, 12, 0, 0, System.TimeSpan.Zero)
          Annotations = Map.empty }

    let s = Logger.formatSimple entry
    Assert.Contains("2024-03-15T12:00:00.000Z", s)
    Assert.Contains("level=WARN", s)
    Assert.Contains("message=hi", s)

[<Fact>]
let ``a custom logger receives entries`` () =
    let captured = System.Collections.Generic.List<string>()
    let logger = Logger.make (fun e -> captured.Add(Logger.formatSimple e))

    logger.Log
        { Message = box "x"
          LogLevel = LogLevel.Error
          Date = System.DateTimeOffset.UtcNow
          Annotations = Map.empty }

    Assert.Equal(1, captured.Count)
    Assert.Contains("level=ERROR", captured.[0])

[<Fact>]
let ``Logger.log runs as an effect`` () =
    Assert.Equal<Exit<unit, string>>(Exit.succeed (), Effect.runSync () (Logger.log LogLevel.Info (box "hello")))
