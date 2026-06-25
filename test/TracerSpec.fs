module TracerSpec

open Effect
open Effect.Vitest

describe "Tracer" (fun () ->
    test "external spans expose ids through AnySpan helpers" (fun () ->
        let externalSpan = Tracer.externalSpan "span-1" "trace-1" true
        let any = ExternalRef externalSpan

        toBe externalSpan.Sampled true
        toBe (Tracer.spanId any) "span-1"
        toBe (Tracer.traceId any) "trace-1")

    test "default tracer creates child spans with parent trace id" (fun () ->
        let tracer = Tracer.make ()
        let root = tracer.Span("root", Tracer.defaultSpanOptions)
        let child =
            tracer.Span(
                "child",
                { Tracer.defaultSpanOptions with
                    Kind = Client
                    Parent = Some(LiveSpan root) }
        )

        toBe child.Name "child"
        toBe (child.Kind = Client) true
        toBe child.TraceId root.TraceId
        toBe (child.SpanId <> root.SpanId) true)

    test "addSpanStackTrace records the frame attribute" (fun () ->
        let span = Tracer.Default.Span("span", Tracer.defaultSpanOptions)

        Tracer.addSpanStackTrace span "at test"

        toEqual (span.Attributes |> Map.tryFind "code.stacktrace") (Some(box "at test"))))
