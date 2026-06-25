namespace Effect

/// Slice: wave-4 kernel 2b. Distributed-tracing spans (port of Tracer.ts).
/// Minimal-but-faithful: the `Span`/`ExternalSpan` model + options + an in-memory
/// `Tracer`. Real exporters/sampling and fiber-local parent propagation are
/// deferred; this provides the surface the upper layers (Layer/Channel/Stream) need.

type SpanKind =
    | Internal
    | Server
    | Client
    | Producer
    | Consumer

type SpanStatus =
    | Started of startTime: int64
    | Ended of startTime: int64 * endTime: int64

/// A span created elsewhere, referenced by ids only. (Tracer.ExternalSpan)
type ExternalSpan =
    { SpanId: string
      TraceId: string
      Sampled: bool }

/// A live, mutable span. (Tracer.Span)
type Span =
    { Name: string
      SpanId: string
      TraceId: string
      Parent: AnySpan option
      Kind: SpanKind
      mutable Status: SpanStatus
      mutable Attributes: Map<string, obj> }

/// Either a live span or an external reference. (Tracer.AnySpan)
and AnySpan =
    | LiveSpan of Span
    | ExternalRef of ExternalSpan

/// Options for starting a span. (Tracer.SpanOptions)
type SpanOptions =
    { Kind: SpanKind
      Attributes: Map<string, obj>
      Parent: AnySpan option }

[<RequireQualifiedAccess>]
module Tracer =

    let defaultSpanOptions: SpanOptions =
        { Kind = Internal
          Attributes = Map.empty
          Parent = Option.None }

    let spanId (span: AnySpan) : string =
        match span with
        | LiveSpan s -> s.SpanId
        | ExternalRef e -> e.SpanId

    let traceId (span: AnySpan) : string =
        match span with
        | LiveSpan s -> s.TraceId
        | ExternalRef e -> e.TraceId

    /// Reference an externally-created span by ids. (Tracer.externalSpan)
    let externalSpan (spanId: string) (traceId: string) (sampled: bool) : ExternalSpan =
        { SpanId = spanId
          TraceId = traceId
          Sampled = sampled }

    /// The `Context` tag for the ambient parent span. (Tracer.ParentSpan)
    let ParentSpan: Tag<AnySpan> = Tag.make<AnySpan> "effect/Tracer/ParentSpan"

    /// Attach a stack frame to a span's attributes. (Tracer.addSpanStackTrace)
    let addSpanStackTrace (span: Span) (frame: string) : unit =
        span.Attributes <- span.Attributes |> Map.add "code.stacktrace" (box frame)

    /// A tracer makes spans. (Tracer.Tracer)
    type Tracer =
        abstract Span: name: string * options: SpanOptions -> Span

    /// In-memory tracer: ids are GUIDs, spans start in `Started`. (default tracer)
    let make () : Tracer =
        { new Tracer with
            member _.Span(name, options) =
                let now = System.DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()
                let newId () = System.Guid.NewGuid().ToString("N")

                let traceId =
                    match options.Parent with
                    | Some p -> traceId p
                    | Option.None -> newId ()

                { Name = name
                  SpanId = newId ()
                  TraceId = traceId
                  Parent = options.Parent
                  Kind = options.Kind
                  Status = Started now
                  Attributes = options.Attributes } }

    let Default: Tracer = make ()
