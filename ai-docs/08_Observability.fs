/// @title Observability — logging, the Console service, and tracing
///
/// Three pillars, all as plain values/effects:
///   * `Logger` / `LogLevel` — levelled logging with a minimum-level gate.
///   * `Console` — console output as a swappable `Context` *service* (so tests
///     can capture output instead of hitting stdout).
///   * `Tracer` — spans with parent/child trace propagation and attributes.
///
/// Mirrors upstream ai-docs `08_observability`. Fiber-local logger sets, metrics,
/// and live exporters are later slices; the model below is the faithful core.
module EffSharp.AiDocs.Observability08

open Effect
open EffSharp.AiDocs.Demo

// ───────────────────────────────────────────────────────────────────────────
// 1. LogLevel — an ordered DU; routing is a NATIVE match
// ───────────────────────────────────────────────────────────────────────────

/// Decide where a level goes by matching the `LogLevel` DU directly — no
/// `LogLevel.match` combinator needed.
let routeOf (level: LogLevel) : string =
    match level with
    | LogLevel.Fatal
    | LogLevel.Error -> "pager"
    | LogLevel.Warn -> "alerts channel"
    | _ -> "stdout only"

// ───────────────────────────────────────────────────────────────────────────
// 2. Logger.log — gated by References.MinimumLogLevel (default Info)
// ───────────────────────────────────────────────────────────────────────────

/// `Logger.log` writes through the default logger *if* the level clears the
/// minimum threshold. Debug/Trace sit below the default Info gate, so they are
/// silently dropped — `LogLevel.isEnabled` is the predicate behind that.
let logging: Effect<unit, string, unit> =
    effect {
        do! Logger.log LogLevel.Info (box "service started")
        do! Logger.log LogLevel.Warn (box "cache nearly full")
        do! Logger.log LogLevel.Debug (box "you will NOT see this (below Info)")
    }

// ───────────────────────────────────────────────────────────────────────────
// 3. Console as a service — swap stdout for a capturing fake (great for tests)
// ───────────────────────────────────────────────────────────────────────────

/// Build an in-memory console by copy-updating the live one — F# record update
/// overrides just the methods we care about; everything else stays live.
let makeCapturingConsole (sink: ResizeArray<string>) : Console =
    let render (args: obj[]) =
        args |> Array.map string |> String.concat " "

    { Console.live with
        Info = fun args -> sink.Add("INFO " + render args)
        Warn = fun args -> sink.Add("WARN " + render args) }

/// A program written against the abstract `Console` service (env = `Context`).
let consoleProgram: Effect<unit, string, Context> =
    effect {
        do! Console.info [| box "processing order"; box 1001 |]
        do! Console.warn [| box "retrying payment" |]
    }

// ───────────────────────────────────────────────────────────────────────────
// 4. Tracer — spans, parent/child trace propagation, attributes
// ───────────────────────────────────────────────────────────────────────────

/// Summarise a span's lifecycle by matching the `SpanStatus` DU.
let spanSummary (s: Span) : string =
    match s.Status with
    | Started t -> sprintf "%s started @%d (open)" s.Name t
    | Ended(startT, endT) -> sprintf "%s took %dms" s.Name (endT - startT)

let run () : unit =
    section "08 Observability"

    printfn
        "level routing    : Error→%s · Warn→%s · Info→%s"
        (routeOf LogLevel.Error)
        (routeOf LogLevel.Warn)
        (routeOf LogLevel.Info)

    printfn "Logger.log (Debug is gated out by the Info minimum):"
    runUnit logging

    // Provide the capturing console; the SAME program now records instead of
    // printing. This is the payoff of routing IO through a Context service.
    let captured = ResizeArray<string>()

    Effect.provideService Console.tag (makeCapturingConsole captured) consoleProgram
    |> runUnit

    printfn "captured console : %A" (List.ofSeq captured)

    // Tracing: a server span and a child client span. The child inherits the
    // parent's TraceId — that propagation is what stitches a distributed trace.
    let tracer = Tracer.make ()

    let parent =
        tracer.Span(
            "http.request",
            { Tracer.defaultSpanOptions with
                Kind = Server }
        )

    let child =
        tracer.Span(
            "db.query",
            { Tracer.defaultSpanOptions with
                Kind = Client
                Parent = Some(LiveSpan parent) }
        )

    child.Attributes <- child.Attributes |> Map.add "db.system" (box "postgres")
    child.Status <- Ended(0L, 12L) // pretend the query took 12ms

    printfn "trace ids match  : %b" (parent.TraceId = child.TraceId)
    printfn "  parent: %s" (spanSummary parent)
    printfn "  child : %s  attrs=%A" (spanSummary child) child.Attributes
