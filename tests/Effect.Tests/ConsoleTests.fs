module Effect.Tests.ConsoleTests

open Xunit
open Effect

// Upstream ships no Console.test.ts; these Facts exercise the ported service
// against a capturing mock console (routing + scoped group ordering).

let private render (args: obj[]) =
    args |> Array.map string |> String.concat " "

/// A `Console` that records `(method, renderedArgs)` into `log`.
let private capturing (log: ResizeArray<string * string>) : Console =
    let rec0 name = fun () -> log.Add(name, "")

    let recA name =
        fun (args: obj[]) -> log.Add(name, render args)

    let recL name =
        fun (label: string option) -> log.Add(name, defaultArg label "")

    { Assert =
        fun cond args ->
            if not cond then
                log.Add("assert", sprintf "%b %s" cond (render args))
      Clear = rec0 "clear"
      Count = recL "count"
      CountReset = recL "countReset"
      Debug = recA "debug"
      Dir = fun item _ -> log.Add("dir", string item)
      Dirxml = recA "dirxml"
      Error = recA "error"
      Group = recL "group"
      GroupCollapsed = recL "groupCollapsed"
      GroupEnd = rec0 "groupEnd"
      Info = recA "info"
      Log = recA "log"
      Table = fun data _ -> log.Add("table", string data)
      Time = recL "time"
      TimeEnd = recL "timeEnd"
      TimeLog = fun label args -> log.Add("timeLog", sprintf "%s %s" (defaultArg label "") (render args))
      Trace = recA "trace"
      Warn = recA "warn" }

let private run (ctx: Context) (eff: Effect<'A, 'E, Context>) : 'A =
    match Effect.runSync ctx eff with
    | Success a -> a
    | Failure c -> failwithf "unexpected failure: %s" (Cause.render c)

let private withCapture (f: ResizeArray<string * string> -> Effect<unit, string, Context>) : (string * string) list =
    let log = ResizeArray()
    let ctx = Context.make Console.tag (capturing log)
    run ctx (f log)
    List.ofSeq log

[<Fact>]
let ``log routes to the Log method`` () =
    let events = withCapture (fun _ -> Console.log [| box "hello"; box 42 |])
    Assert.Equal<(string * string) list>([ "log", "hello 42" ], events)

[<Fact>]
let ``error/warn/info/debug route to their methods`` () =
    let events =
        withCapture (fun _ ->
            Console.error [| box "e" |]
            |> Effect.zipRight (Console.warn [| box "w" |])
            |> Effect.zipRight (Console.info [| box "i" |])
            |> Effect.zipRight (Console.debug [| box "d" |]))

    Assert.Equal<(string * string) list>([ "error", "e"; "warn", "w"; "info", "i"; "debug", "d" ], events)

[<Fact>]
let ``assertLog logs only on false`` () =
    let events =
        withCapture (fun _ ->
            Console.assertLog true [| box "kept-quiet" |]
            |> Effect.zipRight (Console.assertLog false [| box "boom" |]))

    Assert.Equal<(string * string) list>([ "assert", "false boom" ], events)

[<Fact>]
let ``withGroup brackets the body with group and groupEnd`` () =
    let events =
        withCapture (fun _ -> Console.withGroup (Some "G") false (Console.log [| box "body" |]))

    Assert.Equal<(string * string) list>([ "group", "G"; "log", "body"; "groupEnd", "" ], events)

[<Fact>]
let ``withGroup collapsed uses groupCollapsed`` () =
    let events =
        withCapture (fun _ -> Console.withGroup (Some "G") true (Console.log [| box "x" |]))

    Assert.Equal<(string * string) list>([ "groupCollapsed", "G"; "log", "x"; "groupEnd", "" ], events)

[<Fact>]
let ``withTime brackets the body with time and timeEnd`` () =
    let events =
        withCapture (fun _ -> Console.withTime (Some "t") (Console.log [| box "work" |]))

    Assert.Equal<(string * string) list>([ "time", "t"; "log", "work"; "timeEnd", "t" ], events)

[<Fact>]
let ``scoped group runs groupEnd when the scope closes`` () =
    let log = ResizeArray()
    let ctx = Context.make Console.tag (capturing log)

    let program =
        Scope.scoped (fun scope ->
            Console.group scope (Some "S") false
            |> Effect.zipRight (Console.log [| box "inside" |]))

    run ctx program
    Assert.Equal<(string * string) list>([ "group", "S"; "log", "inside"; "groupEnd", "" ], List.ofSeq log)

[<Fact>]
let ``consoleWith falls back to live when no service is provided`` () =
    // With an empty context the live console is used; just assert it runs.
    let r = Effect.runSync Context.empty (Console.log [| box "to-stdout" |])

    match r with
    | Success() -> ()
    | Failure c -> Assert.Fail(Cause.render c)
