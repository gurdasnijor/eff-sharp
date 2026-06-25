module ConsoleSpec

open Effect
open Effect.Vitest

let private render (args: obj[]) =
    args |> Array.map string |> String.concat " "

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

describe "Console" (fun () ->
    test "log routes to the Log method" (fun () ->
        let events = withCapture (fun _ -> Console.log [| box "hello"; box 42 |])
        toBe (events = [ "log", "hello 42" ]) true)

    test "error/warn/info/debug route to their methods" (fun () ->
        let events =
            withCapture (fun _ ->
                Console.error [| box "e" |]
                |> Effect.zipRight (Console.warn [| box "w" |])
                |> Effect.zipRight (Console.info [| box "i" |])
                |> Effect.zipRight (Console.debug [| box "d" |]))

        toBe (events = [ "error", "e"; "warn", "w"; "info", "i"; "debug", "d" ]) true)

    test "assertLog logs only on false" (fun () ->
        let events =
            withCapture (fun _ ->
                Console.assertLog true [| box "kept-quiet" |]
                |> Effect.zipRight (Console.assertLog false [| box "boom" |]))

        toBe (events = [ "assert", "false boom" ]) true)

    test "withGroup brackets the body with group and groupEnd" (fun () ->
        let events =
            withCapture (fun _ -> Console.withGroup (Some "G") false (Console.log [| box "body" |]))

        toBe (events = [ "group", "G"; "log", "body"; "groupEnd", "" ]) true)

    test "withGroup collapsed uses groupCollapsed" (fun () ->
        let events =
            withCapture (fun _ -> Console.withGroup (Some "G") true (Console.log [| box "x" |]))

        toBe (events = [ "groupCollapsed", "G"; "log", "x"; "groupEnd", "" ]) true)

    test "withTime brackets the body with time and timeEnd" (fun () ->
        let events =
            withCapture (fun _ -> Console.withTime (Some "t") (Console.log [| box "work" |]))

        toBe (events = [ "time", "t"; "log", "work"; "timeEnd", "t" ]) true)

    test "scoped group runs groupEnd when the scope closes" (fun () ->
        let log = ResizeArray()
        let ctx = Context.make Console.tag (capturing log)

        let program =
            Scope.scoped (fun scope ->
                Console.group scope (Some "S") false
                |> Effect.zipRight (Console.log [| box "inside" |]))

        run ctx program
        toBe (List.ofSeq log = [ "group", "S"; "log", "inside"; "groupEnd", "" ]) true)

    test "consoleWith falls back to live when no service is provided" (fun () ->
        match Effect.runSync Context.empty (Console.log [| box "to-stdout" |]) with
        | Success() -> ()
        | Failure c -> failwith (Cause.render c)))
