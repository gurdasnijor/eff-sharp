module LoggerSpec

open System
open Effect
open Effect.Vitest

let private entry message level =
    { Message = box message
      LogLevel = level
      Date = DateTimeOffset.FromUnixTimeMilliseconds(1710504000000L)
      Annotations = Map.empty }

describe "Logger" (fun () ->
    test "formatSimple emits timestamp, level, and message" (fun () ->
        let formatted = Logger.formatSimple (entry "hello" LogLevel.Info)

        toBe formatted "2024-03-15T12:00:00.000Z level=INFO message=hello")

    test "map transforms logger output" (fun () ->
        let logger = Logger.make (fun e -> Logger.formatSimple e)
        let mapped = Logger.map (fun (text: string) -> text.Contains("level=WARN")) logger

        toBe (mapped.Log(entry "careful" LogLevel.Warn)) true)

    test "current loggers reference defaults to the default logger" (fun () ->
        toBe (Reference.defaultValue Logger.CurrentLoggers |> List.length) 1))
