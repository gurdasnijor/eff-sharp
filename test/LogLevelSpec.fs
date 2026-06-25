module LogLevelSpec

open Effect
open Effect.Vitest

describe "LogLevel" (fun () ->
    test "values preserve upstream declaration order" (fun () ->
        toEqual
            LogLevel.values
            [ LogLevel.All
              LogLevel.Fatal
              LogLevel.Error
              LogLevel.Warn
              LogLevel.Info
              LogLevel.Debug
              LogLevel.Trace
              LogLevel.None ])

    test "orders and enables by ordinal threshold" (fun () ->
        toBe (LogLevel.isLessThan LogLevel.Debug LogLevel.Info) true
        toBe (LogLevel.isGreaterThan LogLevel.Error LogLevel.Warn) true
        toBe (LogLevel.isEnabled LogLevel.Error LogLevel.Warn) true
        toBe (LogLevel.isEnabled LogLevel.Info LogLevel.Warn) false)

    test "labels are uppercase case names" (fun () ->
        toBe (LogLevel.label LogLevel.Fatal) "FATAL"
        toBe (LogLevel.label LogLevel.None) "NONE"))
