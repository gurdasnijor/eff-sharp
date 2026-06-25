module Kernel2Spec

open Effect
open Effect.Vitest

describe "Kernel2" (fun () ->
    test "LogLevel orders by severity and isEnabled honors the threshold" (fun () ->
        toBe (LogLevel.isGreaterThan LogLevel.Error LogLevel.Info) true
        toBe (LogLevel.Order LogLevel.Trace LogLevel.Debug) -1
        toBe (LogLevel.isEnabled LogLevel.Error LogLevel.Info) true
        toBe (LogLevel.isEnabled LogLevel.Debug LogLevel.Error) false
        toBe (LogLevel.isEnabled LogLevel.Trace LogLevel.All) true
        toBe (LogLevel.isEnabled LogLevel.Fatal LogLevel.None) false)

    test "Scheduler dispatcher runs scheduled tasks in priority order" (fun () ->
        let d = Scheduler.makeDispatcher ()
        let log = System.Collections.Generic.List<int>()
        d.ScheduleTask((fun () -> log.Add 3), 30)
        d.ScheduleTask((fun () -> log.Add 1), 10)
        d.ScheduleTask((fun () -> log.Add 2), 20)
        d.Flush()
        toEqual (List.ofSeq log) [ 1; 2; 3 ])

    itEffect "Fiber.awaitAll collects each fiber's Exit" (fun () ->
        effect {
            let! f1 = Effect.fork (Effect.succeed 1)
            let! f2 = Effect.fork (Effect.fail "x")
            let! exits = Fiber.awaitAll [ f1; f2 ]
            toBe (List.length exits) 2
            toEqual exits.[0] (Exit.succeed 1)
            toBe (Exit.isFailure exits.[1]) true
        })

    itEffect "Fiber.poll is None while running, Some once completed" (fun () ->
        effect {
            let! f =
                Effect.fork (
                    effect {
                        do! Effect.sleep 1000
                        return 7
                    }
                )

            let! p1 = Fiber.poll f
            do! Effect.interrupt f
            let! p2 = Fiber.poll f
            toBe (Option.isNone p1) true
            toBe (Option.isSome p2) true
        }))
