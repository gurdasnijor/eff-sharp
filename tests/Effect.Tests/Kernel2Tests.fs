module Effect.Tests.Kernel2Tests

open Xunit
open Effect

// LogLevel ---------------------------------------------------------------------

[<Fact>]
let ``LogLevel orders by severity and isEnabled honors the threshold`` () =
    Assert.True(LogLevel.isGreaterThan LogLevel.Error LogLevel.Info)
    Assert.Equal(-1, LogLevel.Order LogLevel.Trace LogLevel.Debug)
    Assert.True(LogLevel.isEnabled LogLevel.Error LogLevel.Info) // Error >= Info
    Assert.False(LogLevel.isEnabled LogLevel.Debug LogLevel.Error) // Debug < Error
    Assert.True(LogLevel.isEnabled LogLevel.Trace LogLevel.All) // everything passes All
    Assert.False(LogLevel.isEnabled LogLevel.Fatal LogLevel.None) // nothing passes None

// Scheduler --------------------------------------------------------------------

[<Fact>]
let ``Scheduler dispatcher runs scheduled tasks in priority order`` () =
    let d = Scheduler.makeDispatcher ()
    let log = System.Collections.Generic.List<int>()
    d.ScheduleTask((fun () -> log.Add 3), 30)
    d.ScheduleTask((fun () -> log.Add 1), 10)
    d.ScheduleTask((fun () -> log.Add 2), 20)
    d.Flush()
    Assert.Equal<int list>([ 1; 2; 3 ], List.ofSeq log)

// Fiber module -----------------------------------------------------------------

[<Fact>]
let ``Fiber.awaitAll collects each fiber's Exit`` () =
    let prog: Effect<Exit<int, string> list, string, unit> =
        effect {
            let! f1 = Effect.fork (Effect.succeed 1)
            let! f2 = Effect.fork (Effect.fail "x")
            return! Fiber.awaitAll [ f1; f2 ]
        }

    match Effect.runSync () prog with
    | Success exits ->
        Assert.Equal(2, List.length exits)
        Assert.Equal<Exit<int, string>>(Exit.succeed 1, exits.[0])
        Assert.True(Exit.isFailure exits.[1])
    | Failure c -> Assert.Fail(sprintf "outer failed: %s" (Cause.render c))

[<Fact>]
let ``Fiber.poll is None while running, Some once completed`` () =
    let prog: Effect<bool * bool, string, unit> =
        effect {
            let! f =
                Effect.fork (
                    effect {
                        do! Effect.sleep 1000
                        return 7
                    }
                )

            let! p1 = Fiber.poll f // still running
            do! Effect.interrupt f
            let! p2 = Fiber.poll f // completed (interrupted)
            return (Option.isNone p1, Option.isSome p2)
        }

    match Effect.runSync () prog with
    | Success(noneWhileRunning, someAfter) ->
        Assert.True(noneWhileRunning)
        Assert.True(someAfter)
    | Failure c -> Assert.Fail(sprintf "outer failed: %s" (Cause.render c))
