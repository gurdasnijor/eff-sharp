module SchedulerSpec

open Effect
open Effect.Vitest

describe "Scheduler" (fun () ->
    test "dispatcher runs lower priorities first" (fun () ->
        let dispatcher = Scheduler.makeDispatcher ()
        let ran = ResizeArray<string>()

        dispatcher.ScheduleTask((fun () -> ran.Add "later"), 10)
        dispatcher.ScheduleTask((fun () -> ran.Add "first"), -1)
        dispatcher.ScheduleTask((fun () -> ran.Add "middle"), 0)
        dispatcher.Flush()

        toEqual (List.ofSeq ran) [ "first"; "middle"; "later" ])

    test "flush drains tasks scheduled re-entrantly" (fun () ->
        let dispatcher = Scheduler.makeDispatcher ()
        let ran = ResizeArray<string>()

        dispatcher.ScheduleTask(
            (fun () ->
                ran.Add "outer"
                dispatcher.ScheduleTask((fun () -> ran.Add "inner"), -10)),
            0
        )

        dispatcher.Flush()

        toEqual (List.ofSeq ran) [ "outer"; "inner" ])

    test "exports yield constants" (fun () ->
        toBe Scheduler.MaxOpsBeforeYield 2048
        toBe Scheduler.PreventSchedulerYield System.Int32.MinValue))
