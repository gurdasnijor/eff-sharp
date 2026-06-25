namespace Effect

open System.Collections.Generic

/// Slice: wave-4 kernel 2. Cooperative task scheduling (port of Scheduler.ts).
///
/// In eff-sharp's `Async`-based core the .NET thread pool actually *runs* effects,
/// so this is deliberately thin: it provides the `SchedulerDispatcher` abstraction
/// — a priority-ordered task queue that batched wake-ups (Queue/PubSub) schedule
/// callbacks onto and `flush` to run — plus the yield-budget constants the run
/// loop uses to decide when to yield. Upstream's `MixedScheduler`/`setImmediate`
/// tick machinery is collapsed onto an immediate priority drain.
type SchedulerDispatcher =
    /// Schedule `task` to run; lower `priority` runs first. (scheduleTask)
    abstract ScheduleTask: task: (unit -> unit) * priority: int -> unit
    /// Run all currently-scheduled tasks in priority order. (flush)
    abstract Flush: unit -> unit

[<RequireQualifiedAccess>]
module Scheduler =

    /// Operations a fiber runs before cooperatively yielding. (MaxOpsBeforeYield)
    [<Literal>]
    let MaxOpsBeforeYield = 2048

    /// A priority key that suppresses a yield. (PreventSchedulerYield)
    [<Literal>]
    let PreventSchedulerYield = System.Int32.MinValue

    /// A dispatcher draining a priority queue. Re-entrant `scheduleTask` during a
    /// `flush` is honored in the same drain (matching upstream's "running" loop).
    type private PriorityDispatcher() =
        let queue = PriorityQueue<unit -> unit, int>()
        let mutable running = false

        interface SchedulerDispatcher with
            member _.ScheduleTask(task, priority) = queue.Enqueue(task, priority)

            member this.Flush() =
                if not running then
                    running <- true

                    try
                        while queue.Count > 0 do
                            (queue.Dequeue()) ()
                    finally
                        running <- false

    /// The interface upstream calls `Scheduler`: a factory for dispatchers.
    type Scheduler =
        abstract MakeDispatcher: unit -> SchedulerDispatcher

    /// The default scheduler — each `makeDispatcher` is an independent priority queue.
    let Default: Scheduler =
        { new Scheduler with
            member _.MakeDispatcher() =
                PriorityDispatcher() :> SchedulerDispatcher }

    /// Convenience: a fresh dispatcher from the default scheduler. (makeDispatcher)
    let makeDispatcher () : SchedulerDispatcher = Default.MakeDispatcher()
