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
#if FABLE_COMPILER
        // Fable lacks System.Collections.Generic.PriorityQueue; a small list with
        // min-priority extraction is faithful here (dispatcher queues are short and
        // drained eagerly each flush).
        let queue = ResizeArray<struct (int * (unit -> unit))>()
        let enqueue (task: unit -> unit) (priority: int) = queue.Add(struct (priority, task))

        let tryDequeue () : (unit -> unit) option =
            if queue.Count = 0 then
                None
            else
                let mutable mi = 0

                for i in 1 .. queue.Count - 1 do
                    let struct (pi, _) = queue.[i]
                    let struct (pm, _) = queue.[mi]
                    if pi < pm then mi <- i

                let struct (_, task) = queue.[mi]
                queue.RemoveAt mi
                Some task
#else
        let queue = PriorityQueue<unit -> unit, int>()
        let enqueue (task: unit -> unit) (priority: int) = queue.Enqueue(task, priority)

        let tryDequeue () : (unit -> unit) option =
            if queue.Count > 0 then Some(queue.Dequeue()) else None
#endif
        let mutable running = false

        interface SchedulerDispatcher with
            member _.ScheduleTask(task, priority) = enqueue task priority

            member this.Flush() =
                if not running then
                    running <- true

                    try
                        let mutable next = tryDequeue ()

                        while next.IsSome do
                            next.Value()
                            next <- tryDequeue ()
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
