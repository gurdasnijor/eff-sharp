/// @title Fibers, structured concurrency, and interruption
///
/// `Effect.fork` runs an effect on a new *fiber* (a lightweight, cooperatively
/// scheduled green thread). You `join` it for its value, `await` it for the full
/// `Exit`, or `interrupt` it. The headline guarantee: interruption is
/// **cooperative**, so honoring an interrupt is a normal `Exit.Interrupt` return
/// — which means **finalizers still run**. A cancellation can never skip your
/// cleanup. (Upstream `Effect.fork` / `Fiber`.)
module EffSharp.AiDocs.Concurrency

open Effect
open EffSharp.AiDocs.Shared

// ---------------------------------------------------------------------------
// fork + join: run two computations as fibers and combine their results.
// ---------------------------------------------------------------------------

let parallelish: Effect<int, string, unit> =
    effect {
        let! f1 = Effect.fork (Effect.sync (fun () -> 6 * 7))
        let! f2 = Effect.fork (Effect.sync (fun () -> 100 + 1))
        let! a = Effect.join f1 // wait for the first fiber's value
        let! b = Effect.join f2
        return a + b
    }

// ---------------------------------------------------------------------------
// interrupt + finalizer: the structured-concurrency headline.  ⭐
// We fork a fiber that "would" sleep for a minute, but attach a finalizer. We
// interrupt it almost immediately — and the finalizer STILL runs, because
// interruption unwinds through `ensuring` rather than around it.
// ---------------------------------------------------------------------------

let interruptRunsFinalizer: Effect<Exit<unit, string>, string, unit> =
    effect {
        let longRunning =
            Effect.sleep 60_000 // a cancellable yield point
            |> Effect.ensuring (Effect.sync (fun () -> printfn "    [child] finalizer ran on interrupt"))

        let! fiber = Effect.fork longRunning
        do! Effect.sleep 20 // let the child start sleeping
        do! Effect.interrupt fiber // request interrupt AND wait until it stops
        return! Effect.await fiber // observe the outcome
    }

let private describeExit (exit: Exit<unit, string>) : string =
    match exit with
    | Success() -> "completed"
    // an interrupted fiber finishes with an Interrupt reason in its Cause
    | Failure { Reasons = [ Reason.Interrupt _ ] } -> "interrupted (cleanly)"
    | Failure cause -> Cause.render cause

let demo () =
    banner "07 · Concurrency — fibers & interruption"
    printfn "  fork + join     => %d" (run parallelish)
    printfn "  interrupt fiber => %s" (describeExit (run interruptRunsFinalizer))
