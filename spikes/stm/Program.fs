module Program

open System
open System.Diagnostics
open System.Threading
open Stm

// ---------------------------------------------------------------------------
// Tiny test harness (assert-in-main; no xunit dependency needed for a spike).
// ---------------------------------------------------------------------------

let mutable private passed = 0
let mutable private failed = 0

let private check name cond =
    if cond then
        passed <- passed + 1
        printfn "  PASS  %s" name
    else
        failed <- failed + 1
        printfn "  FAIL  %s" name

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

/// 1. A single transaction increments a TVar.
let testSingleIncrement () =
    let counter = newTVarUnsafe 0
    atomically (update counter (fun n -> n + 1))
    check "single-tx increment -> 1" (counter.ValueUnsafe = 1)

    let r = atomically (modify counter (fun n -> (sprintf "was %d" n, n + 10)))
    check "modify returns result + stores new value" (r = "was 1" && counter.ValueUnsafe = 11)

/// 2. ALL-OR-NOTHING: a tx that writes two TVars then aborts (via retry, caught
///    by orElse) must leave NO partial writes.
let testAllOrNothing () =
    let a = newTVarUnsafe 100
    let b = newTVarUnsafe 200

    // Left branch writes both then retries; orElse rolls back and runs the
    // (no-op) right branch. Neither write must survive.
    atomically (
        orElse
            (stm {
                do! writeTVar a 999
                do! writeTVar b 888
                return! retry
            })
            (stm { return () })
    )

    check "aborted tx leaves no partial writes" (a.ValueUnsafe = 100 && b.ValueUnsafe = 200)

    // A committed multi-write tx applies BOTH writes atomically.
    atomically (
        stm {
            do! writeTVar a 1
            do! writeTVar b 2
        })

    check "committed multi-write applies all" (a.ValueUnsafe = 1 && b.ValueUnsafe = 2)

/// 3. CONFLICT -> RETRY -> SUCCESS: many threads incrementing a shared counter
///    must never lose an update (optimistic validation forces losers to retry).
let testConflictRetrySucceeds () =
    let counter = newTVarUnsafe 0
    let threads = 8
    let perThread = 5000

    let work () =
        for _ in 1..perThread do
            atomically (update counter (fun n -> n + 1))

    let ts = Array.init threads (fun _ -> Thread(ThreadStart(work)))
    ts |> Array.iter (fun t -> t.Start())
    ts |> Array.iter (fun t -> t.Join())

    let expected = threads * perThread
    check (sprintf "concurrent increments lose nothing (== %d)" expected) (counter.ValueUnsafe = expected)

/// 4. retry BLOCKS until another tx writes the awaited TVar.
let testRetryBlocksUntilWrite () =
    let flag = newTVarUnsafe false
    let observed = newTVarUnsafe 0

    // Waiter: retries until `flag` becomes true, then records a marker.
    let waiter =
        Thread(ThreadStart(fun () ->
            atomically (
                stm {
                    let! ready = readTVar flag
                    if not ready then return! retry else do! writeTVar observed 42
                })))

    waiter.Start()
    // Give the waiter time to reach a blocked retry.
    Thread.Sleep 200
    let beforeWrite = observed.ValueUnsafe // must still be 0 — waiter is blocked
    // Now unblock it.
    atomically (writeTVar flag true)
    let joined = waiter.Join 5000

    check "waiter blocked before the write" (beforeWrite = 0)
    check "waiter unblocked + committed after write" (joined && observed.ValueUnsafe = 42)

// ---------------------------------------------------------------------------
// Lightweight manual throughput probe (always runs; gives numbers even without
// the full BenchmarkDotNet harness). Real numbers come from `-- bench`.
// ---------------------------------------------------------------------------

let private timeMs (f: unit -> unit) =
    let sw = Stopwatch.StartNew()
    f ()
    sw.Stop()
    sw.Elapsed.TotalMilliseconds

let manualContentionProbe () =
    printfn "\nManual contention probe (single shared counter):"
    printfn "  %-8s %-12s %-12s %-10s" "threads" "STM ms" "lock ms" "STM/lock"

    for threads in [ 2; 4; 8 ] do
        let perThread = 20000
        let expected = threads * perThread

        let runStm () =
            let counter = newTVarUnsafe 0

            let work () =
                for _ in 1..perThread do
                    atomically (update counter (fun n -> n + 1))

            let ts = Array.init threads (fun _ -> Thread(ThreadStart(work)))
            ts |> Array.iter (fun t -> t.Start())
            ts |> Array.iter (fun t -> t.Join())
            if counter.ValueUnsafe <> expected then failwithf "STM lost updates: %d <> %d" counter.ValueUnsafe expected

        let runLock () =
            let gate = obj ()
            let mutable counter = 0

            let work () =
                for _ in 1..perThread do
                    lock gate (fun () -> counter <- counter + 1)

            let ts = Array.init threads (fun _ -> Thread(ThreadStart(work)))
            ts |> Array.iter (fun t -> t.Start())
            ts |> Array.iter (fun t -> t.Join())
            if counter <> expected then failwithf "lock lost updates"

        // warm up
        runStm ()
        runLock ()
        let stmMs = timeMs runStm
        let lockMs = timeMs runLock
        printfn "  %-8d %-12.1f %-12.1f %-10.2f" threads stmMs lockMs (stmMs / lockMs)

// ---------------------------------------------------------------------------

[<EntryPoint>]
let main argv =
    if argv |> Array.contains "bench" then
        BenchmarkDotNet.Running.BenchmarkRunner.Run<Benchmarks.Contention>() |> ignore
        0
    else
        printfn "STM spike — TxRef v1 tests\n"
        testSingleIncrement ()
        testAllOrNothing ()
        testConflictRetrySucceeds ()
        testRetryBlocksUntilWrite ()
        manualContentionProbe ()
        printfn "\n%d passed, %d failed" passed failed
        if failed = 0 then 0 else 1
