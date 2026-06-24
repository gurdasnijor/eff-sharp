namespace Fiber.Spike

open System
open System.Diagnostics
open System.Threading

type Exit<'a> =
    | Success of 'a
    | Interrupt
    | Die of exn

/// PROTO 1 — pure Async + CancellationToken.
/// interruption = cancel the token (F# Async checks it at every let!).
/// KNOWN FLAW: Async cancellation unwinds via the cancellation continuation and
/// bypasses try/with/finally, so finalizers do NOT run on interrupt.
module Proto1 =
    type Eff<'a> = Async<Exit<'a>>
    type Fiber<'a> = { Task: Tasks.Task<Exit<'a>>; Cts: CancellationTokenSource }

    let succeed x : Eff<'a> = async { return Success x }
    let sync (f: unit -> 'a) : Eff<'a> = async { return Success(f ()) }

    let bind (f: 'a -> Eff<'b>) (m: Eff<'a>) : Eff<'b> =
        async {
            let! r = m
            match r with
            | Success a -> return! f a
            | Interrupt -> return Interrupt
            | Die e -> return Die e
        }

    let sleep (ms: int) : Eff<unit> =
        async {
            try
                do! Async.Sleep ms
                return Success()
            with :? OperationCanceledException -> return Interrupt
        }

    let ensuring (fin: Eff<unit>) (m: Eff<'a>) : Eff<'a> =
        async {
            let! r = async { try return! m with e -> return Die e }
            let! _ = Async.StartImmediateAsTask(fin, CancellationToken.None) |> Async.AwaitTask
            return r
        }

    let uninterruptible (m: Eff<'a>) : Eff<'a> =
        async {
            let! r = Async.StartImmediateAsTask(m, CancellationToken.None) |> Async.AwaitTask
            return r
        }

    let fork (m: Eff<'a>) : Fiber<'a> =
        let cts = new CancellationTokenSource()
        let task = Async.StartAsTask(m, cancellationToken = cts.Token)

        let wrapped =
            task.ContinueWith(fun (t: Tasks.Task<Exit<'a>>) ->
                if t.IsCanceled then Interrupt
                elif t.IsFaulted then Die(t.Exception :> exn)
                else t.Result)

        { Task = wrapped; Cts = cts }

    let join (fib: Fiber<'a>) : Eff<'a> = async { return! Async.AwaitTask fib.Task }
    let interrupt (fib: Fiber<'a>) : Eff<unit> = async { fib.Cts.Cancel(); return Success() }

    let run (m: Eff<'a>) : Exit<'a> =
        try Async.RunSynchronously m with :? OperationCanceledException -> Interrupt

/// PROTO 2 — Async + explicit Fiber state (interrupt flag + mask counter checked
/// at yield points). Faithful interruption/masking/finalizers, no reified interpreter.
module Proto2 =
    type FiberState =
        { mutable Interrupted: bool
          mutable Mask: int }
        static member Create() = { Interrupted = false; Mask = 0 }

    type Eff<'a> = FiberState -> Async<Exit<'a>>
    type Fiber<'a> = { Task: Tasks.Task<Exit<'a>>; State: FiberState }

    let private checkInterrupt (fs: FiberState) = fs.Interrupted && fs.Mask = 0

    let succeed x : Eff<'a> = fun _ -> async { return Success x }
    let sync (f: unit -> 'a) : Eff<'a> = fun _ -> async { return Success(f ()) }

    let bind (f: 'a -> Eff<'b>) (m: Eff<'a>) : Eff<'b> =
        fun fs ->
            async {
                let! r = m fs
                match r with
                | Success a -> if checkInterrupt fs then return Interrupt else return! (f a) fs
                | Interrupt -> return Interrupt
                | Die e -> return Die e
            }

    let sleep (ms: int) : Eff<unit> =
        fun fs ->
            async {
                let sw = Stopwatch.StartNew()
                let mutable interrupted = false

                while sw.ElapsedMilliseconds < int64 ms && not interrupted do
                    if checkInterrupt fs then interrupted <- true else do! Async.Sleep 2

                return (if interrupted then Interrupt else Success())
            }

    let ensuring (fin: Eff<unit>) (m: Eff<'a>) : Eff<'a> =
        fun fs ->
            async {
                let! r = async { try return! m fs with e -> return Die e }
                fs.Mask <- fs.Mask + 1
                let! _ = fin fs
                fs.Mask <- fs.Mask - 1
                return r
            }

    let uninterruptible (m: Eff<'a>) : Eff<'a> =
        fun fs ->
            async {
                fs.Mask <- fs.Mask + 1
                let! r = m fs
                fs.Mask <- fs.Mask - 1
                if checkInterrupt fs then return Interrupt else return r
            }

    let fork (m: Eff<'a>) : Fiber<'a> =
        let child = FiberState.Create()
        let task = Async.StartAsTask(m child)
        { Task = task; State = child }

    let join (fib: Fiber<'a>) : Eff<'a> = fun _ -> async { return! Async.AwaitTask fib.Task }
    let interrupt (fib: Fiber<'a>) : Eff<unit> = fun _ -> async { fib.State.Interrupted <- true; return Success() }

    let run (m: Eff<'a>) : Exit<'a> = Async.RunSynchronously(m (FiberState.Create()))

/// Faithfulness tests (pass/fail) — the decisive evidence, kept runnable.
module Faithfulness =
    let run () : int =
        let mutable failures = 0
        let check name cond = if cond then printfn "  ok   %s" name else (failures <- failures + 1; printfn "  FAIL %s" name)

        printfn "=== PROTO 1 (Async + CancellationToken) ==="
        (let p = Proto1.fork (Proto1.succeed 42)
         check "fork/join" (Proto1.run (Proto1.join p) = Success 42))
        (let ran = ref false
         let body = Proto1.ensuring (Proto1.sync (fun () -> ran.Value <- true)) (Proto1.sleep 1000)
         let p = Proto1.fork body
         Thread.Sleep 50
         Proto1.run (Proto1.interrupt p) |> ignore
         check "interrupt->Interrupt" (Proto1.run (Proto1.join p) = Interrupt)
         check "finalizer ran on interrupt" ran.Value)

        printfn "=== PROTO 2 (Async + explicit fiber state) ==="
        (let p = Proto2.fork (Proto2.succeed 42)
         check "fork/join" (Proto2.run (Proto2.join p) = Success 42))
        (let ran = ref false
         let body = Proto2.ensuring (Proto2.sync (fun () -> ran.Value <- true)) (Proto2.sleep 1000)
         let p = Proto2.fork body
         Thread.Sleep 50
         Proto2.run (Proto2.interrupt p) |> ignore
         check "interrupt->Interrupt" (Proto2.run (Proto2.join p) = Interrupt)
         check "finalizer ran on interrupt" ran.Value)
        (let completed = ref false
         let masked = Proto2.uninterruptible (Proto2.bind (fun () -> Proto2.sync (fun () -> completed.Value <- true)) (Proto2.sleep 200))
         let p = Proto2.fork masked
         Thread.Sleep 50
         Proto2.run (Proto2.interrupt p) |> ignore
         let r = Proto2.run (Proto2.join p)
         check "masking: region completed" completed.Value
         check "masking: interrupt honored after" (r = Interrupt))

        printfn "%s" (if failures = 0 then "ALL PASS" else sprintf "%d FAILURE(S)" failures)
        failures
