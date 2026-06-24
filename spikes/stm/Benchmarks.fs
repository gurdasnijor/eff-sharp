module Benchmarks

open System.Threading
open BenchmarkDotNet.Attributes
open Stm

/// Contention benchmark: `Threads` workers each perform `PerThread` atomic
/// increments of one shared counter. Compares the optimistic STM against a plain
/// `lock` baseline (the lower bound we must justify ourselves against).
[<MemoryDiagnoser>]
type Contention() =

    [<Params(2, 4, 8)>]
    member val Threads = 0 with get, set

    member val PerThread = 2000 with get, set

    /// Run `work` on `Threads` OS threads, return after all join.
    member private this.Fan(work: unit -> unit) =
        let threads =
            Array.init this.Threads (fun _ -> Thread(ThreadStart(work)))

        threads |> Array.iter (fun t -> t.Start())
        threads |> Array.iter (fun t -> t.Join())

    [<Benchmark(Baseline = true)>]
    member this.LockBaseline() =
        let gate = obj ()
        let mutable counter = 0

        this.Fan(fun () ->
            for _ in 1 .. this.PerThread do
                lock gate (fun () -> counter <- counter + 1))

        counter

    [<Benchmark>]
    member this.StmOptimistic() =
        let counter = newTVarUnsafe 0

        this.Fan(fun () ->
            for _ in 1 .. this.PerThread do
                atomically (update counter (fun n -> n + 1)))

        counter.ValueUnsafe
