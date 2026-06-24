namespace Fiber.Spike

open BenchmarkDotNet.Attributes

/// Bind throughput — left-nested chain of N binds, run to completion. Measures
/// per-bind interpreter overhead of each representation.
[<MemoryDiagnoser; ShortRunJob>]
type BindThroughput() =
    [<Params(10_000, 100_000)>]
    member val N = 0 with get, set

    [<Benchmark(Baseline = true, Description = "Proto1: Async + CancellationToken")>]
    member this.Proto1() =
        let mutable e = Proto1.succeed 0
        for _ in 1 .. this.N do e <- Proto1.bind (fun x -> Proto1.succeed (x + 1)) e
        Proto1.run e

    [<Benchmark(Description = "Proto2: Async + explicit fiber state")>]
    member this.Proto2() =
        let mutable e = Proto2.succeed 0
        for _ in 1 .. this.N do e <- Proto2.bind (fun x -> Proto2.succeed (x + 1)) e
        Proto2.run e

/// Deep right-nested recursion — stack-safety + overhead at depth.
[<MemoryDiagnoser; ShortRunJob>]
type DeepRecursion() =
    [<Params(50_000)>]
    member val Depth = 0 with get, set

    [<Benchmark(Baseline = true, Description = "Proto1")>]
    member this.Proto1() =
        let rec loop n =
            if n = 0 then Proto1.succeed 0 else Proto1.bind (fun _ -> loop (n - 1)) (Proto1.succeed ())
        Proto1.run (loop this.Depth)

    [<Benchmark(Description = "Proto2")>]
    member this.Proto2() =
        let rec loop n =
            if n = 0 then Proto2.succeed 0 else Proto2.bind (fun _ -> loop (n - 1)) (Proto2.succeed ())
        Proto2.run (loop this.Depth)

/// Fork + join overhead — spawn a trivial fiber and await it, N times.
[<MemoryDiagnoser; ShortRunJob>]
type ForkJoin() =
    [<Params(1_000)>]
    member val Count = 0 with get, set

    [<Benchmark(Baseline = true, Description = "Proto1")>]
    member this.Proto1() =
        let mutable acc = 0
        for _ in 1 .. this.Count do
            let f = Proto1.fork (Proto1.succeed 1)
            match Proto1.run (Proto1.join f) with
            | Success v -> acc <- acc + v
            | _ -> ()
        acc

    [<Benchmark(Description = "Proto2")>]
    member this.Proto2() =
        let mutable acc = 0
        for _ in 1 .. this.Count do
            let f = Proto2.fork (Proto2.succeed 1)
            match Proto2.run (Proto2.join f) with
            | Success v -> acc <- acc + v
            | _ -> ()
        acc
