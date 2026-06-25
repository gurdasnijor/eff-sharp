namespace Effect.Benchmarks

open System.Collections.Generic
open BenchmarkDotNet.Attributes
open Effect

// Each benchmark compares the CURRENT port implementation against the native
// FSharp.Core/.NET alternative the alignment review proposed, so the Tier 1/2
// decisions can be made on measured numbers rather than theory.
// ShortRunJob keeps total runtime sane; MemoryDiagnoser reports allocations.

/// Tier 2 — MutableHashSet: current `MutableHashMap<'V,bool>` backing vs native HashSet.
[<MemoryDiagnoser; ShortRunJob>]
type MutableHashSetBench() =
    [<Params(1000, 10000)>]
    member val N = 0 with get, set

    member val Data = [||] with get, set

    [<GlobalSetup>]
    member this.Setup() =
        this.Data <- [| for i in 0 .. this.N - 1 -> i |]

    [<Benchmark(Baseline = true, Description = "current: map-backed MutableHashSet")>]
    member this.Current() =
        let s = MutableHashSet.fromIterable this.Data
        let mutable hits = 0

        for i in this.Data do
            if MutableHashSet.has s i then
                hits <- hits + 1

        hits

    [<Benchmark(Description = "native: HashSet<'V>(Structural)")>]
    member this.Native() =
        let s = HashSet<int>(this.Data, HashIdentity.Structural)
        let mutable hits = 0

        for i in this.Data do
            if s.Contains i then
                hits <- hits + 1

        hits

/// Tier 1 — Chunk.dedupe: hand-rolled HashSet+filter vs Array.distinct.
[<MemoryDiagnoser; ShortRunJob>]
type DedupeBench() =
    [<Params(10000)>]
    member val N = 0 with get, set

    member val Chunk = Chunk.empty<int> with get, set
    member val Arr = [||] with get, set

    [<GlobalSetup>]
    member this.Setup() =
        // ~50% duplicates
        let xs = [ for i in 0 .. this.N - 1 -> i % (this.N / 2) ]
        this.Arr <- List.toArray xs
        this.Chunk <- Chunk.make xs

    [<Benchmark(Baseline = true, Description = "current: dedupeArray (HashSet+filter)")>]
    member this.Current() = Chunk.dedupe this.Chunk

    [<Benchmark(Description = "native: Array.distinct")>]
    member this.Native() = Array.distinct this.Arr

/// Tier 1 — HashMap.modifyAt: tryFind-then-add/remove (2 lookups) vs Map.change (1 pass).
[<MemoryDiagnoser; ShortRunJob>]
type HashMapModifyBench() =
    [<Params(10000)>]
    member val N = 0 with get, set

    member val Hm = HashMap.empty<int, int> with get, set
    member val M = Map.empty<int, int> with get, set

    [<GlobalSetup>]
    member this.Setup() =
        let pairs = [ for i in 0 .. this.N - 1 -> i, i ]
        this.Hm <- HashMap.fromIterable pairs
        this.M <- Map.ofList pairs

    [<Benchmark(Baseline = true, Description = "current: HashMap.modifyAt")>]
    member this.Current() =
        let mutable hm = this.Hm

        for i in 0..999 do
            hm <- HashMap.modifyAt hm i (Option.map ((+) 1))

        hm

    [<Benchmark(Description = "native: Map.change")>]
    member this.Native() =
        let mutable m = this.M

        for i in 0..999 do
            m <- Map.change i (Option.map ((+) 1)) m

        m

/// Tier 2 — Graph.dijkstra: linear-scan extract-min (O(V^2)). Scaling reveals
/// whether a PriorityQueue (O((V+E)logV)) is worth the change.
[<MemoryDiagnoser; ShortRunJob>]
type DijkstraBench() =
    [<Params(200, 800)>]
    member val N = 0 with get, set

    member val Graph = Unchecked.defaultof<Graph<int, float>> with get, set

    [<GlobalSetup>]
    member this.Setup() =
        // a chain 0-1-2-...-(N-1) with unit weights plus a few skip edges
        this.Graph <-
            Graph.directed (
                Some(fun m ->
                    for i in 0 .. this.N - 1 do
                        Graph.addNode m i |> ignore

                    for i in 0 .. this.N - 2 do
                        Graph.addEdge m i (i + 1) 1.0 |> ignore

                    for i in 0 .. this.N - 3 do
                        Graph.addEdge m i (i + 2) 2.5 |> ignore)
            )

    [<Benchmark(Description = "current: linear-scan dijkstra")>]
    member this.Current() =
        Graph.dijkstra this.Graph 0 (this.N - 1) id
