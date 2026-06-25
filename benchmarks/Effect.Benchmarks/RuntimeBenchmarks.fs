namespace Effect.Benchmarks

open BenchmarkDotNet.Attributes
open Effect

/// Golden runtime workloads for eff-sharp.
///
/// Programs are built once in `GlobalSetup`; each benchmark times only
/// `Effect.runSync` over the prebuilt program, matching the JS bench which builds
/// once and measures the run.
[<MemoryDiagnoser; ShortRunJob>]
type RuntimeBench() =

    // Keep these stable so benchmark history remains comparable.
    let bindN = 10000
    let mapN = 10000
    let refN = 10000
    let forkN = 1000

    let mutable bindProgram = Unchecked.defaultof<Effect<int, string, unit>>
    let mutable mapProgram = Unchecked.defaultof<Effect<int, string, unit>>
    let mutable refProgram = Unchecked.defaultof<Effect<int, string, unit>>
    let mutable forkProgram = Unchecked.defaultof<Effect<unit, string, unit>>

    [<GlobalSetup>]
    member _.Setup() =
        // bind throughput: a left-nested chain of N flatMaps.
        bindProgram <-
            let mutable e: Effect<int, string, unit> = Effect.succeed 0

            for _ in 1..bindN do
                e <- e |> Effect.flatMap (fun x -> Effect.succeed (x + 1))

            e

        // succeed/map: N maps over a succeed.
        mapProgram <-
            let mutable e: Effect<int, string, unit> = Effect.succeed 0

            for _ in 1..mapN do
                e <- e |> Effect.map ((+) 1)

            e

        // Ref: N update/get pairs in one effect.
        refProgram <-
            effect {
                let! r = Ref.make 0

                for _ in 1..refN do
                    do! Ref.update r ((+) 1)
                    let! _ = Ref.get r
                    return ()

                return! Ref.get r
            }

        // fork/join: fork + join a trivial fiber, N times.
        forkProgram <-
            effect {
                for _ in 1..forkN do
                    let! fib = Effect.fork (Effect.succeed 1: Effect<int, string, unit>)
                    let! _ = Effect.join fib
                    return ()
            }

    [<Benchmark(Description = "bind throughput (10k flatMap)")>]
    member _.Bind() = Effect.runSync () bindProgram

    [<Benchmark(Description = "succeed/map (10k map)")>]
    member _.Map() = Effect.runSync () mapProgram

    [<Benchmark(Description = "Ref update/get (10k)")>]
    member _.Ref() = Effect.runSync () refProgram

    [<Benchmark(Description = "fork/join (1k)")>]
    member _.ForkJoin() = Effect.runSync () forkProgram
