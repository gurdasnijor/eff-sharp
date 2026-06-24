module Effect.Benchmarks.Main

open BenchmarkDotNet.Running

[<EntryPoint>]
let main argv =
    // `dotnet run -c Release -- --filter *` runs all; pass a filter to scope.
    BenchmarkSwitcher.FromAssembly(typeof<MutableHashSetBench>.Assembly).Run(argv) |> ignore
    0
