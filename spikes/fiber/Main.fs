module Fiber.Spike.Main

open BenchmarkDotNet.Running

// `dotnet run -c Release -- test`  -> run the faithfulness tests (pass/fail)
// `dotnet run -c Release -- bench` -> run the BenchmarkDotNet suite
[<EntryPoint>]
let main argv =
    match argv with
    | [| "test" |] -> Faithfulness.run ()
    | _ ->
        BenchmarkSwitcher.FromTypes([| typeof<BindThroughput>; typeof<DeepRecursion>; typeof<ForkJoin> |]).Run(argv)
        |> ignore
        0
