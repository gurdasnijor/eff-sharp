/// @title Runnable entry point
///
/// `dotnet run --project ai-docs` executes every topic's `run ()` in order, so
/// the gallery is not just type-checked — it actually produces output you can
/// read alongside the source.
module EffSharp.AiDocs.Program

[<EntryPoint>]
let main _argv =
    printfn "eff-sharp ai-docs — runnable examples"
    EffSharp.AiDocs.Stream02.run ()
    EffSharp.AiDocs.Schedule06.run ()
    EffSharp.AiDocs.DateTime07.run ()
    EffSharp.AiDocs.Observability08.run ()
    EffSharp.AiDocs.Batching05.run ()
    printfn "\nAll examples ran.\n"
    0
