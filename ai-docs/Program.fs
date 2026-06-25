<<<<<<< HEAD
/// Entry point: runs the whole ai-docs tour so the examples are not just
/// type-checked but actually exercised. `dotnet run --project ai-docs`.
=======
/// @title Runnable entry point
///
/// `dotnet run --project ai-docs` executes every topic's `run ()` in order, so
/// the gallery is not just type-checked — it actually produces output you can
/// read alongside the source.
>>>>>>> wave6/showcase2
module EffSharp.AiDocs.Program

[<EntryPoint>]
let main _argv =
    printfn "eff-sharp ai-docs — runnable examples"
<<<<<<< HEAD

    EffSharp.AiDocs.Basics.demo ()
    EffSharp.AiDocs.Services.demo ()
    EffSharp.AiDocs.Errors.demo ()
    EffSharp.AiDocs.Resources.demo ()
    EffSharp.AiDocs.Running.demo ()
    EffSharp.AiDocs.PubSubDocs.demo ()
    EffSharp.AiDocs.ManagedRuntime.demo ()
    EffSharp.AiDocs.EffectTests.demo ()
    EffSharp.AiDocs.LayerTests.demo ()
    EffSharp.AiDocs.CEPower.demo ()

    printfn ""
    printfn "done."
=======
    EffSharp.AiDocs.Stream02.run ()
    EffSharp.AiDocs.Schedule06.run ()
    EffSharp.AiDocs.DateTime07.run ()
    EffSharp.AiDocs.Observability08.run ()
    EffSharp.AiDocs.Batching05.run ()
    printfn "\nAll examples ran.\n"
>>>>>>> wave6/showcase2
    0
