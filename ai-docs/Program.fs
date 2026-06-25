/// Entry point: runs the whole ai-docs tour so the examples are not just
/// type-checked but actually exercised. `dotnet run --project ai-docs`.
module EffSharp.AiDocs.Program

[<EntryPoint>]
let main _argv =
    printfn "eff-sharp ai-docs — runnable examples"

    EffSharp.AiDocs.Basics.demo ()
    EffSharp.AiDocs.Services.demo ()
    EffSharp.AiDocs.Errors.demo ()
    EffSharp.AiDocs.Resources.demo ()
    EffSharp.AiDocs.Running.demo ()
    EffSharp.AiDocs.PubSubDocs.demo ()
    EffSharp.AiDocs.Concurrency.demo ()
    EffSharp.AiDocs.State.demo ()
    EffSharp.AiDocs.Queues.demo ()
    EffSharp.AiDocs.Stream02.run ()
    EffSharp.AiDocs.ManagedRuntime.demo ()
    EffSharp.AiDocs.DataToolkit.demo ()
    EffSharp.AiDocs.Batching05.run ()
    EffSharp.AiDocs.Schedule06.run ()
    EffSharp.AiDocs.DateTime07.run ()
    EffSharp.AiDocs.Observability08.run ()
    EffSharp.AiDocs.EffectTests.demo ()
    EffSharp.AiDocs.LayerTests.demo ()
    EffSharp.AiDocs.CEPower.demo ()
    EffSharp.AiDocs.StmPower.demo ()

    printfn ""
    printfn "done."
    0
