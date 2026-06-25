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
    EffSharp.AiDocs.ManagedRuntime.demo ()
    EffSharp.AiDocs.EffectTests.demo ()
    EffSharp.AiDocs.LayerTests.demo ()
    EffSharp.AiDocs.CEPower.demo ()

    printfn ""
    printfn "done."
    0
