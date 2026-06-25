module Effect.Tests.NodePlatformTests

open Xunit
open Effect
open Effect.Platform.Node

// Foundation test for the Effect.Platform.Node package: proves the new project is
// referenced, its module resolves, and core's runner executes its effects. Under
// .NET (this xUnit surface) `runtime` is ".net"; the Fable/JS path returns "node".

[<Fact>]
let ``NodePlatform.runtime is .net on the BCL test surface`` () =
    match Effect.runSync () NodePlatform.runtime with
    | Success r -> Assert.Equal(".net", r)
    | Failure c -> Assert.Fail(Cause.render c)
