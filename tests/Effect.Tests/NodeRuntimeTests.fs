module Effect.Tests.NodeRuntimeTests

open Xunit
open Effect
open Effect.Platform.Node

// Proves NodeRuntime.layer provides the merged Node platform services so a program
// can be run with a single `Layer.provide`. (.NET surface; the Node build is
// covered by the Fable compile.)

[<Fact>]
let ``layer provides Clock + FileSystem so a mixed program runs with one provide`` () =
    let program =
        effect {
            // touches Clock (currentTimeMillis) and FileSystem (via a temp dir round-trip)
            let! t = Clock.currentTimeMillis

            let! dir =
                FileSystem.makeTempDirectory
                    { FileSystem.tempOptions with
                        Prefix = Some "effsharp-nr-" }

            let! existed = FileSystem.exists dir
            do! FileSystem.remove dir FileSystem.removeRecursive
            return (t >= 0L, existed)
        }

    match Effect.runSync () (Layer.provide NodeRuntime.layer program) with
    | Success(timeOk, existed) ->
        Assert.True(timeOk, "clock returned a non-negative time")
        Assert.True(existed, "filesystem created the temp dir")
    | Failure c -> failwithf "program failed: %s" (Cause.render c)
