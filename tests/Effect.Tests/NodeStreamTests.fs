module Effect.Tests.NodeStreamTests

open Xunit
open Effect
open Effect.Platform.Node

// Exercises the .NET layer of the NodeStream/NodeSink adapters (the xUnit
// surface) against in-memory streams. The Node (node:fs/Readable) layer
// Fable-compiles as part of Effect.Platform.Node and is proven by the JS build.

[<Fact>]
let ``fromReadable streams a MemoryStream incrementally to completion`` () =
    let payload = System.Text.Encoding.UTF8.GetBytes(String.replicate 5000 "ab") // > one 8192 chunk
    let ms = new System.IO.MemoryStream(payload)

    let program = Stream.runCollect (NodeStream.fromReadable (fun () -> ms))

    match Effect.runSync Context.empty program with
    | Success chunks ->
        let got = chunks |> List.toArray |> Array.collect id
        Assert.Equal<byte[]>(payload, got)
        Assert.True(chunks.Length >= 2, "payload larger than one buffer should arrive in multiple chunks")
    | Failure c -> failwithf "stream failed: %s" (Cause.render c)

[<Fact>]
let ``fromWritable writes every chunk to a MemoryStream then closes`` () =
    let ms = new System.IO.MemoryStream()
    let input = [ "hello, "B; "node "B; "sink"B ]

    let program =
        Stream.run (NodeSink.fromWritable (fun () -> ms)) (Stream.fromIterable input)

    match Effect.runSync Context.empty program with
    | Success() ->
        // MemoryStream.Close was called by the sink; read the captured bytes back.
        Assert.Equal("hello, node sink", System.Text.Encoding.UTF8.GetString(ms.ToArray()))
    | Failure c -> failwithf "sink failed: %s" (Cause.render c)
