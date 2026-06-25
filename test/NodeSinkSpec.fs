module NodeSinkSpec

open Fable.Core
open Effect
open Effect.Platform.Node
open Effect.Vitest

[<Emit("(function(){ const chunks = []; return { writable: { write: function(chunk){ chunks.push(Array.from(chunk).join(',')); }, end: function(){ chunks.push('end'); } }, chunks: chunks }; })()")>]
let private makeWritableCapture () : obj = jsNative

[<Emit("$0.writable")>]
let private writable (_capture: obj) : obj = jsNative

[<Emit("$0.chunks")>]
let private chunks (_capture: obj) : string[] = jsNative

describe "NodeSink" (fun () ->
    itEffect "writes chunks to a Node writable and ends it" (fun () ->
        let capture = makeWritableCapture ()

        Stream.fromIterable [ [| 1uy; 2uy |]; [| 3uy |] ]
        |> Stream.run (NodeSink.fromWritable (fun () -> writable capture))
        |> Effect.map (fun () ->
            toEqual (chunks capture |> Array.toList) [ "1,2"; "3"; "end" ])))
