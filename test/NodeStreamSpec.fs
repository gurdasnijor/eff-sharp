module NodeStreamSpec

open Fable.Core
open Effect
open Effect.Platform.Node
open Effect.Vitest

[<Emit("(function(){ return { on: function(event, cb){ if (event === 'data') { setTimeout(function(){ cb(new Uint8Array($0)); cb(new Uint8Array($1)); }, 0); } if (event === 'end') { setTimeout(function(){ cb(); }, 0); } return this; } }; })()")>]
let private readableFromTwoChunks (_a: byte[]) (_b: byte[]) : obj = jsNative

describe "NodeStream" (fun () ->
    itEffect "bridges a Node readable into an Effect stream" (fun () ->
        NodeStream.fromReadable (fun () -> readableFromTwoChunks [| 1uy; 2uy |] [| 3uy |])
        |> Stream.runCollect
        |> Effect.map (fun chunks -> toEqual chunks [ [| 1uy; 2uy |]; [| 3uy |] ])))
