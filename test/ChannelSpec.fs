module ChannelSpec

open Effect
open Effect.Vitest

let private collect (c: Channel<'a, string, unit>) : 'a list =
    match Effect.runSync () (Channel.runCollect c) with
    | Success xs -> xs
    | Failure cause -> failwithf "channel failed: %s" (Cause.render cause)

describe "Channel" (fun () ->
    test "writeAll then map" (fun () ->
        let c = Channel.writeAll [ 1; 2; 3 ] |> Channel.map (fun n -> n * n)
        toBe (collect c = [ 1; 4; 9 ]) true)

    test "write and concat sequence outputs" (fun () ->
        let c = Channel.concat (Channel.write 1) (Channel.writeAll [ 2; 3 ])
        toBe (collect c = [ 1; 2; 3 ]) true)

    test "empty writes nothing" (fun () ->
        toBe (collect (Channel.empty: Channel<int, string, unit>) = []) true)

    test "fromStream / toStream round-trip" (fun () ->
        let s = Stream.range 1 4
        toBe (collect (Channel.fromStream s) = [ 1; 2; 3; 4 ]) true

        let summed =
            match Effect.runSync () (Stream.run Sink.sum (Channel.toStream (Channel.writeAll [ 1; 2; 3; 4 ]))) with
            | Success n -> n
            | Failure cause -> failwithf "%s" (Cause.render cause)

        toBe summed 10))
