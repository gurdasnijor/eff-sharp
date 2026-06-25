module Effect.Tests.ChannelTests

open Xunit
open Effect

// Smoke tests for the minimal output-writer Channel (no upstream test ported;
// Channel.test.ts exercises the full pull-based primitive, which is deferred —
// see Channel.fs). These cover the write/map/concat core and Stream interop.

let private collect (c: Channel<'a, string, unit>) : 'a list =
    match Effect.runSync () (Channel.runCollect c) with
    | Success xs -> xs
    | Failure cause -> failwithf "channel failed: %s" (Cause.render cause)

[<Fact>]
let ``writeAll then map`` () =
    let c = Channel.writeAll [ 1; 2; 3 ] |> Channel.map (fun n -> n * n)
    Assert.Equal<int list>([ 1; 4; 9 ], collect c)

[<Fact>]
let ``write and concat sequence outputs`` () =
    let c = Channel.concat (Channel.write 1) (Channel.writeAll [ 2; 3 ])
    Assert.Equal<int list>([ 1; 2; 3 ], collect c)

[<Fact>]
let ``empty writes nothing`` () =
    Assert.Equal<int list>([], collect (Channel.empty: Channel<int, string, unit>))

[<Fact>]
let ``fromStream / toStream round-trip`` () =
    let s = Stream.range 1 4
    // Stream -> Channel -> collect
    Assert.Equal<int list>([ 1; 2; 3; 4 ], collect (Channel.fromStream s))
    // Channel -> Stream -> run a Sink over it
    let summed =
        match Effect.runSync () (Stream.run Sink.sum (Channel.toStream (Channel.writeAll [ 1; 2; 3; 4 ]))) with
        | Success n -> n
        | Failure c -> failwithf "%s" (Cause.render c)

    Assert.Equal(10, summed)
