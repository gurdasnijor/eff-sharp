module Effect.Tests.SinkTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/Sink.test.ts (pragmatic
// subset). Upstream drives sinks via `Stream.run` / `Stream.transduce`; this
// port uses `Stream.run sink`. Chunked/leftover/transduce cases are omitted
// (see Sink.fs deferral note).

let private run (sink: Sink<'a, 'b, string, unit>) (s: Stream<'a, string, unit>) : 'b =
    match Effect.runSync () (Stream.run sink s) with
    | Success x -> x
    | Failure c -> failwithf "sink failed: %s" (Cause.render c)

// --- reduce / fold ---

[<Fact>]
let ``reduce equivalence with Array.reduce`` () =
    // upstream "reduce": equivalence with folding the collected list.
    let s = Stream.range 1 9
    let viaSink = run (Sink.reduce "" (fun acc n -> acc + string n)) s
    let viaList = run (Sink.collect ()) s |> List.fold (fun acc n -> acc + string n) ""
    Assert.Equal(viaList, viaSink)
    Assert.Equal("123456789", viaSink)

// --- reduceWhile (short-circuit) ---

[<Fact>]
let ``reduceWhile terminates in the middle`` () =
    // upstream: range(1,9), cont n<=5, (+)  => 6  (0+1+2+3, stops when state=6)
    let r = run (Sink.reduceWhile 0 (fun n -> n <= 5) (+)) (Stream.range 1 9)
    Assert.Equal(6, r)

[<Fact>]
let ``reduceWhile immediate termination`` () =
    let r = run (Sink.reduceWhile 0 (fun n -> n <= -1) (+)) (Stream.range 1 9)
    Assert.Equal(0, r)

[<Fact>]
let ``reduceWhile no termination`` () =
    let r = run (Sink.reduceWhile 0 (fun n -> n <= 500) (+)) (Stream.range 1 9)
    Assert.Equal(45, r)

[<Fact>]
let ``reduceWhile over empty yields initial`` () =
    let r = run (Sink.reduceWhile 0 (fun _ -> true) (+)) Stream.empty
    Assert.Equal(0, r)

// --- collect / sum / count ---

[<Fact>]
let ``collect gathers all elements`` () =
    Assert.Equal<int list>([ 1; 2; 3 ], run (Sink.collect ()) (Stream.make [ 1; 2; 3 ]))
    Assert.Equal<int list>([], run (Sink.collect ()) (Stream.empty: Stream<int, string, unit>))

[<Fact>]
let ``sum adds the inputs`` () =
    Assert.Equal(45, run Sink.sum (Stream.range 1 9))
    Assert.Equal(5050, run Sink.sum (Stream.range 1 100))
    Assert.Equal(0, run Sink.sum Stream.empty)

[<Fact>]
let ``count counts the inputs`` () =
    Assert.Equal(4, run Sink.count (Stream.make [ 1; 2; 3; 4 ]))
    Assert.Equal(0, run Sink.count (Stream.empty: Stream<int, string, unit>))

// --- forEach / drain ---

[<Fact>]
let ``forEach runs an effect per element`` () =
    let seen = System.Collections.Generic.List<int>()
    run (Sink.forEach (fun n -> Effect.sync (fun () -> seen.Add n))) (Stream.make [ 1; 2; 3 ])
    Assert.Equal<int list>([ 1; 2; 3 ], List.ofSeq seen)

[<Fact>]
let ``drain consumes everything`` () =
    Assert.Equal((), run Sink.drain (Stream.range 1 5))

// --- head / last (head short-circuits the producer) ---

[<Fact>]
let ``head takes the first and stops the producer`` () =
    let produced = System.Collections.Generic.List<int>()

    let s =
        Stream.range 1 1000
        |> Stream.tap (fun n -> Effect.sync (fun () -> produced.Add n))

    let r = run (Sink.head ()) s
    Assert.Equal(Some 1, r)
    Assert.Equal(1, produced.Count) // stopped after the first element

[<Fact>]
let ``head over empty is None`` () =
    Assert.Equal(None, run (Sink.head ()) (Stream.empty: Stream<int, string, unit>))

[<Fact>]
let ``last takes the final element`` () =
    Assert.Equal(Some 3, run (Sink.last ()) (Stream.make [ 1; 2; 3 ]))
    Assert.Equal(None, run (Sink.last ()) (Stream.empty: Stream<int, string, unit>))

// --- succeed / map / mapInput ---

[<Fact>]
let ``succeed ignores input and short-circuits`` () =
    let produced = System.Collections.Generic.List<int>()

    let s =
        Stream.range 1 1000
        |> Stream.tap (fun n -> Effect.sync (fun () -> produced.Add n))

    Assert.Equal(99, run (Sink.succeed 99) s)
    // Short-circuits on the first element: the push-fold producer cannot be
    // pre-empted before its first push, so exactly 1 element is produced
    // (not all 1000), and none is folded into the result.
    Assert.Equal(1, produced.Count)

[<Fact>]
let ``map transforms the output`` () =
    let r = run (Sink.sum |> Sink.map (fun n -> n * 10)) (Stream.range 1 9)
    Assert.Equal(450, r)

[<Fact>]
let ``mapInput adapts the input`` () =
    // sum of string lengths
    let sink = Sink.sum |> Sink.mapInput (fun (s: string) -> s.Length)
    Assert.Equal(6, run sink (Stream.make [ "a"; "bb"; "ccc" ]))

// --- fromWrite (write + close) ---

[<Fact>]
let ``fromWrite writes each input then closes once`` () =
    let program =
        effect {
            let! written = Ref.make []
            let! closed = Ref.make 0

            let sink =
                Sink.fromWrite (fun (x: int) -> Ref.update written (fun xs -> xs @ [ x ])) (fun () ->
                    Ref.update closed ((+) 1))

            do! Stream.run sink (Stream.range 1 3)
            let! w = Ref.get written
            let! c = Ref.get closed
            return (w, c)
        }

    match Effect.runSync () program with
    | Success(w, c) ->
        Assert.Equal<int list>([ 1; 2; 3 ], w)
        Assert.Equal(1, c) // close runs exactly once, after the input ends
    | Failure cause -> failwithf "sink failed: %s" (Cause.render cause)
