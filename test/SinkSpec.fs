module SinkSpec

open Effect
open Effect.Vitest

let private run (sink: Sink<'a, 'b, string, unit>) (s: Stream<'a, string, unit>) : 'b =
    match Effect.runSync () (Stream.run sink s) with
    | Success x -> x
    | Failure cause -> failwithf "sink failed: %s" (Cause.render cause)

describe "Sink" (fun () ->
    test "reduce equivalence with Array.reduce" (fun () ->
        let s = Stream.range 1 9
        let viaSink = run (Sink.reduce "" (fun acc n -> acc + string n)) s
        let viaList = run (Sink.collect ()) s |> List.fold (fun acc n -> acc + string n) ""
        toBe viaSink viaList
        toBe viaSink "123456789")

    test "reduceWhile terminates in the middle" (fun () ->
        let r = run (Sink.reduceWhile 0 (fun n -> n <= 5) (+)) (Stream.range 1 9)
        toBe r 6)

    test "reduceWhile immediate termination" (fun () ->
        let r = run (Sink.reduceWhile 0 (fun n -> n <= -1) (+)) (Stream.range 1 9)
        toBe r 0)

    test "reduceWhile no termination" (fun () ->
        let r = run (Sink.reduceWhile 0 (fun n -> n <= 500) (+)) (Stream.range 1 9)
        toBe r 45)

    test "reduceWhile over empty yields initial" (fun () ->
        let r = run (Sink.reduceWhile 0 (fun _ -> true) (+)) Stream.empty
        toBe r 0)

    test "collect gathers all elements" (fun () ->
        toBe (run (Sink.collect ()) (Stream.make [ 1; 2; 3 ]) = [ 1; 2; 3 ]) true
        toBe (run (Sink.collect ()) (Stream.empty: Stream<int, string, unit>) = []) true)

    test "sum adds the inputs" (fun () ->
        toBe (run Sink.sum (Stream.range 1 9)) 45
        toBe (run Sink.sum (Stream.range 1 100)) 5050
        toBe (run Sink.sum Stream.empty) 0)

    test "count counts the inputs" (fun () ->
        toBe (run Sink.count (Stream.make [ 1; 2; 3; 4 ])) 4
        toBe (run Sink.count (Stream.empty: Stream<int, string, unit>)) 0)

    test "forEach runs an effect per element" (fun () ->
        let seen = System.Collections.Generic.List<int>()
        run (Sink.forEach (fun n -> Effect.sync (fun () -> seen.Add n))) (Stream.make [ 1; 2; 3 ])
        toBe (List.ofSeq seen = [ 1; 2; 3 ]) true)

    test "drain consumes everything" (fun () ->
        run Sink.drain (Stream.range 1 5)
        toBe true true)

    test "head takes the first and stops the producer" (fun () ->
        let produced = System.Collections.Generic.List<int>()

        let s =
            Stream.range 1 1000
            |> Stream.tap (fun n -> Effect.sync (fun () -> produced.Add n))

        let r = run (Sink.head ()) s
        toBe (r = Some 1) true
        toBe produced.Count 1)

    test "head over empty is None" (fun () ->
        let r = run (Sink.head ()) (Stream.empty: Stream<int, string, unit>)
        toBe (r = None) true)

    test "last takes the final element" (fun () ->
        toBe (run (Sink.last ()) (Stream.make [ 1; 2; 3 ]) = Some 3) true
        toBe (run (Sink.last ()) (Stream.empty: Stream<int, string, unit>) = None) true)

    test "succeed ignores input and short-circuits" (fun () ->
        let produced = System.Collections.Generic.List<int>()

        let s =
            Stream.range 1 1000
            |> Stream.tap (fun n -> Effect.sync (fun () -> produced.Add n))

        toBe (run (Sink.succeed 99) s) 99
        toBe produced.Count 1)

    test "map transforms the output" (fun () ->
        let r = run (Sink.sum |> Sink.map (fun n -> n * 10)) (Stream.range 1 9)
        toBe r 450)

    test "mapInput adapts the input" (fun () ->
        let sink = Sink.sum |> Sink.mapInput (fun (s: string) -> s.Length)
        toBe (run sink (Stream.make [ "a"; "bb"; "ccc" ])) 6)

    test "fromWrite writes each input then closes once" (fun () ->
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
            toBe (w = [ 1; 2; 3 ]) true
            toBe c 1
        | Failure cause -> failwithf "sink failed: %s" (Cause.render cause)))
