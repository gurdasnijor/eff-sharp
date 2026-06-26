module StreamSpec

open Effect
open Effect.Vitest

let private collect (s: Stream<'a, string, unit>) : 'a list =
    match Effect.runSync () (Stream.runCollect s) with
    | Success xs -> xs
    | Failure cause -> failwithf "stream failed: %s" (Cause.render cause)

describe "Stream" (fun () ->
    test "stream { } yields, yield!s, and for-loops" (fun () ->
        let s: Stream<int, string, unit> =
            stream {
                yield 1
                yield 2
                yield! Stream.range 3 4

                for x in [ 5; 6 ] do
                    yield x * 10
            }

        toBe (collect s = [ 1; 2; 3; 4; 50; 60 ]) true)

    test "map / filter / flatMap compose" (fun () ->
        let s =
            Stream.range 1 6
            |> Stream.filter (fun n -> n % 2 = 0)
            |> Stream.map (fun n -> n * n)
            |> Stream.flatMap (fun n -> Stream.make [ n; -n ])

        toBe (collect s = [ 4; -4; 16; -16; 36; -36 ]) true)

    test "take stops the producer early" (fun () ->
        let produced = System.Collections.Generic.List<int>()

        let s =
            Stream.range 1 1000
            |> Stream.tap (fun n -> Effect.sync (fun () -> produced.Add n))
            |> Stream.take 3

        toBe (collect s = [ 1; 2; 3 ]) true
        toBe produced.Count 3)

    test "runForEach runs an effect per element" (fun () ->
        let sum = ref 0

        let program =
            Stream.range 1 5
            |> Stream.runForEach (fun n -> Effect.sync (fun () -> sum.Value <- sum.Value + n))

        toBe (Effect.runSync () program = Exit.succeed ()) true
        toBe sum.Value 15)

    test "mapEffect threads effects through the stream" (fun () ->
        let s = Stream.range 1 3 |> Stream.mapEffect (fun n -> Effect.succeed (n + 100))
        toBe (collect s = [ 101; 102; 103 ]) true)

    test "catchAll switches to a recovery stream" (fun () ->
        let s =
            Stream.concat (Stream.make [ 1; 2 ]) (Stream.fromEffect (Effect.fail "boom"))
            |> Stream.catchAll (fun error -> Stream.make [ String.length error ])

        toBe (collect s = [ 1; 2; 4 ]) true)

    test "repeatEffectOption pulls until None" (fun () ->
        let program =
            effect {
                let! r = Ref.make [ 1; 2; 3 ]

                let pull: Effect<int option, string, unit> =
                    Ref.modify r (function
                        | [] -> None, []
                        | x :: xs -> Some x, xs)

                return! Stream.runCollect (Stream.repeatEffectOption pull)
            }

        match Effect.runSync () program with
        | Success xs -> toBe (xs = [ 1; 2; 3 ]) true
        | Failure cause -> failwithf "stream failed: %s" (Cause.render cause))

    test "repeatEffectOption ends with the pull's error" (fun () ->
        let pull: Effect<int option, string, unit> = Effect.fail "boom"

        match Effect.runSync () (Stream.runCollect (Stream.repeatEffectOption pull)) with
        | Success _ -> failwith "expected failure"
        | Failure cause -> toContain (Cause.render cause) "boom"))
