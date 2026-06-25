module Effect.Tests.StreamTests

open Xunit
open Effect

let private collect (s: Stream<'a, string, unit>) : 'a list =
    match Effect.runSync () (Stream.runCollect s) with
    | Success xs -> xs
    | Failure c -> failwithf "stream failed: %s" (Cause.render c)

[<Fact>]
let ``stream { } yields, yield!s, and for-loops`` () =
    let s: Stream<int, string, unit> =
        stream {
            yield 1
            yield 2
            yield! Stream.range 3 4

            for x in [ 5; 6 ] do
                yield x * 10
        }

    Assert.Equal<int list>([ 1; 2; 3; 4; 50; 60 ], collect s)

[<Fact>]
let ``map / filter / flatMap compose`` () =
    let s =
        Stream.range 1 6
        |> Stream.filter (fun n -> n % 2 = 0) // 2,4,6
        |> Stream.map (fun n -> n * n) // 4,16,36
        |> Stream.flatMap (fun n -> Stream.make [ n; -n ]) // 4,-4,16,-16,36,-36

    Assert.Equal<int list>([ 4; -4; 16; -16; 36; -36 ], collect s)

[<Fact>]
let ``take stops the producer early`` () =
    let produced = System.Collections.Generic.List<int>()

    let s =
        Stream.range 1 1000
        |> Stream.tap (fun n -> Effect.sync (fun () -> produced.Add n))
        |> Stream.take 3

    Assert.Equal<int list>([ 1; 2; 3 ], collect s)
    Assert.Equal(3, produced.Count) // producer stopped after 3, not 1000

[<Fact>]
let ``runForEach runs an effect per element`` () =
    let sum = ref 0

    let prog =
        Stream.range 1 5
        |> Stream.runForEach (fun n -> Effect.sync (fun () -> sum.Value <- sum.Value + n))

    Assert.Equal<Exit<unit, string>>(Exit.succeed (), Effect.runSync () prog)
    Assert.Equal(15, sum.Value)

[<Fact>]
let ``mapEffect threads effects through the stream`` () =
    let s = Stream.range 1 3 |> Stream.mapEffect (fun n -> Effect.succeed (n + 100))
    Assert.Equal<int list>([ 101; 102; 103 ], collect s)
