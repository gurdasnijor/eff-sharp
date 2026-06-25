module Effect.Tests.FilterTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/src/Filter.ts (no upstream test
// file exists; these Facts assert the documented behavior of each export).
// Ok = upstream Result.succeed (pass); Error = Result.fail (filtered out).

[<Fact>]
let ``fromPredicate passes or fails with the input`` () =
    let positive = Filter.fromPredicate (fun n -> n > 0)
    Assert.Equal(Ok 5, positive 5)
    Assert.Equal(Error -3, positive -3)

[<Fact>]
let ``fromPredicateOption passes the Some value, fails with the input`` () =
    let firstChar =
        Filter.fromPredicateOption (fun (s: string) -> if s.Length > 0 then Some s.[0] else None)

    Assert.Equal(Ok 'h', firstChar "hi")
    Assert.Equal(Error "", firstChar "")

[<Fact>]
let ``equals uses structural equality`` () =
    let isOne = Filter.equals 1
    Assert.Equal(Ok 1, isOne 1)
    Assert.Equal(Error 2, isOne 2)

[<Fact>]
let ``string and boolean narrow obj inputs`` () =
    Assert.Equal(Ok "hello", Filter.string (box "hello"))
    Assert.Equal(Error(box 42), Filter.string (box 42))
    Assert.Equal(Ok true, Filter.boolean (box true))
    Assert.Equal(Error(box "x"), Filter.boolean (box "x"))

[<Fact>]
let ``tryCatch passes the result or fails with the input on throw`` () =
    let parse = Filter.tryCatch (fun (s: string) -> int s)
    Assert.Equal(Ok 42, parse "42")
    Assert.Equal(Error "nope", parse "nope")

[<Fact>]
let ``mapFail transforms only the failure value`` () =
    let f =
        Filter.fromPredicate (fun n -> n > 0)
        |> Filter.mapFail (fun n -> sprintf "bad:%d" n)

    Assert.Equal(Ok 5, f 5)
    Assert.Equal(Error "bad:-1", f -1)

[<Fact>]
let ``orElse falls back to the right filter`` () =
    let f =
        Filter.fromPredicate (fun n -> n > 10)
        |> Filter.orElse (Filter.fromPredicate (fun n -> n < 0))

    Assert.Equal(Ok 20, f 20)
    Assert.Equal(Ok -5, f -5)
    Assert.Equal(Error 3, f 3)

[<Fact>]
let ``zip and zipWith require both to pass`` () =
    let positive = Filter.fromPredicate (fun n -> n > 0)
    let even = Filter.fromPredicate (fun n -> n % 2 = 0)
    let zipped = positive |> Filter.zip even
    Assert.Equal(Ok(4, 4), zipped 4)
    Assert.Equal(Error 3, zipped 3)
    Assert.Equal(Error -2, zipped -2)
    let summed = positive |> Filter.zipWith even (fun a b -> a + b)
    Assert.Equal(Ok 8, summed 4)

[<Fact>]
let ``andLeft and andRight keep one side`` () =
    let positive = Filter.fromPredicate (fun n -> n > 0)
    let lessThan10 = Filter.make (fun n -> if n < 10 then Ok(n * 2) else Error n)
    Assert.Equal(Ok 4, (positive |> Filter.andLeft lessThan10) 4)
    Assert.Equal(Ok 8, (positive |> Filter.andRight lessThan10) 4)

[<Fact>]
let ``compose feeds the left pass into the right`` () =
    let positive = Filter.fromPredicate (fun n -> n > 0)
    let double = Filter.make (fun n -> if n < 100 then Ok(n * 2) else Error n)
    let f = positive |> Filter.compose double
    Assert.Equal(Ok 10, f 5)
    Assert.Equal(Error -1, f -1)
    Assert.Equal(Error 200, f 200)

[<Fact>]
let ``composePassthrough fails with the original input`` () =
    let nonEmpty = Filter.fromPredicate (fun (s: string) -> s.Length > 2)
    let f = Filter.string |> Filter.composePassthrough nonEmpty
    Assert.Equal(Ok "hello", f (box "hello"))
    Assert.Equal(Error(box "hi"), f (box "hi")) // right fails -> original input
    Assert.Equal(Error(box 5), f (box 5)) // left fails -> original input

[<Fact>]
let ``toPredicate, toOption, toResult convert a filter`` () =
    let positive = Filter.fromPredicate (fun n -> n > 0)
    Assert.True(Filter.toPredicate positive 5)
    Assert.False(Filter.toPredicate positive -1)
    Assert.Equal(Some 5, Filter.toOption positive 5)
    Assert.Equal(None, Filter.toOption positive -1)
    Assert.Equal(Ok 5, Filter.toResult positive 5)
    Assert.Equal(Error -1, Filter.toResult positive -1)
