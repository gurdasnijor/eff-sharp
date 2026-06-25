module FilterSpec

open Effect
open Effect.Vitest

describe "Filter" (fun () ->
    test "fromPredicate passes or fails with the input" (fun () ->
        let positive = Filter.fromPredicate (fun n -> n > 0)
        toEqual (positive 5) (Ok 5)
        toEqual (positive -3) (Error -3))

    test "fromPredicateOption passes the Some value, fails with the input" (fun () ->
        let firstChar =
            Filter.fromPredicateOption (fun (s: string) -> if s.Length > 0 then Some s.[0] else None)

        toEqual (firstChar "hi") (Ok 'h')
        toEqual (firstChar "") (Error ""))

    test "equals uses structural equality" (fun () ->
        let isOne = Filter.equals 1
        toEqual (isOne 1) (Ok 1)
        toEqual (isOne 2) (Error 2))

    test "string and boolean narrow obj inputs" (fun () ->
        toEqual (Filter.string (box "hello")) (Ok "hello")
        toEqual (Filter.string (box 42)) (Error(box 42))
        toEqual (Filter.boolean (box true)) (Ok true)
        toEqual (Filter.boolean (box "x")) (Error(box "x")))

    test "tryCatch passes the result or fails with the input on throw" (fun () ->
        let parse = Filter.tryCatch (fun (s: string) -> int s)
        toEqual (parse "42") (Ok 42)
        toEqual (parse "nope") (Error "nope"))

    test "mapFail transforms only the failure value" (fun () ->
        let f =
            Filter.fromPredicate (fun n -> n > 0)
            |> Filter.mapFail (fun n -> sprintf "bad:%d" n)

        toEqual (f 5) (Ok 5)
        toEqual (f -1) (Error "bad:-1"))

    test "orElse falls back to the right filter" (fun () ->
        let f =
            Filter.fromPredicate (fun n -> n > 10)
            |> Filter.orElse (Filter.fromPredicate (fun n -> n < 0))

        toEqual (f 20) (Ok 20)
        toEqual (f -5) (Ok -5)
        toEqual (f 3) (Error 3))

    test "zip and zipWith require both to pass" (fun () ->
        let positive = Filter.fromPredicate (fun n -> n > 0)
        let even = Filter.fromPredicate (fun n -> n % 2 = 0)
        let zipped = positive |> Filter.zip even
        toEqual (zipped 4) (Ok(4, 4))
        toEqual (zipped 3) (Error 3)
        toEqual (zipped -2) (Error -2)
        let summed = positive |> Filter.zipWith even (fun a b -> a + b)
        toEqual (summed 4) (Ok 8))

    test "andLeft and andRight keep one side" (fun () ->
        let positive = Filter.fromPredicate (fun n -> n > 0)
        let lessThan10 = Filter.make (fun n -> if n < 10 then Ok(n * 2) else Error n)
        toEqual ((positive |> Filter.andLeft lessThan10) 4) (Ok 4)
        toEqual ((positive |> Filter.andRight lessThan10) 4) (Ok 8))

    test "compose feeds the left pass into the right" (fun () ->
        let positive = Filter.fromPredicate (fun n -> n > 0)
        let double = Filter.make (fun n -> if n < 100 then Ok(n * 2) else Error n)
        let f = positive |> Filter.compose double
        toEqual (f 5) (Ok 10)
        toEqual (f -1) (Error -1)
        toEqual (f 200) (Error 200))

    test "composePassthrough fails with the original input" (fun () ->
        let nonEmpty = Filter.fromPredicate (fun (s: string) -> s.Length > 2)
        let f = Filter.string |> Filter.composePassthrough nonEmpty
        toEqual (f (box "hello")) (Ok "hello")
        toEqual (f (box "hi")) (Error(box "hi"))
        toEqual (f (box 5)) (Error(box 5)))

    test "toPredicate, toOption, toResult convert a filter" (fun () ->
        let positive = Filter.fromPredicate (fun n -> n > 0)
        toBe (Filter.toPredicate positive 5) true
        toBe (Filter.toPredicate positive -1) false
        toEqual (Filter.toOption positive 5) (Some 5)
        toEqual (Filter.toOption positive -1) None
        toEqual (Filter.toResult positive 5) (Ok 5)
        toEqual (Filter.toResult positive -1) (Error -1)))

