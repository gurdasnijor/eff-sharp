module NonEmptyIterableSpec

open Effect
open Effect.Vitest

describe "NonEmptyIterable" (fun () ->
    test "unprepend splits head and rest" (fun () ->
        let first, rest = NonEmptyIterable.unprepend [ 1; 2; 3; 4; 5 ]
        toBe first 1
        toEqual (List.ofSeq rest) [ 2; 3; 4; 5 ])

    test "unprepend on a single element yields empty rest" (fun () ->
        let first, rest = NonEmptyIterable.unprepend [ "h" ]
        toBe first "h"
        toEqual (List.ofSeq rest) [])

    test "unprepend works over any seq" (fun () ->
        let first, rest =
            NonEmptyIterable.unprepend (
                seq {
                    10
                    20
                    30
                }
            )

        toBe first 10
        toEqual (List.ofSeq rest) [ 20; 30 ])

    test "unprepend throws on empty" (fun () ->
        toThrow (fun () -> NonEmptyIterable.unprepend (Seq.empty<int>) |> ignore)))

