module CombinerSpec

open Effect
open Effect.Vitest

let private Concat = Combiner.make (fun (a: string) b -> a + b)

describe "Combiner" (fun () ->
    test "flip" (fun () ->
        let C = Combiner.flip Concat
        toBe (C.combine "a" "b") "ba")

    test "min" (fun () ->
        let C = Combiner.min Order.Number
        toBe (C.combine 1.0 2.0) 1.0
        toBe (C.combine 2.0 1.0) 1.0)

    test "max" (fun () ->
        let C = Combiner.max Order.Number
        toBe (C.combine 1.0 2.0) 2.0
        toBe (C.combine 2.0 1.0) 2.0)

    test "first" (fun () ->
        let C = Combiner.first<int> ()
        toBe (C.combine 1 2) 1)

    test "last" (fun () ->
        let C = Combiner.last<int> ()
        toBe (C.combine 1 2) 2)

    test "constant" (fun () ->
        let C = Combiner.constant 0
        toBe (C.combine 1 2) 0)

    test "intercalate" (fun () ->
        let C = Combiner.intercalate "+" Concat
        toBe (C.combine "a" "b") "a+b"))

