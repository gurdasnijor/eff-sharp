module ReducerSpec

open Effect
open Effect.Vitest

let private Concat = Reducer.make (fun (a: string) b -> a + b) ""

describe "Reducer" (fun () ->
    test "flip" (fun () ->
        let R = Reducer.flip Concat
        toBe (R.combine "a" "b") "ba"
        toBe R.initialValue ""
        toBe (R.combineAll [ "a"; "b"; "c" ]) "cba")

    test "combineAll uses default left-to-right fold" (fun () ->
        toBe (Concat.combineAll [ "a"; "b"; "c" ]) "abc"
        toBe (Concat.combineAll []) "")

    test "makeWith uses the custom combineAll" (fun () ->
        let Product =
            Reducer.makeWith (*) 1 (fun collection ->
                let mutable acc = 1
                let mutable hitZero = false

                for n in collection do
                    if n = 0 then
                        hitZero <- true

                    acc <- acc * n

                if hitZero then 0 else acc)

        toBe (Product.combineAll [ 2; 3; 4 ]) 24
        toBe (Product.combineAll [ 2; 0; 4 ]) 0
        toBe (Product.combine 2 3) 6)

    test "toCombiner exposes combine" (fun () ->
        let C = Reducer.toCombiner Concat
        toBe (C.combine "a" "b") "ab"))

