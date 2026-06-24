module Effect.Tests.ReducerTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/Reducer.test.ts
// Upstream uses `String.ReducerConcat` (String module not ported); a string
// concat reducer is built inline via `Reducer.make` instead.

let private Concat = Reducer.make (fun (a: string) b -> a + b) ""

[<Fact>]
let ``flip`` () =
    let R = Reducer.flip Concat
    Assert.Equal("ba", R.combine "a" "b")
    Assert.Equal("", R.initialValue)
    Assert.Equal("cba", R.combineAll [ "a"; "b"; "c" ])

[<Fact>]
let ``combineAll uses default left-to-right fold`` () =
    Assert.Equal("abc", Concat.combineAll [ "a"; "b"; "c" ])
    Assert.Equal("", Concat.combineAll [])

[<Fact>]
let ``makeWith uses the custom combineAll`` () =
    // Product reducer that short-circuits on the absorbing element 0.
    let Product =
        Reducer.makeWith (*) 1 (fun collection ->
            let mutable acc = 1
            let mutable hitZero = false
            for n in collection do
                if n = 0 then hitZero <- true
                acc <- acc * n
            if hitZero then 0 else acc)
    Assert.Equal(24, Product.combineAll [ 2; 3; 4 ])
    Assert.Equal(0, Product.combineAll [ 2; 0; 4 ])
    Assert.Equal(6, Product.combine 2 3)

[<Fact>]
let ``toCombiner exposes combine`` () =
    let C = Reducer.toCombiner Concat
    Assert.Equal("ab", C.combine "a" "b")
