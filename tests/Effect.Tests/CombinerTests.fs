module Effect.Tests.CombinerTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/Combiner.test.ts
// Upstream uses `String.ReducerConcat` (String module not ported); a string
// concat combiner is built inline via `Combiner.make` instead.

let private Concat = Combiner.make (fun (a: string) b -> a + b)

[<Fact>]
let ``flip`` () =
    let C = Combiner.flip Concat
    Assert.Equal("ba", C.combine "a" "b")

[<Fact>]
let ``min`` () =
    let C = Combiner.min Order.Number
    Assert.Equal(1.0, C.combine 1.0 2.0)
    Assert.Equal(1.0, C.combine 2.0 1.0)

[<Fact>]
let ``max`` () =
    let C = Combiner.max Order.Number
    Assert.Equal(2.0, C.combine 1.0 2.0)
    Assert.Equal(2.0, C.combine 2.0 1.0)

[<Fact>]
let ``first`` () =
    let C = Combiner.first<int> ()
    Assert.Equal(1, C.combine 1 2)

[<Fact>]
let ``last`` () =
    let C = Combiner.last<int> ()
    Assert.Equal(2, C.combine 1 2)

[<Fact>]
let ``constant`` () =
    let C = Combiner.constant 0
    Assert.Equal(0, C.combine 1 2)

[<Fact>]
let ``intercalate`` () =
    let C = Combiner.intercalate "+" Concat
    Assert.Equal("a+b", C.combine "a" "b")
