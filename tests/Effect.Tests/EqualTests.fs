module Effect.Tests.EqualTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/Equal.test.ts.
// The JS-object-machinery cases (symbol keys, non-enumerable props, proxies,
// circular refs, byReference) are dropped per the porting decoupling rule;
// these Facts cover the structural-equality behaviour using idiomatic F# values.

type private Point = { X: int; Y: int }

[<Fact>]
let ``structurally identical records are equal`` () =
    Assert.True(Equal.equals { X = 1; Y = 2 } { X = 1; Y = 2 })

[<Fact>]
let ``records with different values are not equal`` () =
    Assert.False(Equal.equals { X = 1; Y = 2 } { X = 1; Y = 3 })

[<Fact>]
let ``primitives compare by value`` () =
    Assert.True(Equal.equals 1 1)
    Assert.False(Equal.equals "a" "b")
    Assert.True(Equal.equals "a" "a")

[<Fact>]
let ``NaN equals NaN`` () =
    Assert.True(Equal.equals nan nan)
    Assert.False(Equal.equals nan 0.0)
    Assert.False(Equal.equals 42.0 nan)
    Assert.False(Equal.equals nan System.Double.PositiveInfinity)

[<Fact>]
let ``NaN inside arrays and records`` () =
    Assert.True(Equal.equals [| nan |] [| nan |])
    Assert.True(Equal.equals [| nan; 1.0 |] [| nan; 1.0 |])
    Assert.False(Equal.equals [| nan; 1.0 |] [| nan; 2.0 |])
    Assert.False(Equal.equals [| nan |] [| System.Double.PositiveInfinity |])
    Assert.True(Equal.equals {| arr = [ nan ] |} {| arr = [ nan ] |})
    Assert.True(Equal.equals {| nested = {| deep = nan |} |} {| nested = {| deep = nan |} |})

[<Fact>]
let ``lists and tuples compare structurally`` () =
    Assert.True(Equal.equals [ 1; 2; 3 ] [ 1; 2; 3 ])
    Assert.False(Equal.equals [ 1; 2; 3 ] [ 1; 2; 4 ])
    Assert.True(Equal.equals (1, "a") (1, "a"))
    Assert.False(Equal.equals (1, "a") (1, "b"))

[<Fact>]
let ``options compare structurally`` () =
    Assert.True(Equal.equals (Some 42) (Some 42))
    Assert.False(Equal.equals (Some 42) (Some 43))
    Assert.False(Equal.equals (Some 42) None)

[<Fact>]
let ``maps compare order-independently`` () =
    let m1 = Map [ "a", 1; "b", 2 ]
    let m2 = Map [ "b", 2; "a", 1 ]
    Assert.True(Equal.equals m1 m2)
    Assert.False(Equal.equals m1 (Map [ "a", 1; "b", 3 ]))
    Assert.True(Equal.equals (Map<string, int> []) (Map<string, int> []))

[<Fact>]
let ``sets compare order-independently`` () =
    Assert.True(Equal.equals (Set [ 1; 2; 3 ]) (Set [ 3; 2; 1 ]))
    Assert.False(Equal.equals (Set [ 1; 2; 3 ]) (Set [ 1; 2 ]))

[<Fact>]
let ``dates compare by value`` () =
    let d1 = System.DateTime(2023, 1, 1)
    let d2 = System.DateTime(2023, 1, 1)
    let d3 = System.DateTime(2023, 1, 2)
    Assert.True(Equal.equals d1 d2)
    Assert.False(Equal.equals d1 d3)

[<Fact>]
let ``asEquivalence wraps equals`` () =
    let eq = Equal.asEquivalence<Point> ()
    Assert.True(eq { X = 1; Y = 2 } { X = 1; Y = 2 })
    Assert.False(eq { X = 1; Y = 2 } { X = 2; Y = 1 })
