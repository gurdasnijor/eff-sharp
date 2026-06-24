module Effect.Tests.EquivalenceTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/Equivalence.test.ts

[<Fact>]
let ``String`` () =
    Assert.True(Equivalence.String "a" "a")
    Assert.False(Equivalence.String "a" "b")

[<Fact>]
let ``Number treats NaN as equal to itself`` () =
    Assert.True(Equivalence.Number 1.0 1.0)
    Assert.False(Equivalence.Number 1.0 2.0)
    Assert.True(Equivalence.Number nan nan)
    Assert.False(Equivalence.Number nan 1.0)
    Assert.False(Equivalence.Number 1.0 nan)
    Assert.True(Equivalence.Number 0.0 -0.0)

[<Fact>]
let ``Boolean`` () =
    Assert.True(Equivalence.Boolean true true)
    Assert.True(Equivalence.Boolean false false)
    Assert.False(Equivalence.Boolean true false)
    Assert.False(Equivalence.Boolean false true)

[<Fact>]
let ``BigInt`` () =
    Assert.True(Equivalence.BigInt 1I 1I)
    Assert.False(Equivalence.BigInt 1I 2I)

[<Fact>]
let ``mapInput derives equivalence from a field`` () =
    // Person = name * age; compare by name only.
    let eqPerson = Equivalence.strictEqual<string> () |> Equivalence.mapInput (fun (name, _age) -> name)
    Assert.True(eqPerson ("a", 1) ("a", 2))
    Assert.True(eqPerson ("a", 1) ("a", 1))
    Assert.False(eqPerson ("a", 1) ("b", 1))
    Assert.False(eqPerson ("a", 1) ("b", 2))

[<Fact>]
let ``combine ANDs two equivalences`` () =
    // T = string * int * bool; combine equivalence on fst and snd.
    let e0 = Equivalence.strictEqual<string> () |> Equivalence.mapInput (fun (a, _, _) -> a)
    let e1 = Equivalence.strictEqual<int> () |> Equivalence.mapInput (fun (_, b, _) -> b)
    let eq = Equivalence.combine e1 e0
    Assert.True(eq ("a", 1, true) ("a", 1, true))
    Assert.True(eq ("a", 1, true) ("a", 1, false))
    Assert.False(eq ("a", 1, true) ("b", 1, true))
    Assert.False(eq ("a", 1, true) ("a", 2, false))

[<Fact>]
let ``combineAll ANDs many equivalences`` () =
    let e0 = Equivalence.strictEqual<string> () |> Equivalence.mapInput (fun (a, _, _) -> a)
    let e1 = Equivalence.strictEqual<int> () |> Equivalence.mapInput (fun (_, b, _) -> b)
    let e2 = Equivalence.strictEqual<bool> () |> Equivalence.mapInput (fun (_, _, c) -> c)
    let eq = Equivalence.combineAll [ e0; e1; e2 ]
    Assert.True(eq ("a", 1, true) ("a", 1, true))
    Assert.False(eq ("a", 1, true) ("b", 1, true))
    Assert.False(eq ("a", 1, true) ("a", 2, true))
    Assert.False(eq ("a", 1, true) ("a", 1, false))

[<Fact>]
let ``combineAll empty is always true`` () =
    let eq = Equivalence.combineAll ([]: Equivalence<int> list)
    Assert.True(eq 1 2)

[<Fact>]
let ``tuple2`` () =
    let eq = Equivalence.tuple2 (Equivalence.strictEqual<string> ()) (Equivalence.strictEqual<int> ())
    Assert.True(eq ("a", 1) ("a", 1))
    Assert.False(eq ("a", 1) ("c", 1))
    Assert.False(eq ("a", 1) ("a", 2))

[<Fact>]
let ``Array compares element-wise with length`` () =
    let eq = Equivalence.Array (Equivalence.strictEqual<int> ())
    Assert.True(eq [] [])
    Assert.True(eq [ 1; 2; 3 ] [ 1; 2; 3 ])
    Assert.False(eq [ 1; 2; 3 ] [ 1; 2; 4 ])
    Assert.False(eq [ 1; 2; 3 ] [ 1; 2 ])

[<Fact>]
let ``record compares same key set and values`` () =
    let eq = Equivalence.record (Equivalence.strictEqual<int> ())
    let m1 = Map [ "a", 1; "b", 2 ]
    Assert.True(eq m1 (Map [ "a", 1; "b", 2 ]))
    Assert.False(eq m1 (Map [ "a", 1; "b", 3 ]))
    Assert.False(eq m1 (Map [ "a", 1 ]))
    Assert.False(eq (Map [ "a", 1 ]) m1)

[<Fact>]
let ``Date compares by timestamp`` () =
    let d1 = System.DateTime(2020, 1, 1, 0, 0, 0, System.DateTimeKind.Utc)
    let d2 = System.DateTime(2020, 1, 1, 0, 0, 0, System.DateTimeKind.Utc)
    let d3 = System.DateTime(2021, 1, 1, 0, 0, 0, System.DateTimeKind.Utc)
    Assert.True(Equivalence.Date d1 d2)
    Assert.False(Equivalence.Date d1 d3)
    Assert.True(Equivalence.Date d1 d1)
