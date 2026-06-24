module Effect.Tests.OrderTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/Order.test.ts

[<Fact>]
let ``Struct (via combine of mapInputs)`` () =
    // Upstream Order.Struct({a,b}); idiomatic F# is combine of field orders.
    let byA = Order.String |> Order.mapInput (fun (a, _b) -> a)
    let byB = Order.String |> Order.mapInput (fun (_a, b) -> b)
    let o = Order.combine byB byA
    Assert.Equal(-1, o ("a", "b") ("a", "c"))
    Assert.Equal(0, o ("a", "b") ("a", "b"))
    Assert.Equal(1, o ("a", "c") ("a", "b"))

[<Fact>]
let ``tuple2`` () =
    let o = Order.tuple2 Order.String Order.String
    Assert.Equal(-1, o ("a", "b") ("a", "c"))
    Assert.Equal(0, o ("a", "b") ("a", "b"))
    Assert.Equal(1, o ("a", "b") ("a", "a"))
    Assert.Equal(-1, o ("a", "b") ("b", "a"))

[<Fact>]
let ``mapInput`` () =
    let o = Order.Number |> Order.mapInput (fun (s: string) -> float s.Length)
    Assert.Equal(0, o "a" "b")
    Assert.Equal(-1, o "a" "bb")
    Assert.Equal(1, o "aa" "b")

[<Fact>]
let ``Number compares and treats NaN as lowest`` () =
    let o = Order.Number
    Assert.Equal(0, o 1.0 1.0)
    Assert.Equal(-1, o 1.0 2.0)
    Assert.Equal(1, o 2.0 1.0)
    Assert.Equal(0, o 0.0 -0.0)
    Assert.Equal(0, o nan nan)
    Assert.Equal(-1, o nan 1.0)
    Assert.Equal(1, o 1.0 nan)

[<Fact>]
let ``Date compares by timestamp`` () =
    let o = Order.Date
    let epoch = System.DateTime(2020, 1, 1)
    Assert.Equal(-1, o epoch (epoch.AddTicks 1L))
    Assert.Equal(0, o epoch epoch)
    Assert.Equal(1, o (epoch.AddTicks 1L) epoch)

[<Fact>]
let ``clamp keeps values inside inclusive bounds`` () =
    let clamp x = Order.clamp Order.Number 1.0 10.0 x
    Assert.Equal(2.0, clamp 2.0)
    Assert.Equal(10.0, clamp 10.0)
    Assert.Equal(10.0, clamp 20.0)
    Assert.Equal(1.0, clamp 1.0)
    Assert.Equal(1.0, clamp -10.0)

[<Fact>]
let ``isBetween checks inclusive bounds`` () =
    let between x = Order.isBetween Order.Number 1.0 10.0 x
    Assert.True(between 2.0)
    Assert.True(between 10.0)
    Assert.False(between 20.0)
    Assert.True(between 1.0)
    Assert.False(between -10.0)

[<Fact>]
let ``flip reverses an existing order`` () =
    let o = Order.flip Order.Number
    Assert.Equal(1, o 1.0 2.0)
    Assert.Equal(-1, o 2.0 1.0)
    Assert.Equal(0, o 2.0 2.0)

[<Fact>]
let ``isLessThan is strict`` () =
    let lt a b = Order.isLessThan Order.Number a b
    Assert.True(lt 0.0 1.0)
    Assert.False(lt 1.0 1.0)
    Assert.False(lt 2.0 1.0)

[<Fact>]
let ``isLessThanOrEqualTo`` () =
    let lte a b = Order.isLessThanOrEqualTo Order.Number a b
    Assert.True(lte 0.0 1.0)
    Assert.True(lte 1.0 1.0)
    Assert.False(lte 2.0 1.0)

[<Fact>]
let ``isGreaterThan is strict`` () =
    let gt a b = Order.isGreaterThan Order.Number a b
    Assert.False(gt 0.0 1.0)
    Assert.False(gt 1.0 1.0)
    Assert.True(gt 2.0 1.0)

[<Fact>]
let ``isGreaterThanOrEqualTo`` () =
    let gte a b = Order.isGreaterThanOrEqualTo Order.Number a b
    Assert.False(gte 0.0 1.0)
    Assert.True(gte 1.0 1.0)
    Assert.True(gte 2.0 1.0)

[<Fact>]
let ``min returns the first when equal`` () =
    let o = Order.Number |> Order.mapInput (fun (r: int ref) -> float r.Value)
    let mn a b = Order.min o a b
    let first = ref 1
    let second = ref 1
    Assert.Equal(ref 1, mn (ref 1) (ref 2))
    Assert.Equal(ref 1, mn (ref 2) (ref 1))
    Assert.Same(first, mn first second)

[<Fact>]
let ``max returns the first when equal`` () =
    let o = Order.Number |> Order.mapInput (fun (r: int ref) -> float r.Value)
    let mx a b = Order.max o a b
    let first = ref 1
    let second = ref 1
    Assert.Equal(ref 2, mx (ref 1) (ref 2))
    Assert.Equal(ref 2, mx (ref 2) (ref 1))
    Assert.Same(first, mx first second)

[<Fact>]
let ``combine sorts by first then second`` () =
    let tuples = [ (2, "c"); (1, "b"); (2, "a"); (1, "c") ]
    let byFst = Order.Number |> Order.mapInput (fun (x, _) -> float x)
    let bySnd = Order.String |> Order.mapInput (fun (_, y) -> y)
    let sorted = tuples |> List.sortWith (Order.combine bySnd byFst)
    Assert.Equal<(int * string) list>([ (1, "b"); (1, "c"); (2, "a"); (2, "c") ], sorted)
    let sorted2 = tuples |> List.sortWith (Order.combine byFst bySnd)
    Assert.Equal<(int * string) list>([ (2, "a"); (1, "b"); (1, "c"); (2, "c") ], sorted2)

[<Fact>]
let ``Array orders lexicographically then by length`` () =
    let o = Order.Array Order.Number
    Assert.Equal(-1, o [ 1.0; 2.0 ] [ 1.0; 3.0 ])
    Assert.Equal(-1, o [ 1.0; 2.0 ] [ 1.0; 2.0; 3.0 ])
    Assert.Equal(1, o [ 1.0; 2.0; 3.0 ] [ 1.0; 2.0 ])
    Assert.Equal(0, o [ 1.0; 2.0 ] [ 1.0; 2.0 ])
