module Effect.Tests.BigIntTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/BigInt.test.ts
// (extended with coverage for the math/predicate surface the upstream test omits).

// --- upstream test cases ---

[<Fact>]
let ``Equivalence compares bigints`` () =
    Assert.True(BigInt.Equivalence 1I 1I)
    Assert.False(BigInt.Equivalence 1I 2I)

[<Fact>]
let ``divide returns Some for non-zero divisors, None for zero`` () =
    Assert.Equal(Some 2I, BigInt.divide 6I 3I)
    Assert.Equal(None, BigInt.divide 6I 0I)
    Assert.Equal(2I, BigInt.divideUnsafe 6I 3I)
    Assert.Equal(1I, BigInt.divideUnsafe 6I 4I) // truncates toward zero

[<Fact>]
let ``sqrt returns integer square roots`` () =
    Assert.Equal(Some 2I, BigInt.sqrt 4I)
    Assert.Equal(Some 3I, BigInt.sqrt 9I)
    Assert.Equal(Some 4I, BigInt.sqrt 16I)
    Assert.Equal(None, BigInt.sqrt -1I)

[<Fact>]
let ``toNumber converts within the safe integer range`` () =
    Assert.Equal(Some 42.0, BigInt.toNumber 42I)
    Assert.Equal(None, BigInt.toNumber (9007199254740991I + 1I))

[<Fact>]
let ``fromString parses valid integer strings only`` () =
    Assert.Equal(Some 42I, BigInt.fromString "42")
    Assert.Equal(None, BigInt.fromString " ")
    Assert.Equal(None, BigInt.fromString "a")

[<Fact>]
let ``fromNumber converts safe integers only`` () =
    Assert.Equal(Some 42I, BigInt.fromNumber 42.0)
    Assert.Equal(None, BigInt.fromNumber (9007199254740991.0 + 1.0))
    Assert.Equal(None, BigInt.fromNumber 1.5) // non-integral

[<Fact>]
let ``ReducerSum combines with zero as identity`` () =
    Assert.Equal(3I, BigInt.ReducerSum.combine 1I 2I)
    Assert.Equal(2I, BigInt.ReducerSum.combine BigInt.ReducerSum.initialValue 2I)
    Assert.Equal(2I, BigInt.ReducerSum.combine 2I BigInt.ReducerSum.initialValue)

[<Fact>]
let ``ReducerMultiply combines with one as identity`` () =
    Assert.Equal(6I, BigInt.ReducerMultiply.combine 2I 3I)
    Assert.Equal(2I, BigInt.ReducerMultiply.combine BigInt.ReducerMultiply.initialValue 2I)
    Assert.Equal(2I, BigInt.ReducerMultiply.combine 2I BigInt.ReducerMultiply.initialValue)
    Assert.Equal(0I, BigInt.ReducerMultiply.combineAll [ 2I; 0I; 3I ]) // short-circuits on zero

[<Fact>]
let ``CombinerMax and CombinerMin pick extremes`` () =
    Assert.Equal(2I, BigInt.CombinerMax.combine 1I 2I)
    Assert.Equal(1I, BigInt.CombinerMin.combine 1I 2I)

// --- extended coverage ---

[<Fact>]
let ``basic arithmetic`` () =
    Assert.Equal(5I, BigInt.sum 2I 3I)
    Assert.Equal(6I, BigInt.multiply 2I 3I)
    Assert.Equal(-1I, BigInt.subtract 2I 3I)
    Assert.Equal(1I, BigInt.remainder 10I 3I)
    Assert.Equal(3I, BigInt.increment 2I)
    Assert.Equal(2I, BigInt.decrement 3I)

[<Fact>]
let ``sign and abs`` () =
    Assert.Equal(-1, BigInt.sign -5I)
    Assert.Equal(0, BigInt.sign 0I)
    Assert.Equal(1, BigInt.sign 5I)
    Assert.Equal(5I, BigInt.abs -5I)
    Assert.Equal(5I, BigInt.abs 5I)

[<Fact>]
let ``gcd and lcm`` () =
    Assert.Equal(1I, BigInt.gcd 2I 3I)
    Assert.Equal(2I, BigInt.gcd 2I 4I)
    Assert.Equal(8I, BigInt.gcd 16I 24I)
    Assert.Equal(6I, BigInt.lcm 2I 3I)
    Assert.Equal(48I, BigInt.lcm 16I 24I)

[<Fact>]
let ``sqrtUnsafe throws on negative`` () =
    Assert.Equal(4I, BigInt.sqrtUnsafe 16I)
    Assert.Throws<System.ArgumentException>(fun () -> BigInt.sqrtUnsafe -1I |> ignore) |> ignore

[<Fact>]
let ``sumAll and multiplyAll aggregate`` () =
    Assert.Equal(9I, BigInt.sumAll [ 2I; 3I; 4I ])
    Assert.Equal(0I, BigInt.sumAll [])
    Assert.Equal(24I, BigInt.multiplyAll [ 2I; 3I; 4I ])
    Assert.Equal(1I, BigInt.multiplyAll [])
    Assert.Equal(0I, BigInt.multiplyAll [ 2I; 0I; 3I ])

[<Fact>]
let ``comparison predicates`` () =
    Assert.True(BigInt.isLessThan 2I 3I)
    Assert.False(BigInt.isLessThan 3I 3I)
    Assert.True(BigInt.isLessThanOrEqualTo 3I 3I)
    Assert.True(BigInt.isGreaterThan 4I 3I)
    Assert.True(BigInt.isGreaterThanOrEqualTo 3I 3I)

[<Fact>]
let ``between clamp min max`` () =
    Assert.True(BigInt.between 0I 5I 3I)
    Assert.False(BigInt.between 0I 5I 6I)
    Assert.Equal(3I, BigInt.clamp 1I 5I 3I)
    Assert.Equal(1I, BigInt.clamp 1I 5I 0I)
    Assert.Equal(5I, BigInt.clamp 1I 5I 6I)
    Assert.Equal(2I, BigInt.min 2I 3I)
    Assert.Equal(3I, BigInt.max 2I 3I)
