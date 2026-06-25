module BigIntSpec

open Effect
open Effect.Vitest

describe "BigInt" (fun () ->
    test "Equivalence compares bigints" (fun () ->
        toBe (BigInt.Equivalence 1I 1I) true
        toBe (BigInt.Equivalence 1I 2I) false)

    test "divide returns Some for non-zero divisors, None for zero" (fun () ->
        toEqual (BigInt.divide 6I 3I) (Some 2I)
        toEqual (BigInt.divide 6I 0I) None
        toEqual (BigInt.divideUnsafe 6I 3I) 2I
        toEqual (BigInt.divideUnsafe 6I 4I) 1I)

    test "sqrt returns integer square roots" (fun () ->
        toEqual (BigInt.sqrt 4I) (Some 2I)
        toEqual (BigInt.sqrt 9I) (Some 3I)
        toEqual (BigInt.sqrt 16I) (Some 4I)
        toEqual (BigInt.sqrt -1I) None)

    test "toNumber converts within the safe integer range" (fun () ->
        toEqual (BigInt.toNumber 42I) (Some 42.0)
        toEqual (BigInt.toNumber (9007199254740991I + 1I)) None)

    test "fromString parses valid integer strings only" (fun () ->
        toEqual (BigInt.fromString "42") (Some 42I)
        toEqual (BigInt.fromString " ") None
        toEqual (BigInt.fromString "a") None)

    test "fromNumber converts safe integers only" (fun () ->
        toEqual (BigInt.fromNumber 42.0) (Some 42I)
        toEqual (BigInt.fromNumber (9007199254740991.0 + 1.0)) None
        toEqual (BigInt.fromNumber 1.5) None)

    test "reducers and combiners" (fun () ->
        toEqual (BigInt.ReducerSum.combine 1I 2I) 3I
        toEqual (BigInt.ReducerSum.combine BigInt.ReducerSum.initialValue 2I) 2I
        toEqual (BigInt.ReducerMultiply.combine 2I 3I) 6I
        toEqual (BigInt.ReducerMultiply.combineAll [ 2I; 0I; 3I ]) 0I
        toEqual (BigInt.CombinerMax.combine 1I 2I) 2I
        toEqual (BigInt.CombinerMin.combine 1I 2I) 1I)

    test "basic arithmetic" (fun () ->
        toEqual (BigInt.sum 2I 3I) 5I
        toEqual (BigInt.multiply 2I 3I) 6I
        toEqual (BigInt.subtract 2I 3I) -1I
        toEqual (BigInt.remainder 10I 3I) 1I
        toEqual (BigInt.increment 2I) 3I
        toEqual (BigInt.decrement 3I) 2I)

    test "sign, abs, gcd, lcm" (fun () ->
        toBe (BigInt.sign -5I) -1
        toBe (BigInt.sign 0I) 0
        toBe (BigInt.sign 5I) 1
        toEqual (BigInt.abs -5I) 5I
        toEqual (BigInt.gcd 16I 24I) 8I
        toEqual (BigInt.lcm 16I 24I) 48I)

    test "sqrtUnsafe throws on negative" (fun () ->
        toEqual (BigInt.sqrtUnsafe 16I) 4I
        toThrow (fun () -> BigInt.sqrtUnsafe -1I |> ignore))

    test "aggregate helpers" (fun () ->
        toEqual (BigInt.sumAll [ 2I; 3I; 4I ]) 9I
        toEqual (BigInt.sumAll []) 0I
        toEqual (BigInt.multiplyAll [ 2I; 3I; 4I ]) 24I
        toEqual (BigInt.multiplyAll []) 1I
        toEqual (BigInt.multiplyAll [ 2I; 0I; 3I ]) 0I)

    test "comparison predicates and bounds" (fun () ->
        toBe (BigInt.isLessThan 2I 3I) true
        toBe (BigInt.isLessThan 3I 3I) false
        toBe (BigInt.isLessThanOrEqualTo 3I 3I) true
        toBe (BigInt.isGreaterThan 4I 3I) true
        toBe (BigInt.isGreaterThanOrEqualTo 3I 3I) true
        toBe (BigInt.between 0I 5I 3I) true
        toBe (BigInt.between 0I 5I 6I) false
        toEqual (BigInt.clamp 1I 5I 0I) 1I
        toEqual (BigInt.clamp 1I 5I 6I) 5I
        toEqual (BigInt.min 2I 3I) 2I
        toEqual (BigInt.max 2I 3I) 3I))

