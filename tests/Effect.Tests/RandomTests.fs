module Effect.Tests.RandomTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/Random.test.ts.
// `it.effect` cases become xUnit Facts; effects run via `Effect.runSync`.
// Determinism/distinctness tests mirror upstream (exact ISAAC outputs are not
// asserted upstream — those come from mock services, which we reproduce).

/// Run a Context-effect that cannot fail, returning its success value.
let private run (ctx: Context) (eff: Effect<'A, 'E, Context>) : 'A =
    match Effect.runSync ctx eff with
    | Success a -> a
    | Failure c -> failwithf "unexpected failure: %s" (Cause.render c)

/// A mock `Random` whose `NextDoubleUnsafe` walks a fixed list (then 0.0).
let private mockDoubles (values: float list) : Context =
    let arr = List.toArray values
    let mutable i = 0

    let nextDouble () =
        let v = if i < arr.Length then arr.[i] else 0.0
        i <- i + 1
        v

    Context.make Random.tag (Random.ofDouble nextDouble)

let private empty = Context.empty

[<Fact>]
let ``next generates a number between 0 inclusive and 1 exclusive`` () =
    let value = run empty Random.next
    Assert.True(value >= 0.0 && value < 1.0)

[<Fact>]
let ``nextBoolean generates a boolean`` () =
    let value = run empty Random.nextBoolean
    Assert.True(value = true || value = false)

[<Fact>]
let ``nextBoolean is deterministic with the same seed`` () =
    let program =
        effect {
            let! a = Random.nextBoolean
            let! b = Random.nextBoolean
            let! c = Random.nextBoolean
            return [ a; b; c ]
        }

    let r1 = run empty (Random.withSeed (SeedString "next-boolean-seed") program)
    let r2 = run empty (Random.withSeed (SeedString "next-boolean-seed") program)
    Assert.Equal<bool list>(r1, r2)

[<Fact>]
let ``nextBoolean uses a strict greater-than threshold`` () =
    let ctx = mockDoubles [ 0.5; 0.500001; 0.1 ]

    let program =
        effect {
            let! a = Random.nextBoolean
            let! b = Random.nextBoolean
            let! c = Random.nextBoolean
            return [ a; b; c ]
        }

    Assert.Equal<bool list>([ false; true; false ], run ctx program)

[<Fact>]
let ``nextInt generates a safe integer in range`` () =
    let value = run empty Random.nextInt
    Assert.True(value >= -9_007_199_254_740_991.0 && value <= 9_007_199_254_740_991.0)
    Assert.Equal(value, floor value)

[<Fact>]
let ``nextBetween generates number in half-open range`` () =
    for _ in 1..100 do
        let value = run empty (Random.nextBetween 10.0 20.0)
        Assert.True(value >= 10.0 && value < 20.0)

[<Fact>]
let ``nextBetween does not round the bounds`` () =
    let ctx = mockDoubles [ 0.75 ]
    Assert.Equal(18.0, run ctx (Random.nextBetween 10.5 20.5))

[<Fact>]
let ``nextBetween handles negative ranges`` () =
    let value = run empty (Random.nextBetween -10.0 10.0)
    Assert.True(value >= -10.0 && value < 10.0)

[<Fact>]
let ``nextIntBetween generates integer in closed range`` () =
    for _ in 1..100 do
        let value = run empty (Random.nextIntBetween 1.0 6.0 false)
        Assert.Equal(value, floor value)
        Assert.True(value >= 1.0 && value <= 6.0)

[<Fact>]
let ``nextIntBetween generates integer in half-open range`` () =
    for _ in 1..100 do
        let value = run empty (Random.nextIntBetween 1.0 6.0 true)
        Assert.True(value >= 1.0 && value < 6.0)

[<Fact>]
let ``nextIntBetween excludes the upper bound in half-open ranges`` () =
    // JS uses `1 - Number.EPSILON`; the nearest double below 1.0 is the F# analogue.
    let ctx = mockDoubles [ System.Math.BitDecrement 1.0 ]
    Assert.Equal(5.0, run ctx (Random.nextIntBetween 1.0 6.0 true))

[<Fact>]
let ``shuffle returns a shuffled copy of all values`` () =
    let input = [ 1; 2; 3; 4; 5 ]
    let output = run empty (Random.shuffle input)
    Assert.Equal<int list>([ 1; 2; 3; 4; 5 ], input)
    Assert.Equal<int list>([ 1; 2; 3; 4; 5 ], List.sort (List.ofArray output))

[<Fact>]
let ``shuffle is deterministic with the same seed`` () =
    let program = Random.shuffle [ 1; 2; 3; 4; 5 ]
    let r1 = run empty (Random.withSeed (SeedString "shuffle-seed") program)
    let r2 = run empty (Random.withSeed (SeedString "shuffle-seed") program)
    Assert.Equal<int[]>(r1, r2)

[<Fact>]
let ``choice selects a random element from an iterable`` () =
    let ctx = mockDoubles [ 0.75 ]
    Assert.Equal("c", run ctx (Random.choice [ "a"; "b"; "c" ]))

[<Fact>]
let ``choice fails with NoSuchElementError for an empty iterable`` () =
    match Effect.runSync empty (Random.choice ([]: int list)) with
    | Failure c ->
        match Cause.failures c with
        | (e: NoSuchElementError) :: _ -> Assert.Equal("Cannot select a random element from an empty array", e.Message)
        | [] -> Assert.Fail "expected a typed failure"
    | Success _ -> Assert.Fail "expected failure"

[<Fact>]
let ``choice is deterministic with the same seed`` () =
    let program =
        effect {
            let! a = Random.choice [ 1; 2; 3; 4; 5 ]
            let! b = Random.choice [ 1; 2; 3; 4; 5 ]
            let! c = Random.choice [ 1; 2; 3; 4; 5 ]
            return [ a; b; c ]
        }

    let r1 = run empty (Random.withSeed (SeedString "choice-seed") program)
    let r2 = run empty (Random.withSeed (SeedString "choice-seed") program)
    Assert.Equal<int list>(r1, r2)

let private seq5: Effect<float list, string, Context> =
    effect {
        let! a = Random.next
        let! b = Random.next
        let! c = Random.next
        let! d = Random.next
        let! e = Random.next
        return [ a; b; c; d; e ]
    }

[<Fact>]
let ``withSeed produces deterministic sequence with same seed`` () =
    let r1 = run empty (Random.withSeed (SeedString "test-seed") seq5)
    let r2 = run empty (Random.withSeed (SeedString "test-seed") seq5)
    Assert.Equal<float list>(r1, r2)

[<Fact>]
let ``withSeed produces different sequences with different seeds`` () =
    let r1 = run empty (Random.withSeed (SeedInt 12345) seq5)
    let r2 = run empty (Random.withSeed (SeedInt 67890) seq5)
    Assert.NotEqual<float list>(r1, r2)

[<Fact>]
let ``withSeed distinguishes one-character string seeds`` () =
    let e = run empty (Random.withSeed (SeedString "") seq5)
    let a = run empty (Random.withSeed (SeedString "a") seq5)
    let b = run empty (Random.withSeed (SeedString "b") seq5)
    Assert.NotEqual<float list>(a, e)
    Assert.NotEqual<float list>(b, e)
    Assert.NotEqual<float list>(a, b)

[<Fact>]
let ``withSeed distinguishes trailing partial UTF-8 words in string seeds`` () =
    for s1, s2 in [ "1234a", "1234b"; "1234ab", "1234ac"; "1234abc", "1234abd" ] do
        let r1 = run empty (Random.withSeed (SeedString s1) seq5)
        let r2 = run empty (Random.withSeed (SeedString s2) seq5)
        Assert.NotEqual<float list>(r1, r2)

[<Fact>]
let ``withSeed distinguishes astral character string seeds`` () =
    let e = run empty (Random.withSeed (SeedString "") seq5)
    let r1 = run empty (Random.withSeed (SeedString "😀") seq5)
    let r2 = run empty (Random.withSeed (SeedString "😁") seq5)
    let r3 = run empty (Random.withSeed (SeedString "🐀") seq5)
    Assert.NotEqual<float list>(r1, e)
    Assert.NotEqual<float list>(r2, e)
    Assert.NotEqual<float list>(r1, r2)
    Assert.NotEqual<float list>(r1, r3)

[<Fact>]
let ``withSeed works with numeric seeds`` () =
    let r1 = run empty (Random.withSeed (SeedInt 12345) seq5)
    let r2 = run empty (Random.withSeed (SeedInt 12345) seq5)
    Assert.Equal<float list>(r1, r2)
