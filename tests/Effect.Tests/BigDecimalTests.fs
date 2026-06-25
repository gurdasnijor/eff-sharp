module Effect.Tests.BigDecimalTests

open System.Numerics
open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/BigDecimal.test.ts
//
// Upstream `assertEquals`/`assertSome` use Effect's normalized equality, mapped
// here to `BigDecimal.equals`. Upstream `deepStrictEqual` (structural / scale
// sensitive) maps to F# structural `=`. Property-based tests are skipped (they
// are `describe.skip` upstream). JS-only behaviours (toJSON, NodeInspect, pipe,
// Equal.symbol) are dropped per BigDecimal.fs doc comment.

let private S = BigDecimal.fromStringUnsafe
let private bd (v: int) (s: int) = BigDecimal.make (BigInteger v) s

let private assertEq (expected: BigDecimal) (actual: BigDecimal) =
    Assert.True(
        BigDecimal.equals expected actual,
        sprintf "expected %s = %s" (BigDecimal.format expected) (BigDecimal.format actual)
    )

let private assertSomeEq (expected: BigDecimal) (actual: BigDecimal option) =
    match actual with
    | Some a -> assertEq expected a
    | None -> Assert.Fail "expected Some"

[<Fact>]
let ``isBigDecimal`` () =
    Assert.True(BigDecimal.isBigDecimal (box (S "0")))
    Assert.True(BigDecimal.isBigDecimal (box (S "987")))
    Assert.True(BigDecimal.isBigDecimal (box (S "123.0")))
    Assert.True(BigDecimal.isBigDecimal (box (S "0.123")))
    Assert.True(BigDecimal.isBigDecimal (box (S "123.456")))
    Assert.False(BigDecimal.isBigDecimal (box "1"))
    Assert.False(BigDecimal.isBigDecimal (box true))

[<Fact>]
let ``sign`` () =
    Assert.Equal(-1, BigDecimal.sign (S "-5"))
    Assert.Equal(0, BigDecimal.sign (S "0"))
    Assert.Equal(1, BigDecimal.sign (S "5"))
    Assert.Equal(-1, BigDecimal.sign (S "-123.456"))
    Assert.Equal(1, BigDecimal.sign (S "456.789"))

[<Fact>]
let ``equals`` () =
    Assert.True(BigDecimal.equals (S "1") (S "1"))
    Assert.True(BigDecimal.equals (S "0.00012300") (S "0.000123"))
    Assert.True(BigDecimal.equals (S "5") (S "5.0"))
    Assert.True(BigDecimal.equals (S "123.0000") (S "123.00"))
    Assert.False(BigDecimal.equals (S "1") (S "2"))
    Assert.False(BigDecimal.equals (S "1") (S "1.1"))
    Assert.False(BigDecimal.equals (S "1") (S "0.1"))

[<Fact>]
let ``sum`` () =
    assertEq (S "2") (BigDecimal.sum (S "2") (S "0"))
    assertEq (S "2") (BigDecimal.sum (S "0") (S "2"))
    assertEq (S "0") (BigDecimal.sum (S "0") (S "0"))
    assertEq (S "3") (BigDecimal.sum (S "2") (S "1"))
    assertEq (S "53") (BigDecimal.sum (S "3.00000") (S "50"))
    assertEq (S "1.2345678") (BigDecimal.sum (S "1.23") (S "0.0045678"))
    assertEq (S "0") (BigDecimal.sum (S "123.456") (S "-123.456"))

[<Fact>]
let ``sumAll`` () =
    assertEq (S "0") (BigDecimal.sumAll [])
    assertEq (S "9") (BigDecimal.sumAll [ S "2"; S "3"; S "4" ])
    assertEq (S "0") (BigDecimal.sumAll [ S "1.5"; S "-1.5" ])

[<Fact>]
let ``multiply`` () =
    assertEq (S "6") (BigDecimal.multiply (S "3") (S "2"))
    assertEq (S "0") (BigDecimal.multiply (S "3") (S "0"))
    assertEq (S "-3") (BigDecimal.multiply (S "3") (S "-1"))
    assertEq (S "1.5") (BigDecimal.multiply (S "3") (S "0.5"))
    assertEq (S "-7.5") (BigDecimal.multiply (S "3") (S "-2.5"))

[<Fact>]
let ``multiplyAll`` () =
    assertEq (S "1") (BigDecimal.multiplyAll [])
    assertEq (S "24") (BigDecimal.multiplyAll [ S "2"; S "3"; S "4" ])
    assertEq (S "0") (BigDecimal.multiplyAll [ S "2"; S "0"; S "4" ])

[<Fact>]
let ``subtract`` () =
    assertEq (S "-1") (BigDecimal.subtract (S "0") (S "1"))
    assertEq (S "1.1") (BigDecimal.subtract (S "2.1") (S "1"))
    assertEq (S "2") (BigDecimal.subtract (S "3") (S "1"))
    assertEq (S "3") (BigDecimal.subtract (S "3") (S "0"))
    assertEq (S "4") (BigDecimal.subtract (S "3") (S "-1"))
    assertEq (S "2.5") (BigDecimal.subtract (S "3") (S "0.5"))
    assertEq (S "5.5") (BigDecimal.subtract (S "3") (S "-2.5"))

[<Fact>]
let ``roundTerminal`` () =
    let z = BigInteger.Zero
    let o = BigInteger.One
    Assert.Equal(z, BigDecimal.roundTerminal (BigInteger 0))
    Assert.Equal(z, BigDecimal.roundTerminal (BigInteger 4))
    Assert.Equal(o, BigDecimal.roundTerminal (BigInteger 5))
    Assert.Equal(o, BigDecimal.roundTerminal (BigInteger 9))
    Assert.Equal(z, BigDecimal.roundTerminal (BigInteger 49))
    Assert.Equal(o, BigDecimal.roundTerminal (BigInteger 59))
    Assert.Equal(o, BigDecimal.roundTerminal (BigInteger 99))
    Assert.Equal(z, BigDecimal.roundTerminal (BigInteger -4))
    Assert.Equal(o, BigDecimal.roundTerminal (BigInteger -5))
    Assert.Equal(o, BigDecimal.roundTerminal (BigInteger -9))
    Assert.Equal(z, BigDecimal.roundTerminal (BigInteger -49))
    Assert.Equal(o, BigDecimal.roundTerminal (BigInteger -59))
    Assert.Equal(o, BigDecimal.roundTerminal (BigInteger -99))

[<Fact>]
let ``divide`` () =
    let assertDivide (x: string) (y: string) (z: string) =
        assertSomeEq (S z) (BigDecimal.divide (S x) (S y))
        assertEq (S z) (BigDecimal.divideUnsafe (S x) (S y))

    assertDivide "0" "1" "0"
    assertDivide "0" "10" "0"
    assertDivide "2" "1" "2"
    assertDivide "20" "1" "20"
    assertDivide "10" "10" "1"
    assertDivide "100" "10.0" "10"
    assertDivide "20.0" "200" "0.1"
    assertDivide "4" "2" "2.0"
    assertDivide "15" "3" "5.0"
    assertDivide "1" "2" "0.5"
    assertDivide "1" "0.02" "50"
    assertDivide "1" "0.2" "5"
    assertDivide "1.0" "0.02" "50"
    assertDivide "1" "0.020" "50"
    assertDivide "5.0" "4.00" "1.25"
    assertDivide "5.0" "4.000" "1.25"
    assertDivide "5" "4.000" "1.25"
    assertDivide "5" "4" "1.25"
    assertDivide "100" "5" "20"
    assertDivide "-50" "5" "-10"
    assertDivide "200" "-5" "-40.0"

    assertDivide
        "1"
        "3"
        "0.3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333"

    assertDivide
        "-2"
        "-3"
        "0.6666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666667"

    assertDivide
        "-12.34"
        "1.233"
        "-10.00811030008110300081103000811030008110300081103000811030008110300081103000811030008110300081103001"

    assertDivide
        "125348"
        "352.2283"
        "355.8714617763535752237966114591019517738921035021887792661748076460636467881768727839301952739175132"

    Assert.True((BigDecimal.divide (S "5") (S "0")).IsNone)

    Assert.Throws<System.InvalidOperationException>(fun () -> BigDecimal.divideUnsafe (S "5") (S "0") |> ignore)
    |> ignore

[<Fact>]
let ``Equivalence`` () =
    Assert.True(BigDecimal.equivalence (S "1") (S "1"))
    Assert.True(BigDecimal.equivalence (S "0.00012300") (S "0.000123"))
    Assert.True(BigDecimal.equivalence (S "5") (S "5.00"))
    Assert.False(BigDecimal.equivalence (S "1") (S "2"))
    Assert.False(BigDecimal.equivalence (S "1") (S "1.1"))

[<Fact>]
let ``Order`` () =
    Assert.Equal(-1, BigDecimal.order (S "1") (S "2"))
    Assert.Equal(1, BigDecimal.order (S "2") (S "1"))
    Assert.Equal(0, BigDecimal.order (S "2") (S "2"))
    Assert.Equal(-1, BigDecimal.order (S "1") (S "1.1"))
    Assert.Equal(1, BigDecimal.order (S "1.1") (S "1"))
    Assert.Equal(0, BigDecimal.order (S "0.00012300") (S "0.000123"))
    Assert.Equal(0, BigDecimal.order (S "5") (S "5.000"))
    Assert.Equal(1, BigDecimal.order (S "5") (S "0.500"))
    Assert.Equal(-1, BigDecimal.order (S "5") (S "50.00"))

[<Fact>]
let ``isLessThan`` () =
    Assert.True(BigDecimal.isLessThan (S "2") (S "3"))
    Assert.False(BigDecimal.isLessThan (S "3") (S "3"))
    Assert.False(BigDecimal.isLessThan (S "4") (S "3"))

[<Fact>]
let ``isLessThanOrEqualTo`` () =
    Assert.True(BigDecimal.isLessThanOrEqualTo (S "2") (S "3"))
    Assert.True(BigDecimal.isLessThanOrEqualTo (S "3") (S "3"))
    Assert.False(BigDecimal.isLessThanOrEqualTo (S "4") (S "3"))

[<Fact>]
let ``isGreaterThan`` () =
    Assert.False(BigDecimal.isGreaterThan (S "2") (S "3"))
    Assert.False(BigDecimal.isGreaterThan (S "3") (S "3"))
    Assert.True(BigDecimal.isGreaterThan (S "4") (S "3"))

[<Fact>]
let ``isGreaterThanOrEqualTo`` () =
    Assert.False(BigDecimal.isGreaterThanOrEqualTo (S "2") (S "3"))
    Assert.True(BigDecimal.isGreaterThanOrEqualTo (S "3") (S "3"))
    Assert.True(BigDecimal.isGreaterThanOrEqualTo (S "4") (S "3"))

[<Fact>]
let ``between`` () =
    Assert.True(BigDecimal.between (S "0") (S "5") (S "3"))
    Assert.False(BigDecimal.between (S "0") (S "5") (S "-1"))
    Assert.False(BigDecimal.between (S "0") (S "5") (S "6"))
    Assert.False(BigDecimal.between (S "0.02") (S "5") (S "0.0123"))
    Assert.True(BigDecimal.between (S "0.02") (S "5") (S "0.05"))

[<Fact>]
let ``clamp`` () =
    assertEq (S "3") (BigDecimal.clamp (S "0") (S "5") (S "3"))
    assertEq (S "0") (BigDecimal.clamp (S "0") (S "5") (S "-1"))
    assertEq (S "5") (BigDecimal.clamp (S "0") (S "5") (S "6"))
    assertEq (S "0.02") (BigDecimal.clamp (S "0.02") (S "5") (S "0.0123"))

[<Fact>]
let ``min`` () =
    assertEq (S "2") (BigDecimal.min (S "2") (S "3"))
    assertEq (S "0.1") (BigDecimal.min (S "5") (S "0.1"))
    assertEq (S "0.005") (BigDecimal.min (S "0.005") (S "3"))
    assertEq (S "1.2") (BigDecimal.min (S "123.456") (S "1.2"))

[<Fact>]
let ``max`` () =
    assertEq (S "3") (BigDecimal.max (S "2") (S "3"))
    assertEq (S "5") (BigDecimal.max (S "5") (S "0.1"))
    assertEq (S "3") (BigDecimal.max (S "0.005") (S "3"))
    assertEq (S "123.456") (BigDecimal.max (S "123.456") (S "1.2"))

[<Fact>]
let ``abs`` () =
    assertEq (S "2") (BigDecimal.abs (S "2"))
    assertEq (S "3") (BigDecimal.abs (S "-3"))
    assertEq (S "0.000456") (BigDecimal.abs (S "0.000456"))
    assertEq (S "0.123") (BigDecimal.abs (S "-0.123"))

[<Fact>]
let ``negate`` () =
    assertEq (S "-2") (BigDecimal.negate (S "2"))
    assertEq (S "3") (BigDecimal.negate (S "-3"))
    assertEq (S "-0.000456") (BigDecimal.negate (S "0.000456"))
    assertEq (S "0.123") (BigDecimal.negate (S "-0.123"))

[<Fact>]
let ``remainder`` () =
    assertSomeEq (S "1") (BigDecimal.remainder (S "5") (S "2"))
    assertSomeEq (S "0") (BigDecimal.remainder (S "4") (S "2"))
    assertSomeEq (S "0.056") (BigDecimal.remainder (S "123.456") (S "0.2"))
    Assert.True((BigDecimal.remainder (S "5") (S "0")).IsNone)

[<Fact>]
let ``remainderUnsafe`` () =
    assertEq (S "1") (BigDecimal.remainderUnsafe (S "5") (S "2"))
    assertEq (S "0") (BigDecimal.remainderUnsafe (S "4") (S "2"))
    assertEq (S "0.056") (BigDecimal.remainderUnsafe (S "123.456") (S "0.2"))

    Assert.Throws<System.InvalidOperationException>(fun () -> BigDecimal.remainderUnsafe (S "5") (S "0") |> ignore)
    |> ignore

[<Fact>]
let ``normalize`` () =
    Assert.Equal(BigDecimal.makeNormalizedUnsafe (BigInteger 0) 0, BigDecimal.normalize (S "0"))
    Assert.Equal(BigDecimal.makeNormalizedUnsafe (BigInteger 123) 3, BigDecimal.normalize (S "0.123000"))
    Assert.Equal(BigDecimal.makeNormalizedUnsafe (BigInteger 123) 0, BigDecimal.normalize (S "123.000"))
    Assert.Equal(BigDecimal.makeNormalizedUnsafe (BigInteger -123) 6, BigDecimal.normalize (S "-0.000123000"))
    Assert.Equal(BigDecimal.makeNormalizedUnsafe (BigInteger -123) 0, BigDecimal.normalize (S "-123.000"))
    Assert.Equal(BigDecimal.makeNormalizedUnsafe (BigInteger 123) -5, BigDecimal.normalize (S "12300000"))

[<Fact>]
let ``fromString`` () =
    // structural equality: scale-sensitive representation checks
    Assert.Equal(Some(bd 2 0), BigDecimal.fromString "2")
    Assert.Equal(Some(bd -2 0), BigDecimal.fromString "-2")
    Assert.Equal(Some(bd 123 3), BigDecimal.fromString "0.123")
    Assert.Equal(Some(bd 200 0), BigDecimal.fromString "200")
    Assert.Equal(Some(bd 20000000 0), BigDecimal.fromString "20000000")
    Assert.Equal(Some(bd -20000000 0), BigDecimal.fromString "-20000000")
    Assert.Equal(Some(bd 200 2), BigDecimal.fromString "2.00")
    Assert.Equal(Some(bd 200 7), BigDecimal.fromString "0.0000200")
    Assert.Equal(Some BigDecimal.zero, BigDecimal.fromString "")
    Assert.Equal(Some(bd 1 -5), BigDecimal.fromString "1e5")
    Assert.Equal(Some(bd 1 -15), BigDecimal.fromString "1E15")
    Assert.Equal(Some(bd 1 -5), BigDecimal.fromString "1e+5")
    Assert.Equal(Some(bd 1 -15), BigDecimal.fromString "1E+15")
    Assert.Equal(Some(bd -15 -2), BigDecimal.fromString "-1.5E3")
    Assert.Equal(Some(bd -15 -2), BigDecimal.fromString "-1.5e3")
    Assert.Equal(Some(bd -5 -2), BigDecimal.fromString "-.5e3")
    Assert.Equal(Some(bd -5 -3), BigDecimal.fromString "-5e3")
    Assert.Equal(Some(bd -5 3), BigDecimal.fromString "-5e-3")
    Assert.Equal(Some(bd 15 3), BigDecimal.fromString "15e-3")
    Assert.Equal(Some(bd 2 0), BigDecimal.fromString "0.00002e5")
    Assert.Equal(Some(bd 2 10), BigDecimal.fromString "0.00002e-5")
    Assert.Equal(None, BigDecimal.fromString "0.0000e2e1")
    Assert.Equal(None, BigDecimal.fromString "0.1.2")

[<Fact>]
let ``format`` () =
    Assert.Equal("2", BigDecimal.format (S "2"))
    Assert.Equal("-2", BigDecimal.format (S "-2"))
    Assert.Equal("0.123", BigDecimal.format (S "0.123"))
    Assert.Equal("200", BigDecimal.format (S "200"))
    Assert.Equal("20000000", BigDecimal.format (S "20000000"))
    Assert.Equal("-20000000", BigDecimal.format (S "-20000000"))
    Assert.Equal("2", BigDecimal.format (S "2.00"))
    Assert.Equal("0.2", BigDecimal.format (S "0.200"))
    Assert.Equal("0.123", BigDecimal.format (S "0.123000"))
    Assert.Equal("-456.123", BigDecimal.format (S "-456.123"))
    Assert.Equal("100", BigDecimal.format (bd 10 -1))
    Assert.Equal("1e+25", BigDecimal.format (bd 1 -25))
    Assert.Equal("1.2345e+29", BigDecimal.format (bd 12345 -25))
    Assert.Equal("1.2345e-21", BigDecimal.format (bd 12345 25))
    Assert.Equal("-1.2345e-16", BigDecimal.format (bd -12345 20))

[<Fact>]
let ``toExponential`` () =
    Assert.Equal("1.23456e+10", BigDecimal.toExponential (bd 123456 -5))

[<Fact>]
let ``toString`` () =
    Assert.Equal("BigDecimal(2)", BigDecimal.toString (S "2"))

[<Fact>]
let ``scale`` () =
    Assert.Equal(S "3.000", BigDecimal.scale 3 (S "3.0005"))

[<Fact>]
let ``fromBigInt`` () =
    Assert.Equal(bd 1 0, BigDecimal.fromBigInt BigInteger.One)

[<Fact>]
let ``fromNumber`` () =
    Assert.Equal(Some(bd 123 0), BigDecimal.fromNumber 123.0)
    Assert.Equal(Some(bd 123456 3), BigDecimal.fromNumber 123.456)
    Assert.Equal(None, BigDecimal.fromNumber System.Double.PositiveInfinity)

[<Fact>]
let ``toNumberUnsafe`` () =
    Assert.Equal(123.456, BigDecimal.toNumberUnsafe (S "123.456"))

[<Fact>]
let ``isInteger`` () =
    Assert.True(BigDecimal.isInteger (S "0"))
    Assert.True(BigDecimal.isInteger (S "1"))
    Assert.False(BigDecimal.isInteger (S "1.1"))

[<Fact>]
let ``isZero`` () =
    Assert.True(BigDecimal.isZero (S "0"))
    Assert.False(BigDecimal.isZero (S "1"))

[<Fact>]
let ``isNegative`` () =
    Assert.True(BigDecimal.isNegative (S "-1"))
    Assert.False(BigDecimal.isNegative (S "0"))
    Assert.False(BigDecimal.isNegative (S "1"))

[<Fact>]
let ``isPositive`` () =
    Assert.False(BigDecimal.isPositive (S "-1"))
    Assert.False(BigDecimal.isPositive (S "0"))
    Assert.True(BigDecimal.isPositive (S "1"))

[<Fact>]
let ``digitAt`` () =
    let z = BigInteger.Zero
    Assert.Equal(z, BigDecimal.digitAt -2 (S "12.34"))
    Assert.Equal(BigInteger 1, BigDecimal.digitAt -1 (S "12.34"))
    Assert.Equal(BigInteger 2, BigDecimal.digitAt 0 (S "12.34"))
    Assert.Equal(BigInteger 3, BigDecimal.digitAt 1 (S "12.34"))
    Assert.Equal(BigInteger 4, BigDecimal.digitAt 2 (S "12.34"))
    Assert.Equal(z, BigDecimal.digitAt 3 (S "12.34"))

[<Fact>]
let ``round ceil`` () =
    assertEq (S "150") (BigDecimal.ceil -1 (S "145"))
    assertEq (S "-14") (BigDecimal.ceil 0 (S "-14.5"))
    assertEq (S "1000") (BigDecimal.round Ceil -3 (S "321.123"))
    assertEq (S "150") (BigDecimal.round Ceil -1 (S "145"))
    assertEq (S "-2") (BigDecimal.round Ceil 0 (S "-2.0"))
    assertEq (S "-1") (BigDecimal.round Ceil 0 (S "-1.9"))
    assertEq (S "0.1234567898766") (BigDecimal.round Ceil 13 (S "0.12345678987654321"))
    assertEq (S "-0.1234567898765") (BigDecimal.round Ceil 13 (S "-0.12345678987654321"))

[<Fact>]
let ``round floor`` () =
    assertEq (S "140") (BigDecimal.floor -1 (S "145"))
    assertEq (S "-15") (BigDecimal.floor 0 (S "-14.5"))
    assertEq (S "0") (BigDecimal.round Floor -3 (S "321.123"))
    assertEq (S "140") (BigDecimal.round Floor -1 (S "145"))
    assertEq (S "-3") (BigDecimal.round Floor 0 (S "-2.1"))
    assertEq (S "-2") (BigDecimal.round Floor 0 (S "-1.9"))
    assertEq (S "0.1234567898765") (BigDecimal.round Floor 13 (S "0.12345678987654321"))
    assertEq (S "-0.1234567898766") (BigDecimal.round Floor 13 (S "-0.12345678987654321"))

[<Fact>]
let ``round to-zero`` () =
    assertEq (S "140") (BigDecimal.truncate -1 (S "145"))
    assertEq (S "-14") (BigDecimal.truncate 0 (S "-14.5"))
    assertEq (S "0") (BigDecimal.round ToZero -3 (S "321.123"))
    assertEq (S "140") (BigDecimal.round ToZero -1 (S "145"))
    assertEq (S "-2") (BigDecimal.round ToZero 0 (S "-2.1"))
    assertEq (S "-1") (BigDecimal.round ToZero 0 (S "-1.9"))
    assertEq (S "0.1234567898765") (BigDecimal.round ToZero 13 (S "0.12345678987654321"))
    assertEq (S "-0.1234567898765") (BigDecimal.round ToZero 13 (S "-0.12345678987654321"))

[<Fact>]
let ``round from-zero`` () =
    assertEq (S "1000") (BigDecimal.round FromZero -3 (S "321.123"))
    assertEq (S "150") (BigDecimal.round FromZero -1 (S "145"))
    assertEq (S "-3") (BigDecimal.round FromZero 0 (S "-2.1"))
    assertEq (S "-2") (BigDecimal.round FromZero 0 (S "-1.9"))
    assertEq (S "0.1234567898766") (BigDecimal.round FromZero 13 (S "0.12345678987654321"))
    assertEq (S "-0.1234567898766") (BigDecimal.round FromZero 13 (S "-0.12345678987654321"))

[<Fact>]
let ``round half-ceil`` () =
    assertEq (S "0") (BigDecimal.round HalfCeil -3 (S "321.123"))
    assertEq (S "150") (BigDecimal.round HalfCeil -1 (S "145"))
    assertEq (S "-2") (BigDecimal.round HalfCeil 0 (S "-2.5"))
    assertEq (S "2") (BigDecimal.round HalfCeil 1 (S "1.95"))
    assertEq (S "-1.9") (BigDecimal.round HalfCeil 1 (S "-1.95"))
    assertEq (S "0.123456789877") (BigDecimal.round HalfCeil 12 (S "0.1234567898765"))
    assertEq (S "-0.123456789876") (BigDecimal.round HalfCeil 12 (S "-0.1234567898765"))

[<Fact>]
let ``round half-floor`` () =
    assertEq (S "0") (BigDecimal.round HalfFloor -3 (S "321.123"))
    assertEq (S "140") (BigDecimal.round HalfFloor -1 (S "145"))
    assertEq (S "-2") (BigDecimal.round HalfFloor 0 (S "-2.4"))
    assertEq (S "-3") (BigDecimal.round HalfFloor 0 (S "-2.5"))
    assertEq (S "1.9") (BigDecimal.round HalfFloor 1 (S "1.95"))
    assertEq (S "-2") (BigDecimal.round HalfFloor 1 (S "-1.95"))
    assertEq (S "0.123456789876") (BigDecimal.round HalfFloor 12 (S "0.1234567898765"))
    assertEq (S "-0.123456789877") (BigDecimal.round HalfFloor 12 (S "-0.1234567898765"))

[<Fact>]
let ``round half-to-zero`` () =
    assertEq (S "0") (BigDecimal.round HalfToZero -3 (S "321.123"))
    assertEq (S "140") (BigDecimal.round HalfToZero -1 (S "145"))
    assertEq (S "-2") (BigDecimal.round HalfToZero 0 (S "-2.4"))
    assertEq (S "-2") (BigDecimal.round HalfToZero 0 (S "-2.5"))
    assertEq (S "1.9") (BigDecimal.round HalfToZero 1 (S "1.95"))
    assertEq (S "-1.9") (BigDecimal.round HalfToZero 1 (S "-1.95"))
    assertEq (S "0.123456789876") (BigDecimal.round HalfToZero 12 (S "0.1234567898765"))
    assertEq (S "-0.123456789876") (BigDecimal.round HalfToZero 12 (S "-0.1234567898765"))

[<Fact>]
let ``round half-from-zero`` () =
    assertEq (S "0") (BigDecimal.round HalfFromZero -3 (S "321.123"))
    assertEq (S "150") (BigDecimal.round HalfFromZero -1 (S "145"))
    assertEq (S "-2") (BigDecimal.round HalfFromZero 0 (S "-2.4"))
    assertEq (S "-3") (BigDecimal.round HalfFromZero 0 (S "-2.5"))
    assertEq (S "2") (BigDecimal.round HalfFromZero 1 (S "1.95"))
    assertEq (S "-2") (BigDecimal.round HalfFromZero 1 (S "-1.95"))
    assertEq (S "0.123456789877") (BigDecimal.round HalfFromZero 12 (S "0.1234567898765"))
    assertEq (S "-0.123456789877") (BigDecimal.round HalfFromZero 12 (S "-0.1234567898765"))

[<Fact>]
let ``round half-even`` () =
    assertEq (S "0") (BigDecimal.round HalfEven -3 (S "321.123"))
    assertEq (S "140") (BigDecimal.round HalfEven -1 (S "145"))
    assertEq (S "-2") (BigDecimal.round HalfEven 0 (S "-2.4"))
    assertEq (S "-2") (BigDecimal.round HalfEven 0 (S "-2.5"))
    assertEq (S "2") (BigDecimal.round HalfEven 1 (S "1.95"))
    assertEq (S "-2") (BigDecimal.round HalfEven 1 (S "-1.95"))
    assertEq (S "0.123456789876") (BigDecimal.round HalfEven 12 (S "0.1234567898765"))
    assertEq (S "-0.123456789876") (BigDecimal.round HalfEven 12 (S "-0.1234567898765"))
