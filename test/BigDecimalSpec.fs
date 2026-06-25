module BigDecimalSpec

open System.Numerics
open Effect
open Effect.Vitest

let private S = BigDecimal.fromStringUnsafe
let private bd (v: int) (s: int) = BigDecimal.make (BigInteger v) s
let private bdl (v: int64) (s: int) = BigDecimal.make (BigInteger v) s

let private same actual expected =
    toBe (actual = expected) true

let private assertEq (expected: BigDecimal) (actual: BigDecimal) =
    if not (BigDecimal.equals expected actual) then
        failwithf "expected %s = %s" (BigDecimal.format expected) (BigDecimal.format actual)

let private assertSomeEq (expected: BigDecimal) (actual: BigDecimal option) =
    match actual with
    | Some a -> assertEq expected a
    | None -> failwith "expected Some"

describe "BigDecimal" (fun () ->
    test "guards sign and scale-insensitive equality" (fun () ->
        toBe (BigDecimal.isBigDecimal (box (S "0"))) true
        toBe (BigDecimal.isBigDecimal (box (S "987"))) true
        toBe (BigDecimal.isBigDecimal (box (S "123.456"))) true
        toBe (BigDecimal.isBigDecimal (box "1")) false
        toBe (BigDecimal.isBigDecimal (box true)) false

        toBe (BigDecimal.sign (S "-5")) -1
        toBe (BigDecimal.sign (S "0")) 0
        toBe (BigDecimal.sign (S "5")) 1
        toBe (BigDecimal.sign (S "-123.456")) -1
        toBe (BigDecimal.sign (S "456.789")) 1

        toBe (BigDecimal.equals (S "1") (S "1")) true
        toBe (BigDecimal.equals (S "0.00012300") (S "0.000123")) true
        toBe (BigDecimal.equals (S "5") (S "5.0")) true
        toBe (BigDecimal.equals (S "123.0000") (S "123.00")) true
        toBe (BigDecimal.equals (S "1") (S "2")) false
        toBe (BigDecimal.equals (S "1") (S "1.1")) false)

    test "adds multiplies and subtracts" (fun () ->
        assertEq (S "2") (BigDecimal.sum (S "2") (S "0"))
        assertEq (S "2") (BigDecimal.sum (S "0") (S "2"))
        assertEq (S "0") (BigDecimal.sum (S "0") (S "0"))
        assertEq (S "3") (BigDecimal.sum (S "2") (S "1"))
        assertEq (S "53") (BigDecimal.sum (S "3.00000") (S "50"))
        assertEq (S "1.2345678") (BigDecimal.sum (S "1.23") (S "0.0045678"))
        assertEq (S "0") (BigDecimal.sum (S "123.456") (S "-123.456"))
        assertEq (S "0") (BigDecimal.sumAll [])
        assertEq (S "9") (BigDecimal.sumAll [ S "2"; S "3"; S "4" ])

        assertEq (S "6") (BigDecimal.multiply (S "3") (S "2"))
        assertEq (S "0") (BigDecimal.multiply (S "3") (S "0"))
        assertEq (S "-3") (BigDecimal.multiply (S "3") (S "-1"))
        assertEq (S "1.5") (BigDecimal.multiply (S "3") (S "0.5"))
        assertEq (S "-7.5") (BigDecimal.multiply (S "3") (S "-2.5"))
        assertEq (S "1") (BigDecimal.multiplyAll [])
        assertEq (S "24") (BigDecimal.multiplyAll [ S "2"; S "3"; S "4" ])
        assertEq (S "0") (BigDecimal.multiplyAll [ S "2"; S "0"; S "4" ])

        assertEq (S "-1") (BigDecimal.subtract (S "0") (S "1"))
        assertEq (S "1.1") (BigDecimal.subtract (S "2.1") (S "1"))
        assertEq (S "2") (BigDecimal.subtract (S "3") (S "1"))
        assertEq (S "4") (BigDecimal.subtract (S "3") (S "-1"))
        assertEq (S "2.5") (BigDecimal.subtract (S "3") (S "0.5"))
        assertEq (S "5.5") (BigDecimal.subtract (S "3") (S "-2.5")))

    test "roundTerminal and division precision" (fun () ->
        same (BigDecimal.roundTerminal (BigInteger 0)) BigInteger.Zero
        same (BigDecimal.roundTerminal (BigInteger 4)) BigInteger.Zero
        same (BigDecimal.roundTerminal (BigInteger 5)) BigInteger.One
        same (BigDecimal.roundTerminal (BigInteger 99)) BigInteger.One
        same (BigDecimal.roundTerminal (BigInteger -4)) BigInteger.Zero
        same (BigDecimal.roundTerminal (BigInteger -59)) BigInteger.One

        let assertDivide x y z =
            assertSomeEq (S z) (BigDecimal.divide (S x) (S y))
            assertEq (S z) (BigDecimal.divideUnsafe (S x) (S y))

        assertDivide "0" "1" "0"
        assertDivide "2" "1" "2"
        assertDivide "10" "10" "1"
        assertDivide "100" "10.0" "10"
        assertDivide "20.0" "200" "0.1"
        assertDivide "4" "2" "2.0"
        assertDivide "1" "2" "0.5"
        assertDivide "1" "0.02" "50"
        assertDivide "5" "4" "1.25"
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

        same (BigDecimal.divide (S "5") (S "0")) None
        toThrow (fun () -> BigDecimal.divideUnsafe (S "5") (S "0") |> ignore))

    test "orders and clamps values" (fun () ->
        toBe (BigDecimal.equivalence (S "0.00012300") (S "0.000123")) true
        toBe (BigDecimal.equivalence (S "5") (S "5.00")) true
        toBe (BigDecimal.equivalence (S "1") (S "2")) false

        toBe (BigDecimal.order (S "1") (S "2")) -1
        toBe (BigDecimal.order (S "2") (S "1")) 1
        toBe (BigDecimal.order (S "2") (S "2")) 0
        toBe (BigDecimal.order (S "0.00012300") (S "0.000123")) 0
        toBe (BigDecimal.order (S "5") (S "0.500")) 1
        toBe (BigDecimal.order (S "5") (S "50.00")) -1

        toBe (BigDecimal.isLessThan (S "2") (S "3")) true
        toBe (BigDecimal.isLessThanOrEqualTo (S "3") (S "3")) true
        toBe (BigDecimal.isGreaterThan (S "4") (S "3")) true
        toBe (BigDecimal.isGreaterThanOrEqualTo (S "3") (S "3")) true
        toBe (BigDecimal.between (S "0") (S "5") (S "3")) true
        toBe (BigDecimal.between (S "0") (S "5") (S "6")) false
        toBe (BigDecimal.between (S "0.02") (S "5") (S "0.05")) true

        assertEq (S "3") (BigDecimal.clamp (S "0") (S "5") (S "3"))
        assertEq (S "0") (BigDecimal.clamp (S "0") (S "5") (S "-1"))
        assertEq (S "5") (BigDecimal.clamp (S "0") (S "5") (S "6"))
        assertEq (S "2") (BigDecimal.min (S "2") (S "3"))
        assertEq (S "0.1") (BigDecimal.min (S "5") (S "0.1"))
        assertEq (S "3") (BigDecimal.max (S "2") (S "3"))
        assertEq (S "123.456") (BigDecimal.max (S "123.456") (S "1.2")))

    test "sign helpers and remainders" (fun () ->
        assertEq (S "2") (BigDecimal.abs (S "2"))
        assertEq (S "3") (BigDecimal.abs (S "-3"))
        assertEq (S "0.123") (BigDecimal.abs (S "-0.123"))
        assertEq (S "-2") (BigDecimal.negate (S "2"))
        assertEq (S "3") (BigDecimal.negate (S "-3"))

        assertSomeEq (S "1") (BigDecimal.remainder (S "5") (S "2"))
        assertSomeEq (S "0") (BigDecimal.remainder (S "4") (S "2"))
        assertSomeEq (S "0.056") (BigDecimal.remainder (S "123.456") (S "0.2"))
        same (BigDecimal.remainder (S "5") (S "0")) None
        assertEq (S "1") (BigDecimal.remainderUnsafe (S "5") (S "2"))
        assertEq (S "0.056") (BigDecimal.remainderUnsafe (S "123.456") (S "0.2"))
        toThrow (fun () -> BigDecimal.remainderUnsafe (S "5") (S "0") |> ignore))

    test "normalizes and parses strings with scale-sensitive representation" (fun () ->
        same (BigDecimal.normalize (S "0")) (BigDecimal.makeNormalizedUnsafe (BigInteger 0) 0)
        same (BigDecimal.normalize (S "0.123000")) (BigDecimal.makeNormalizedUnsafe (BigInteger 123) 3)
        same (BigDecimal.normalize (S "123.000")) (BigDecimal.makeNormalizedUnsafe (BigInteger 123) 0)
        same (BigDecimal.normalize (S "-0.000123000")) (BigDecimal.makeNormalizedUnsafe (BigInteger -123) 6)
        same (BigDecimal.normalize (S "12300000")) (BigDecimal.makeNormalizedUnsafe (BigInteger 123) -5)

        same (BigDecimal.fromString "2") (Some(bd 2 0))
        same (BigDecimal.fromString "-2") (Some(bd -2 0))
        same (BigDecimal.fromString "0.123") (Some(bd 123 3))
        same (BigDecimal.fromString "2.00") (Some(bd 200 2))
        same (BigDecimal.fromString "0.0000200") (Some(bd 200 7))
        same (BigDecimal.fromString "") (Some BigDecimal.zero)
        same (BigDecimal.fromString "1e5") (Some(bd 1 -5))
        same (BigDecimal.fromString "1E+15") (Some(bd 1 -15))
        same (BigDecimal.fromString "-1.5E3") (Some(bd -15 -2))
        same (BigDecimal.fromString "-.5e3") (Some(bd -5 -2))
        same (BigDecimal.fromString "-5e-3") (Some(bd -5 3))
        same (BigDecimal.fromString "0.00002e-5") (Some(bd 2 10))
        same (BigDecimal.fromString "0.0000e2e1") None
        same (BigDecimal.fromString "0.1.2") None)

    test "formats and converts representations" (fun () ->
        toBe (BigDecimal.format (S "2")) "2"
        toBe (BigDecimal.format (S "-2")) "-2"
        toBe (BigDecimal.format (S "0.123")) "0.123"
        toBe (BigDecimal.format (S "2.00")) "2"
        toBe (BigDecimal.format (S "0.200")) "0.2"
        toBe (BigDecimal.format (S "-456.123")) "-456.123"
        toBe (BigDecimal.format (bd 10 -1)) "100"
        toBe (BigDecimal.format (bd 1 -25)) "1e+25"
        toBe (BigDecimal.format (bd 12345 -25)) "1.2345e+29"
        toBe (BigDecimal.format (bd 12345 25)) "1.2345e-21"
        toBe (BigDecimal.format (bd -12345 20)) "-1.2345e-16"
        toBe (BigDecimal.toExponential (bd 123456 -5)) "1.23456e+10"
        toBe (BigDecimal.toString (S "2")) "BigDecimal(2)"
        same (BigDecimal.scale 3 (S "3.0005")) (S "3.000")
        same (BigDecimal.fromBigInt BigInteger.One) (bd 1 0)
        same (BigDecimal.fromNumber 123.0) (Some(bd 123 0))
        same (BigDecimal.fromNumber 123.456) (Some(bd 123456 3))
        same (BigDecimal.fromNumber System.Double.PositiveInfinity) None
        toBe (BigDecimal.toNumberUnsafe (S "123.456")) 123.456)

    test "integer zero sign and digit predicates" (fun () ->
        toBe (BigDecimal.isInteger (S "0")) true
        toBe (BigDecimal.isInteger (S "1")) true
        toBe (BigDecimal.isInteger (S "1.1")) false
        toBe (BigDecimal.isZero (S "0")) true
        toBe (BigDecimal.isZero (S "1")) false
        toBe (BigDecimal.isNegative (S "-1")) true
        toBe (BigDecimal.isNegative (S "0")) false
        toBe (BigDecimal.isPositive (S "1")) true
        toBe (BigDecimal.isPositive (S "0")) false
        same (BigDecimal.digitAt -2 (S "12.34")) BigInteger.Zero
        same (BigDecimal.digitAt -1 (S "12.34")) (BigInteger 1)
        same (BigDecimal.digitAt 0 (S "12.34")) (BigInteger 2)
        same (BigDecimal.digitAt 1 (S "12.34")) (BigInteger 3)
        same (BigDecimal.digitAt 2 (S "12.34")) (BigInteger 4)
        same (BigDecimal.digitAt 3 (S "12.34")) BigInteger.Zero)

    test "rounds with directed modes" (fun () ->
        assertEq (S "150") (BigDecimal.ceil -1 (S "145"))
        assertEq (S "-14") (BigDecimal.ceil 0 (S "-14.5"))
        assertEq (S "1000") (BigDecimal.round Ceil -3 (S "321.123"))
        assertEq (S "150") (BigDecimal.round Ceil -1 (S "145"))
        assertEq (S "-1") (BigDecimal.round Ceil 0 (S "-1.9"))
        assertEq (S "0.1234567898766") (BigDecimal.round Ceil 13 (S "0.12345678987654321"))
        assertEq (S "-0.1234567898765") (BigDecimal.round Ceil 13 (S "-0.12345678987654321"))

        assertEq (S "140") (BigDecimal.floor -1 (S "145"))
        assertEq (S "-15") (BigDecimal.floor 0 (S "-14.5"))
        assertEq (S "0") (BigDecimal.round Floor -3 (S "321.123"))
        assertEq (S "-3") (BigDecimal.round Floor 0 (S "-2.1"))
        assertEq (S "0.1234567898765") (BigDecimal.round Floor 13 (S "0.12345678987654321"))
        assertEq (S "-0.1234567898766") (BigDecimal.round Floor 13 (S "-0.12345678987654321"))

        assertEq (S "140") (BigDecimal.truncate -1 (S "145"))
        assertEq (S "-14") (BigDecimal.truncate 0 (S "-14.5"))
        assertEq (S "0") (BigDecimal.round ToZero -3 (S "321.123"))
        assertEq (S "-1") (BigDecimal.round ToZero 0 (S "-1.9"))
        assertEq (S "0.1234567898765") (BigDecimal.round ToZero 13 (S "0.12345678987654321"))
        assertEq (S "-0.1234567898765") (BigDecimal.round ToZero 13 (S "-0.12345678987654321"))

        assertEq (S "1000") (BigDecimal.round FromZero -3 (S "321.123"))
        assertEq (S "150") (BigDecimal.round FromZero -1 (S "145"))
        assertEq (S "-3") (BigDecimal.round FromZero 0 (S "-2.1"))
        assertEq (S "-2") (BigDecimal.round FromZero 0 (S "-1.9"))
        assertEq (S "0.1234567898766") (BigDecimal.round FromZero 13 (S "0.12345678987654321"))
        assertEq (S "-0.1234567898766") (BigDecimal.round FromZero 13 (S "-0.12345678987654321")))

    test "rounds with half modes" (fun () ->
        assertEq (S "0") (BigDecimal.round HalfCeil -3 (S "321.123"))
        assertEq (S "150") (BigDecimal.round HalfCeil -1 (S "145"))
        assertEq (S "-2") (BigDecimal.round HalfCeil 0 (S "-2.5"))
        assertEq (S "2") (BigDecimal.round HalfCeil 1 (S "1.95"))
        assertEq (S "-1.9") (BigDecimal.round HalfCeil 1 (S "-1.95"))
        assertEq (S "0.123456789877") (BigDecimal.round HalfCeil 12 (S "0.1234567898765"))
        assertEq (S "-0.123456789876") (BigDecimal.round HalfCeil 12 (S "-0.1234567898765"))

        assertEq (S "0") (BigDecimal.round HalfFloor -3 (S "321.123"))
        assertEq (S "140") (BigDecimal.round HalfFloor -1 (S "145"))
        assertEq (S "-3") (BigDecimal.round HalfFloor 0 (S "-2.5"))
        assertEq (S "1.9") (BigDecimal.round HalfFloor 1 (S "1.95"))
        assertEq (S "-2") (BigDecimal.round HalfFloor 1 (S "-1.95"))
        assertEq (S "0.123456789876") (BigDecimal.round HalfFloor 12 (S "0.1234567898765"))
        assertEq (S "-0.123456789877") (BigDecimal.round HalfFloor 12 (S "-0.1234567898765"))

        assertEq (S "0") (BigDecimal.round HalfToZero -3 (S "321.123"))
        assertEq (S "140") (BigDecimal.round HalfToZero -1 (S "145"))
        assertEq (S "-2") (BigDecimal.round HalfToZero 0 (S "-2.5"))
        assertEq (S "1.9") (BigDecimal.round HalfToZero 1 (S "1.95"))
        assertEq (S "-1.9") (BigDecimal.round HalfToZero 1 (S "-1.95"))

        assertEq (S "0") (BigDecimal.round HalfFromZero -3 (S "321.123"))
        assertEq (S "150") (BigDecimal.round HalfFromZero -1 (S "145"))
        assertEq (S "-3") (BigDecimal.round HalfFromZero 0 (S "-2.5"))
        assertEq (S "2") (BigDecimal.round HalfFromZero 1 (S "1.95"))
        assertEq (S "-2") (BigDecimal.round HalfFromZero 1 (S "-1.95"))

        assertEq (S "0") (BigDecimal.round HalfEven -3 (S "321.123"))
        assertEq (S "140") (BigDecimal.round HalfEven -1 (S "145"))
        assertEq (S "-2") (BigDecimal.round HalfEven 0 (S "-2.5"))
        assertEq (S "2") (BigDecimal.round HalfEven 1 (S "1.95"))
        assertEq (S "-2") (BigDecimal.round HalfEven 1 (S "-1.95"))
        assertEq (S "0.123456789876") (BigDecimal.round HalfEven 12 (S "0.1234567898765"))
        assertEq (S "-0.123456789876") (BigDecimal.round HalfEven 12 (S "-0.1234567898765")))

    test "large divide regression from upstream" (fun () ->
        assertSomeEq
            (S "355.8714617763535752237966114591019517738921035021887792661748076460636467881768727839301952739175132")
            (BigDecimal.divide (S "125348") (S "352.2283"))
        assertEq
            (S "355.8714617763535752237966114591019517738921035021887792661748076460636467881768727839301952739175132")
            (BigDecimal.divideUnsafe (S "125348") (S "352.2283"))))
