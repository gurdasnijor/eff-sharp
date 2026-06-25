module EqualSpec

open Effect
open Effect.Vitest

type private Point = { X: int; Y: int }

describe "Equal" (fun () ->
    test "structurally identical records are equal" (fun () -> toBe (Equal.equals { X = 1; Y = 2 } { X = 1; Y = 2 }) true)

    test "records with different values are not equal" (fun () -> toBe (Equal.equals { X = 1; Y = 2 } { X = 1; Y = 3 }) false)

    test "primitives compare by value" (fun () ->
        toBe (Equal.equals 1 1) true
        toBe (Equal.equals "a" "b") false
        toBe (Equal.equals "a" "a") true)

    test "NaN equals NaN" (fun () ->
        toBe (Equal.equals nan nan) true
        toBe (Equal.equals nan 0.0) false
        toBe (Equal.equals 42.0 nan) false
        toBe (Equal.equals nan System.Double.PositiveInfinity) false)

    test "NaN inside arrays and records" (fun () ->
        toBe (Equal.equals [| nan |] [| nan |]) true
        toBe (Equal.equals [| nan; 1.0 |] [| nan; 1.0 |]) true
        toBe (Equal.equals [| nan; 1.0 |] [| nan; 2.0 |]) false
        toBe (Equal.equals [| nan |] [| System.Double.PositiveInfinity |]) false
        toBe (Equal.equals {| arr = [ nan ] |} {| arr = [ nan ] |}) true
        toBe (Equal.equals {| nested = {| deep = nan |} |} {| nested = {| deep = nan |} |}) true)

    test "lists and tuples compare structurally" (fun () ->
        toBe (Equal.equals [ 1; 2; 3 ] [ 1; 2; 3 ]) true
        toBe (Equal.equals [ 1; 2; 3 ] [ 1; 2; 4 ]) false
        toBe (Equal.equals (1, "a") (1, "a")) true
        toBe (Equal.equals (1, "a") (1, "b")) false)

    test "options compare structurally" (fun () ->
        toBe (Equal.equals (Some 42) (Some 42)) true
        toBe (Equal.equals (Some 42) (Some 43)) false
        toBe (Equal.equals (Some 42) None) false)

    test "maps compare order-independently" (fun () ->
        let m1 = Map [ "a", 1; "b", 2 ]
        let m2 = Map [ "b", 2; "a", 1 ]
        toBe (Equal.equals m1 m2) true
        toBe (Equal.equals m1 (Map [ "a", 1; "b", 3 ])) false
        toBe (Equal.equals (Map<string, int> []) (Map<string, int> [])) true)

    test "sets compare order-independently" (fun () ->
        toBe (Equal.equals (Set [ 1; 2; 3 ]) (Set [ 3; 2; 1 ])) true
        toBe (Equal.equals (Set [ 1; 2; 3 ]) (Set [ 1; 2 ])) false)

    test "dates compare by value" (fun () ->
        let d1 = System.DateTime(2023, 1, 1)
        let d2 = System.DateTime(2023, 1, 1)
        let d3 = System.DateTime(2023, 1, 2)
        toBe (Equal.equals d1 d2) true
        toBe (Equal.equals d1 d3) false)

    test "asEquivalence wraps equals" (fun () ->
        let eq = Equal.asEquivalence<Point> ()
        toBe (eq { X = 1; Y = 2 } { X = 1; Y = 2 }) true
        toBe (eq { X = 1; Y = 2 } { X = 2; Y = 1 }) false))

