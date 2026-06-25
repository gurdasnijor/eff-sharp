module EquivalenceSpec

open Effect
open Effect.Vitest

describe "Equivalence" (fun () ->
    test "String" (fun () ->
        toBe (Equivalence.String "a" "a") true
        toBe (Equivalence.String "a" "b") false)

    test "Number treats NaN as equal to itself" (fun () ->
        toBe (Equivalence.Number 1.0 1.0) true
        toBe (Equivalence.Number 1.0 2.0) false
        toBe (Equivalence.Number nan nan) true
        toBe (Equivalence.Number nan 1.0) false
        toBe (Equivalence.Number 1.0 nan) false
        toBe (Equivalence.Number 0.0 -0.0) true)

    test "Boolean" (fun () ->
        toBe (Equivalence.Boolean true true) true
        toBe (Equivalence.Boolean false false) true
        toBe (Equivalence.Boolean true false) false
        toBe (Equivalence.Boolean false true) false)

    test "BigInt" (fun () ->
        toBe (Equivalence.BigInt 1I 1I) true
        toBe (Equivalence.BigInt 1I 2I) false)

    test "mapInput derives equivalence from a field" (fun () ->
        let eqPerson =
            Equivalence.strictEqual<string> ()
            |> Equivalence.mapInput (fun (name, _age) -> name)

        toBe (eqPerson ("a", 1) ("a", 2)) true
        toBe (eqPerson ("a", 1) ("a", 1)) true
        toBe (eqPerson ("a", 1) ("b", 1)) false
        toBe (eqPerson ("a", 1) ("b", 2)) false)

    test "combine ANDs two equivalences" (fun () ->
        let e0 =
            Equivalence.strictEqual<string> () |> Equivalence.mapInput (fun (a, _, _) -> a)

        let e1 =
            Equivalence.strictEqual<int> () |> Equivalence.mapInput (fun (_, b, _) -> b)

        let eq = Equivalence.combine e1 e0
        toBe (eq ("a", 1, true) ("a", 1, true)) true
        toBe (eq ("a", 1, true) ("a", 1, false)) true
        toBe (eq ("a", 1, true) ("b", 1, true)) false
        toBe (eq ("a", 1, true) ("a", 2, false)) false)

    test "combineAll ANDs many equivalences" (fun () ->
        let e0 =
            Equivalence.strictEqual<string> () |> Equivalence.mapInput (fun (a, _, _) -> a)

        let e1 =
            Equivalence.strictEqual<int> () |> Equivalence.mapInput (fun (_, b, _) -> b)

        let e2 =
            Equivalence.strictEqual<bool> () |> Equivalence.mapInput (fun (_, _, c) -> c)

        let eq = Equivalence.combineAll [ e0; e1; e2 ]
        toBe (eq ("a", 1, true) ("a", 1, true)) true
        toBe (eq ("a", 1, true) ("b", 1, true)) false
        toBe (eq ("a", 1, true) ("a", 2, true)) false
        toBe (eq ("a", 1, true) ("a", 1, false)) false)

    test "combineAll empty is always true" (fun () ->
        let eq = Equivalence.combineAll ([]: Equivalence<int> list)
        toBe (eq 1 2) true)

    test "tuple2" (fun () ->
        let eq =
            Equivalence.tuple2 (Equivalence.strictEqual<string> ()) (Equivalence.strictEqual<int> ())

        toBe (eq ("a", 1) ("a", 1)) true
        toBe (eq ("a", 1) ("c", 1)) false
        toBe (eq ("a", 1) ("a", 2)) false)

    test "Array compares element-wise with length" (fun () ->
        let eq = Equivalence.Array(Equivalence.strictEqual<int> ())
        toBe (eq [] []) true
        toBe (eq [ 1; 2; 3 ] [ 1; 2; 3 ]) true
        toBe (eq [ 1; 2; 3 ] [ 1; 2; 4 ]) false
        toBe (eq [ 1; 2; 3 ] [ 1; 2 ]) false)

    test "record compares same key set and values" (fun () ->
        let eq = Equivalence.record (Equivalence.strictEqual<int> ())
        let m1 = Map [ "a", 1; "b", 2 ]
        toBe (eq m1 (Map [ "a", 1; "b", 2 ])) true
        toBe (eq m1 (Map [ "a", 1; "b", 3 ])) false
        toBe (eq m1 (Map [ "a", 1 ])) false
        toBe (eq (Map [ "a", 1 ]) m1) false)

    test "Date compares by timestamp" (fun () ->
        let d1 = System.DateTime(2020, 1, 1, 0, 0, 0, System.DateTimeKind.Utc)
        let d2 = System.DateTime(2020, 1, 1, 0, 0, 0, System.DateTimeKind.Utc)
        let d3 = System.DateTime(2021, 1, 1, 0, 0, 0, System.DateTimeKind.Utc)
        toBe (Equivalence.Date d1 d2) true
        toBe (Equivalence.Date d1 d3) false
        toBe (Equivalence.Date d1 d1) true))

