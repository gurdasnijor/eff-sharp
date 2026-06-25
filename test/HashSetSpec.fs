module HashSetSpec

open Effect
open Effect.Vitest

type private Person = { Id: string; Name: string }

describe "HashSet" (fun () ->
    test "empty creates an empty set" (fun () ->
        let set = HashSet.empty<string>
        toBe (HashSet.size set) 0
        toBe (HashSet.isEmpty set) true)

    test "make creates a set from distinct values" (fun () ->
        let set = HashSet.make [ "a"; "b"; "c" ]
        toBe (HashSet.size set) 3
        toBe (HashSet.has set "a") true
        toBe (HashSet.has set "b") true
        toBe (HashSet.has set "c") true
        toBe (HashSet.has set "d") false)

    test "make removes duplicate values" (fun () ->
        let set = HashSet.make [ "a"; "b"; "a"; "c"; "b" ]
        toBe (HashSet.size set) 3
        toBe (HashSet.has set "a") true
        toBe (HashSet.has set "b") true
        toBe (HashSet.has set "c") true)

    test "fromIterable removes duplicates from an iterable" (fun () ->
        let set = HashSet.fromIterable [ "a"; "b"; "c"; "b"; "a" ]
        toBe (HashSet.size set) 3)

    test "add returns a new set without mutating the original" (fun () ->
        let original = HashSet.make [ "a"; "b" ]
        let updated = HashSet.add original "c"
        toBe (HashSet.size original) 2
        toBe (HashSet.has original "c") false
        toBe (HashSet.size updated) 3
        toBe (HashSet.has updated "c") true)

    test "add returns the same instance when the value already exists" (fun () ->
        let original = HashSet.make [ "a"; "b" ]
        let same = HashSet.add original "a"
        toBe (HashSet.size same) 2
        toBe same original)

    test "remove returns a new set without mutating the original" (fun () ->
        let original = HashSet.make [ "a"; "b"; "c" ]
        let updated = HashSet.remove original "b"
        toBe (HashSet.size original) 3
        toBe (HashSet.has original "b") true
        toBe (HashSet.size updated) 2
        toBe (HashSet.has updated "b") false)

    test "remove returns the same instance when the value is absent" (fun () ->
        let original = HashSet.make [ "a"; "b" ]
        let same = HashSet.remove original "c"
        toBe (HashSet.size same) 2
        toBe same original)

    test "size and isEmpty reflect cardinality" (fun () ->
        toBe (HashSet.isEmpty (HashSet.empty<string>)) true
        toBe (HashSet.size (HashSet.make [ "a" ])) 1
        toBe (HashSet.size (HashSet.make [ "a"; "b"; "c" ])) 3)

    test "union keeps values from both sets" (fun () ->
        let result = HashSet.union (HashSet.make [ "a"; "b" ]) (HashSet.make [ "b"; "c" ])
        toBe (HashSet.size result) 3
        toBe (HashSet.has result "a") true
        toBe (HashSet.has result "b") true
        toBe (HashSet.has result "c") true)

    test "intersection keeps only shared values" (fun () ->
        let result =
            HashSet.intersection (HashSet.make [ "a"; "b"; "c" ]) (HashSet.make [ "b"; "c"; "d" ])

        toBe (HashSet.size result) 2
        toBe (HashSet.has result "b") true
        toBe (HashSet.has result "c") true
        toBe (HashSet.has result "a") false)

    test "difference removes values found in the second set" (fun () ->
        let result =
            HashSet.difference (HashSet.make [ "a"; "b"; "c" ]) (HashSet.make [ "b"; "d" ])

        toBe (HashSet.size result) 2
        toBe (HashSet.has result "a") true
        toBe (HashSet.has result "c") true
        toBe (HashSet.has result "b") false)

    test "isSubset checks containment" (fun () ->
        let small = HashSet.make [ "a"; "b" ]
        let large = HashSet.make [ "a"; "b"; "c"; "d" ]
        let other = HashSet.make [ "x"; "y" ]
        toBe (HashSet.isSubset small large) true
        toBe (HashSet.isSubset large small) false
        toBe (HashSet.isSubset small other) false
        toBe (HashSet.isSubset small small) true)

    test "map transforms every value" (fun () ->
        let doubled = HashSet.map (HashSet.make [ 1; 2; 3 ]) (fun n -> n * 2)
        toBe (HashSet.size doubled) 3
        toBe (HashSet.has doubled 2) true
        toBe (HashSet.has doubled 4) true
        toBe (HashSet.has doubled 6) true)

    test "map removes duplicate transformed values" (fun () ->
        let lengths =
            HashSet.map (HashSet.make [ "apple"; "banana"; "cherry" ]) String.length

        toBe (HashSet.size lengths) 2
        toBe (HashSet.has lengths 5) true
        toBe (HashSet.has lengths 6) true)

    test "filter keeps matching values" (fun () ->
        let evens = HashSet.filter (HashSet.make [ 1; 2; 3; 4; 5; 6 ]) (fun n -> n % 2 = 0)
        toBe (HashSet.size evens) 3
        toBe (HashSet.has evens 2) true
        toBe (HashSet.has evens 1) false)

    test "some tests for any match" (fun () ->
        let numbers = HashSet.make [ 1; 2; 3; 4; 5 ]
        toBe (HashSet.some numbers (fun n -> n > 3)) true
        toBe (HashSet.some numbers (fun n -> n > 10)) false
        toBe (HashSet.some (HashSet.empty<int>) (fun n -> n > 0)) false)

    test "every tests for all match" (fun () ->
        let evens = HashSet.make [ 2; 4; 6; 8 ]
        toBe (HashSet.every evens (fun n -> n % 2 = 0)) true
        toBe (HashSet.every evens (fun n -> n > 5)) false
        toBe (HashSet.every (HashSet.empty<int>) (fun n -> n > 0)) true)

    test "reduce folds every value" (fun () ->
        let numbers = HashSet.make [ 1; 2; 3; 4; 5 ]
        toBe (HashSet.reduce numbers 0 (fun acc n -> acc + n)) 15
        toBe (HashSet.reduce (HashSet.empty<int>) 0 (fun acc n -> acc + n)) 0)

    test "toSeq and toList expose the values" (fun () ->
        let set = HashSet.make [ "a"; "b"; "c" ]
        toEqual (HashSet.toSeq set |> List.ofSeq |> List.sort) [ "a"; "b"; "c" ]
        toEqual (HashSet.toList set |> List.sort) [ "a"; "b"; "c" ])

    test "structural equality ignores insertion order" (fun () ->
        let set1 = HashSet.make [ "a"; "b"; "c" ]
        let set2 = HashSet.make [ "c"; "b"; "a" ]
        let set3 = HashSet.make [ "a"; "b"; "d" ]
        toBe (set1 = set2) true
        toBe (set1 = set3) false)

    test "hash is consistent for equal sets" (fun () ->
        let set1 = HashSet.make [ "a"; "b"; "c" ]
        let set2 = HashSet.make [ "c"; "b"; "a" ]
        toBe (hash set1) (hash set2))

    test "structurally equal record values dedupe and match" (fun () ->
        let alice1 = { Id = "1"; Name = "Alice" }
        let alice2 = { Id = "1"; Name = "Alice" }
        let bob = { Id = "2"; Name = "Bob" }

        let people = HashSet.make [ alice1; bob ]
        toBe (HashSet.has people alice2) true
        toBe (HashSet.size people) 2

        let withDuplicate = HashSet.add people alice2
        toBe (HashSet.size withDuplicate) 2))

