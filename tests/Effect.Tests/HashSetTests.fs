module Effect.Tests.HashSetTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/HashSet.test.ts
//
// Backed by FSharp.Core's `Set`, so the custom-`Equal` scenario is ported with
// an F# record value (structural equality/hashing for free). The `isHashSet`
// runtime guard is omitted (see HashSet.fs doc comment).

// --- constructors ---

[<Fact>]
let ``empty creates an empty set`` () =
    let set = HashSet.empty<string>
    Assert.Equal(0, HashSet.size set)
    Assert.True(HashSet.isEmpty set)

[<Fact>]
let ``make creates a set from distinct values`` () =
    let set = HashSet.make [ "a"; "b"; "c" ]
    Assert.Equal(3, HashSet.size set)
    Assert.True(HashSet.has set "a")
    Assert.True(HashSet.has set "b")
    Assert.True(HashSet.has set "c")
    Assert.False(HashSet.has set "d")

[<Fact>]
let ``make removes duplicate values`` () =
    let set = HashSet.make [ "a"; "b"; "a"; "c"; "b" ]
    Assert.Equal(3, HashSet.size set)
    Assert.True(HashSet.has set "a")
    Assert.True(HashSet.has set "b")
    Assert.True(HashSet.has set "c")

[<Fact>]
let ``fromIterable removes duplicates from an iterable`` () =
    let set = HashSet.fromIterable [ "a"; "b"; "c"; "b"; "a" ]
    Assert.Equal(3, HashSet.size set)

// --- basic operations ---

[<Fact>]
let ``add returns a new set without mutating the original`` () =
    let original = HashSet.make [ "a"; "b" ]
    let updated = HashSet.add original "c"
    Assert.Equal(2, HashSet.size original)
    Assert.False(HashSet.has original "c")
    Assert.Equal(3, HashSet.size updated)
    Assert.True(HashSet.has updated "c")

[<Fact>]
let ``add returns the same instance when the value already exists`` () =
    let original = HashSet.make [ "a"; "b" ]
    let same = HashSet.add original "a"
    Assert.Equal(2, HashSet.size same)
    Assert.Same(box original, box same)

[<Fact>]
let ``remove returns a new set without mutating the original`` () =
    let original = HashSet.make [ "a"; "b"; "c" ]
    let updated = HashSet.remove original "b"
    Assert.Equal(3, HashSet.size original)
    Assert.True(HashSet.has original "b")
    Assert.Equal(2, HashSet.size updated)
    Assert.False(HashSet.has updated "b")

[<Fact>]
let ``remove returns the same instance when the value is absent`` () =
    let original = HashSet.make [ "a"; "b" ]
    let same = HashSet.remove original "c"
    Assert.Equal(2, HashSet.size same)
    Assert.Same(box original, box same)

[<Fact>]
let ``size and isEmpty reflect cardinality`` () =
    Assert.True(HashSet.isEmpty (HashSet.empty<string>))
    Assert.Equal(1, HashSet.size (HashSet.make [ "a" ]))
    Assert.Equal(3, HashSet.size (HashSet.make [ "a"; "b"; "c" ]))

// --- set operations ---

[<Fact>]
let ``union keeps values from both sets`` () =
    let result = HashSet.union (HashSet.make [ "a"; "b" ]) (HashSet.make [ "b"; "c" ])
    Assert.Equal(3, HashSet.size result)
    Assert.True(HashSet.has result "a")
    Assert.True(HashSet.has result "b")
    Assert.True(HashSet.has result "c")

[<Fact>]
let ``intersection keeps only shared values`` () =
    let result =
        HashSet.intersection (HashSet.make [ "a"; "b"; "c" ]) (HashSet.make [ "b"; "c"; "d" ])

    Assert.Equal(2, HashSet.size result)
    Assert.True(HashSet.has result "b")
    Assert.True(HashSet.has result "c")
    Assert.False(HashSet.has result "a")

[<Fact>]
let ``difference removes values found in the second set`` () =
    let result =
        HashSet.difference (HashSet.make [ "a"; "b"; "c" ]) (HashSet.make [ "b"; "d" ])

    Assert.Equal(2, HashSet.size result)
    Assert.True(HashSet.has result "a")
    Assert.True(HashSet.has result "c")
    Assert.False(HashSet.has result "b")

[<Fact>]
let ``isSubset checks containment`` () =
    let small = HashSet.make [ "a"; "b" ]
    let large = HashSet.make [ "a"; "b"; "c"; "d" ]
    let other = HashSet.make [ "x"; "y" ]
    Assert.True(HashSet.isSubset small large)
    Assert.False(HashSet.isSubset large small)
    Assert.False(HashSet.isSubset small other)
    Assert.True(HashSet.isSubset small small)

// --- functional operations ---

[<Fact>]
let ``map transforms every value`` () =
    let doubled = HashSet.map (HashSet.make [ 1; 2; 3 ]) (fun n -> n * 2)
    Assert.Equal(3, HashSet.size doubled)
    Assert.True(HashSet.has doubled 2)
    Assert.True(HashSet.has doubled 4)
    Assert.True(HashSet.has doubled 6)

[<Fact>]
let ``map removes duplicate transformed values`` () =
    let lengths =
        HashSet.map (HashSet.make [ "apple"; "banana"; "cherry" ]) String.length

    Assert.Equal(2, HashSet.size lengths) // apple=5, banana=cherry=6
    Assert.True(HashSet.has lengths 5)
    Assert.True(HashSet.has lengths 6)

[<Fact>]
let ``filter keeps matching values`` () =
    let evens = HashSet.filter (HashSet.make [ 1; 2; 3; 4; 5; 6 ]) (fun n -> n % 2 = 0)
    Assert.Equal(3, HashSet.size evens)
    Assert.True(HashSet.has evens 2)
    Assert.False(HashSet.has evens 1)

[<Fact>]
let ``some tests for any match`` () =
    let numbers = HashSet.make [ 1; 2; 3; 4; 5 ]
    Assert.True(HashSet.some numbers (fun n -> n > 3))
    Assert.False(HashSet.some numbers (fun n -> n > 10))
    Assert.False(HashSet.some (HashSet.empty<int>) (fun n -> n > 0))

[<Fact>]
let ``every tests for all match`` () =
    let evens = HashSet.make [ 2; 4; 6; 8 ]
    Assert.True(HashSet.every evens (fun n -> n % 2 = 0))
    Assert.False(HashSet.every evens (fun n -> n > 5))
    Assert.True(HashSet.every (HashSet.empty<int>) (fun n -> n > 0)) // vacuously true

[<Fact>]
let ``reduce folds every value`` () =
    let numbers = HashSet.make [ 1; 2; 3; 4; 5 ]
    Assert.Equal(15, HashSet.reduce numbers 0 (fun acc n -> acc + n))
    Assert.Equal(0, HashSet.reduce (HashSet.empty<int>) 0 (fun acc n -> acc + n))

// --- iteration ---

[<Fact>]
let ``toSeq and toList expose the values`` () =
    let set = HashSet.make [ "a"; "b"; "c" ]
    Assert.Equal<string list>([ "a"; "b"; "c" ], HashSet.toSeq set |> List.ofSeq |> List.sort)
    Assert.Equal<string list>([ "a"; "b"; "c" ], HashSet.toList set |> List.sort)

// --- equality and hashing ---

[<Fact>]
let ``structural equality ignores insertion order`` () =
    let set1 = HashSet.make [ "a"; "b"; "c" ]
    let set2 = HashSet.make [ "c"; "b"; "a" ]
    let set3 = HashSet.make [ "a"; "b"; "d" ]
    Assert.True((set1 = set2))
    Assert.False((set1 = set3))

[<Fact>]
let ``hash is consistent for equal sets`` () =
    let set1 = HashSet.make [ "a"; "b"; "c" ]
    let set2 = HashSet.make [ "c"; "b"; "a" ]
    Assert.Equal(hash set1, hash set2)

// --- structural (Equal-style) values ---

type private Person = { Id: string; Name: string }

[<Fact>]
let ``structurally equal record values dedupe and match`` () =
    let alice1 = { Id = "1"; Name = "Alice" }
    let alice2 = { Id = "1"; Name = "Alice" }
    let bob = { Id = "2"; Name = "Bob" }

    let people = HashSet.make [ alice1; bob ]
    Assert.True(HashSet.has people alice2)
    Assert.Equal(2, HashSet.size people)

    let withDuplicate = HashSet.add people alice2
    Assert.Equal(2, HashSet.size withDuplicate)
