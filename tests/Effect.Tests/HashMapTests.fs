module Effect.Tests.HashMapTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/HashMap.test.ts
//
// Behavioural tests are ported as-is. Tests that exercise HAMT implementation
// internals (bit-31 indexed-leaf inserts, mergeLeaves, fixed-hash collision
// shapes) are intentionally NOT ported: this port is backed by FSharp.Core's
// `Map`, so those internal trie invariants do not apply. The custom-`Equal`-key
// scenario is ported using an F# record key, which gets structural equality and
// hashing for free. The `isHashMap` runtime guard and the mutation-window
// helpers are omitted (see HashMap.fs doc comment).

// --- constructors ---

[<Fact>]
let ``empty has no entries`` () =
    let map = HashMap.empty<string, int>
    Assert.True(HashMap.isEmpty map)
    Assert.Equal(0, HashMap.size map)

[<Fact>]
let ``make builds a map from pairs`` () =
    let map = HashMap.make [ ("a", 1); ("b", 2); ("c", 3) ]
    Assert.Equal(3, HashMap.size map)
    Assert.Equal(Some 1, HashMap.get map "a")
    Assert.Equal(Some 2, HashMap.get map "b")
    Assert.Equal(Some 3, HashMap.get map "c")

[<Fact>]
let ``fromIterable builds a map from a sequence`` () =
    let map = HashMap.fromIterable (seq { "a", 1; "b", 2; "c", 3 })
    Assert.Equal(3, HashMap.size map)
    Assert.Equal(Some 1, HashMap.get map "a")

// --- basic operations ---

[<Fact>]
let ``get returns None for a missing key`` () =
    let map = HashMap.make [ ("a", 1); ("b", 2) ]
    Assert.Equal(None, HashMap.get map "c")

[<Fact>]
let ``has reflects membership`` () =
    let map = HashMap.make [ ("a", 1); ("b", 2) ]
    Assert.True(HashMap.has map "a")
    Assert.False(HashMap.has map "c")

[<Fact>]
let ``set adds a new key without mutating the original`` () =
    let map1 = HashMap.make [ ("a", 1) ]
    let map2 = HashMap.set map1 "b" 2
    Assert.Equal(2, HashMap.size map2)
    Assert.Equal(Some 2, HashMap.get map2 "b")
    // original unchanged
    Assert.Equal(1, HashMap.size map1)
    Assert.False(HashMap.has map1 "b")

[<Fact>]
let ``set overwrites an existing key without mutating the original`` () =
    let map1 = HashMap.make [ ("a", 1); ("b", 2) ]
    let map2 = HashMap.set map1 "a" 10
    Assert.Equal(Some 10, HashMap.get map2 "a")
    Assert.Equal(Some 1, HashMap.get map1 "a")

[<Fact>]
let ``remove deletes an existing key without mutating the original`` () =
    let map1 = HashMap.make [ ("a", 1); ("b", 2); ("c", 3) ]
    let map2 = HashMap.remove map1 "b"
    Assert.Equal(2, HashMap.size map2)
    Assert.False(HashMap.has map2 "b")
    // original unchanged
    Assert.Equal(3, HashMap.size map1)
    Assert.True(HashMap.has map1 "b")

[<Fact>]
let ``remove of a missing key returns the same instance`` () =
    let map1 = HashMap.make [ ("a", 1); ("b", 2) ]
    let map2 = HashMap.remove map1 "c"
    Assert.Same(box map1, box map2)
    Assert.Equal(2, HashMap.size map2)

[<Fact>]
let ``getUnsafe returns the value or throws`` () =
    let map = HashMap.make [ ("a", 1); ("b", 2) ]
    Assert.Equal(1, HashMap.getUnsafe map "a")
    let ex = Assert.Throws<System.Exception>(fun () -> HashMap.getUnsafe map "z" |> ignore)
    Assert.Contains("HashMap.getUnsafe: key not found", ex.Message)

// --- iterators and getters ---

[<Fact>]
let ``keys values and entries expose the contents`` () =
    let map = HashMap.make [ ("a", 1); ("b", 2); ("c", 3) ]
    Assert.Equal<string list>([ "a"; "b"; "c" ], HashMap.keys map |> List.ofSeq |> List.sort)
    Assert.Equal<int list>([ 1; 2; 3 ], HashMap.values map |> List.ofSeq |> List.sort)
    Assert.Equal<int list>([ 1; 2; 3 ], HashMap.toValues map |> List.sort)
    Assert.Equal<(string * int) list>(
        [ ("a", 1); ("b", 2); ("c", 3) ],
        HashMap.entries map |> List.ofSeq |> List.sort
    )
    Assert.Equal<(string * int) list>([ ("a", 1); ("b", 2); ("c", 3) ], HashMap.toEntries map |> List.sort)

// --- bulk operations ---

[<Fact>]
let ``removeMany drops several keys`` () =
    let map1 = HashMap.make [ ("a", 1); ("b", 2); ("c", 3); ("d", 4) ]
    let map2 = HashMap.removeMany map1 [ "b"; "d" ]
    Assert.Equal(2, HashMap.size map2)
    Assert.True(HashMap.has map2 "a")
    Assert.False(HashMap.has map2 "b")
    Assert.True(HashMap.has map2 "c")
    Assert.False(HashMap.has map2 "d")

[<Fact>]
let ``setMany inserts and overwrites`` () =
    let map1 = HashMap.make [ ("a", 1); ("b", 2) ]
    let map2 = HashMap.setMany map1 [ ("c", 3); ("d", 4); ("a", 10) ]
    Assert.Equal(4, HashMap.size map2)
    Assert.Equal(Some 10, HashMap.get map2 "a")
    Assert.Equal(Some 2, HashMap.get map2 "b")
    Assert.Equal(Some 3, HashMap.get map2 "c")
    Assert.Equal(Some 4, HashMap.get map2 "d")

[<Fact>]
let ``union prefers the second map on collisions`` () =
    let map1 = HashMap.make [ ("a", 1); ("b", 2) ]
    let map2 = HashMap.make [ ("b", 20); ("c", 3) ]
    let map3 = HashMap.union map1 map2
    Assert.Equal(3, HashMap.size map3)
    Assert.Equal(Some 1, HashMap.get map3 "a")
    Assert.Equal(Some 20, HashMap.get map3 "b")
    Assert.Equal(Some 3, HashMap.get map3 "c")

// --- mapping operations ---

[<Fact>]
let ``map transforms values`` () =
    let map1 = HashMap.make [ ("a", 1); ("b", 2); ("c", 3) ]
    let map2 = HashMap.map map1 (fun value _key -> value * 2)
    Assert.Equal(Some 2, HashMap.get map2 "a")
    Assert.Equal(Some 4, HashMap.get map2 "b")
    Assert.Equal(Some 6, HashMap.get map2 "c")

[<Fact>]
let ``flatMap expands and flattens`` () =
    let map1 = HashMap.make [ ("a", 1); ("b", 2) ]
    let map2 = HashMap.flatMap map1 (fun value key -> HashMap.make [ (key + "1", value); (key + "2", value * 2) ])
    Assert.Equal(4, HashMap.size map2)
    Assert.Equal(Some 1, HashMap.get map2 "a1")
    Assert.Equal(Some 2, HashMap.get map2 "a2")
    Assert.Equal(Some 2, HashMap.get map2 "b1")
    Assert.Equal(Some 4, HashMap.get map2 "b2")

// --- filtering operations ---

[<Fact>]
let ``filter keeps matching entries`` () =
    let map1 = HashMap.make [ ("a", 1); ("b", 2); ("c", 3); ("d", 4) ]
    let map2 = HashMap.filter map1 (fun value _key -> value % 2 = 0)
    Assert.Equal(2, HashMap.size map2)
    Assert.True(HashMap.has map2 "b")
    Assert.True(HashMap.has map2 "d")
    Assert.False(HashMap.has map2 "a")

[<Fact>]
let ``compact drops None values`` () =
    let map1 = HashMap.make [ ("a", Some 1); ("b", None); ("c", Some 3) ]
    let map2 = HashMap.compact map1
    Assert.Equal(2, HashMap.size map2)
    Assert.Equal(Some 1, HashMap.get map2 "a")
    Assert.False(HashMap.has map2 "b")
    Assert.Equal(Some 3, HashMap.get map2 "c")

[<Fact>]
let ``filterMap keeps Ok results and can see the key`` () =
    let map1 = HashMap.make [ ("a", 1); ("b", 2); ("c", 3); ("d", 4) ]
    let map2 = HashMap.filterMap map1 (fun value _key -> if value % 2 = 0 then Ok(value * 2) else Error())
    Assert.Equal(2, HashMap.size map2)
    Assert.Equal(Some 4, HashMap.get map2 "b")
    Assert.Equal(Some 8, HashMap.get map2 "d")

    let map3 = HashMap.filterMap map1 (fun value key -> if key < "c" then Ok(sprintf "%s:%d" key value) else Error())
    Assert.Equal(2, HashMap.size map3)
    Assert.Equal(Some "a:1", HashMap.get map3 "a")
    Assert.Equal(Some "b:2", HashMap.get map3 "b")
    Assert.False(HashMap.has map3 "c")

// --- search operations ---

[<Fact>]
let ``findFirst returns a matching entry or None`` () =
    let map = HashMap.make [ ("a", 1); ("b", 2); ("c", 3) ]
    Assert.Equal(Some("c", 3), HashMap.findFirst map (fun value _key -> value = 3))
    Assert.Equal(None, HashMap.findFirst map (fun value _key -> value > 5))

[<Fact>]
let ``some and every test the entries`` () =
    let map = HashMap.make [ ("a", 1); ("b", 2); ("c", 3) ]
    Assert.True(HashMap.some map (fun value _key -> value > 2))
    Assert.False(HashMap.some map (fun value _key -> value > 5))
    Assert.True(HashMap.every map (fun value _key -> value > 0))
    Assert.False(HashMap.every map (fun value _key -> value > 1))

[<Fact>]
let ``hasBy tests key and value together`` () =
    let map = HashMap.make [ ("a", 1); ("b", 2); ("c", 3) ]
    Assert.True(HashMap.hasBy map (fun value key -> key = "b" && value = 2))
    Assert.False(HashMap.hasBy map (fun value key -> key = "b" && value = 5))

// --- modification operations ---

[<Fact>]
let ``modifyAt updates inserts and removes`` () =
    let map1 = HashMap.make [ ("a", 1); ("b", 2) ]
    let updated = HashMap.modifyAt map1 "a" (function Some v -> Some(v * 2) | None -> None)
    Assert.Equal(Some 2, HashMap.get updated "a")

    let inserted = HashMap.modifyAt (HashMap.make [ ("a", 1) ]) "b" (function Some v -> Some v | None -> Some 10)
    Assert.Equal(Some 10, HashMap.get inserted "b")

    let removed = HashMap.modifyAt map1 "a" (fun _ -> None)
    Assert.False(HashMap.has removed "a")
    Assert.True(HashMap.has removed "b")

[<Fact>]
let ``modify updates an existing key`` () =
    let map1 = HashMap.make [ ("a", 1); ("b", 2) ]
    let map2 = HashMap.modify map1 "a" (fun value -> value * 3)
    Assert.Equal(Some 3, HashMap.get map2 "a")
    Assert.Equal(Some 2, HashMap.get map2 "b")

// --- reduction operations ---

[<Fact>]
let ``reduce folds the values`` () =
    let map = HashMap.make [ ("a", 1); ("b", 2); ("c", 3) ]
    Assert.Equal(6, HashMap.reduce map 0 (fun acc value _key -> acc + value))

[<Fact>]
let ``forEach visits every entry`` () =
    let map = HashMap.make [ ("a", 1); ("b", 2) ]
    let collected = System.Collections.Generic.List<string * int>()
    HashMap.forEach map (fun value key -> collected.Add(key, value))
    Assert.Equal<(string * int) list>([ ("a", 1); ("b", 2) ], collected |> List.ofSeq |> List.sort)

// --- equality and hashing (F# structural, order-independent) ---

[<Fact>]
let ``structural equality ignores insertion order`` () =
    let map1 = HashMap.make [ ("a", 1); ("b", 2) ]
    let map2 = HashMap.make [ ("b", 2); ("a", 1) ]
    let map3 = HashMap.make [ ("a", 1); ("b", 3) ]
    Assert.True((map1 = map2))
    Assert.False((map1 = map3))

[<Fact>]
let ``hash is consistent for equal maps`` () =
    let map1 = HashMap.make [ ("a", 1); ("b", 2) ]
    let map2 = HashMap.make [ ("b", 2); ("a", 1) ]
    Assert.Equal(hash map1, hash map2)

// --- structural (Equal-style) keys ---

type private Person = { Name: string; Age: int }

[<Fact>]
let ``structurally equal record keys address the same entry`` () =
    let alice1 = { Name = "Alice"; Age = 25 }
    let alice2 = { Name = "Alice"; Age = 25 } // same data, distinct binding
    let bob = { Name = "Bob"; Age = 30 }

    let map = HashMap.make [ (alice1, "value1"); (bob, "value3") ]
    Assert.Equal(Some "value1", HashMap.get map alice2)
    Assert.True(HashMap.has map alice2)

    let map2 = HashMap.set map alice2 "updated"
    Assert.Equal(Some "updated", HashMap.get map2 alice1)
    Assert.Equal(2, HashMap.size map2)

// --- stress (parity with upstream stress test) ---

[<Fact>]
let ``handles many inserts lookups and removals`` () =
    let mutable map = HashMap.empty<int, string>
    for i in 0..999 do
        map <- HashMap.set map i (sprintf "value%d" i)
    Assert.Equal(1000, HashMap.size map)
    Assert.Equal(Some "value42", HashMap.get map 42)

    for i in 0..499 do
        map <- HashMap.remove map i
    Assert.Equal(500, HashMap.size map)
    Assert.False(HashMap.has map 0)
    Assert.True(HashMap.has map 999)
