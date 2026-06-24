module Effect.Tests.TrieTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/Trie.test.ts
//
// Behaviour, not JS phrasing. `Equal.equals` reduces to FSharp.Core structural
// equality on the backing `Map`. Tests asserting on `toJSON`/`inspect` (JS
// serialization) are omitted; `toString` is ported via `Trie.render`.

[<Fact>]
let ``toString renders entries in iteration order`` () =
    let trie = Trie.empty<int> |> Trie.insert "a" 0 |> Trie.insert "b" 1
    Assert.Equal("Trie([[\"a\",0],[\"b\",1]])", Trie.render trie)

[<Fact>]
let ``iterates no entries for an empty trie`` () =
    let trie = Trie.empty<string>
    Assert.Equal(0, Trie.size trie)
    Assert.Equal<(string * string)[]>([||], Trie.toEntries trie)

[<Fact>]
let ``insert returns new tries and orders entries by key`` () =
    let trie1 = Trie.empty<int> |> Trie.insert "call" 0
    let trie2 = trie1 |> Trie.insert "me" 1
    let trie3 = trie2 |> Trie.insert "mind" 2
    let trie4 = trie3 |> Trie.insert "mid" 3
    Assert.Equal<(string * int)[]>([| ("call", 0) |], Trie.toEntries trie1)
    Assert.Equal<(string * int)[]>([| ("call", 0); ("me", 1) |], Trie.toEntries trie2)
    Assert.Equal<(string * int)[]>([| ("call", 0); ("me", 1); ("mind", 2) |], Trie.toEntries trie3)
    Assert.Equal<(string * int)[]>([| ("call", 0); ("me", 1); ("mid", 3); ("mind", 2) |], Trie.toEntries trie4)

[<Fact>]
let ``fromIterable preserves an empty iterable`` () =
    let trie = Trie.fromIterable ([]: (string * int) list)
    Assert.Equal<(string * int)[]>([||], Trie.toEntries trie)

[<Fact>]
let ``make builds a trie from entry pairs`` () =
    let trie = Trie.make [ ("ca", 0); ("me", 1) ]
    Assert.Equal<(string * int)[]>([| ("ca", 0); ("me", 1) |], Trie.toEntries trie)
    Assert.True(Trie.fromIterable [ ("ca", 0); ("me", 1) ] = trie)

[<Fact>]
let ``fromIterable builds the same trie as make`` () =
    let iterable = [ ("ca", 0); ("me", 1) ]
    let trie = Trie.fromIterable iterable
    Assert.Equal<(string * int)[]>([| ("ca", 0); ("me", 1) |], Trie.toEntries trie)
    Assert.True(Trie.make [ ("ca", 0); ("me", 1) ] = trie)

[<Fact>]
let ``fromIterable orders entries by key`` () =
    let trie = Trie.fromIterable [ ("call", 0); ("me", 1); ("mind", 2); ("mid", 3) ]
    Assert.Equal<(string * int)[]>([| ("call", 0); ("me", 1); ("mid", 3); ("mind", 2) |], Trie.toEntries trie)

[<Fact>]
let ``fromIterable orders keys when one key is a prefix`` () =
    let trie = Trie.fromIterable [ ("shells", 0); ("she", 1) ]
    Assert.Equal<(string * int)[]>([| ("she", 1); ("shells", 0) |], Trie.toEntries trie)

[<Fact>]
let ``size counts stored keys`` () =
    let trie = Trie.empty<int> |> Trie.insert "a" 0 |> Trie.insert "b" 1
    Assert.Equal(2, Trie.size trie)

[<Fact>]
let ``isEmpty distinguishes empty and populated tries`` () =
    let trie = Trie.empty<int>
    let trie1 = trie |> Trie.insert "ma" 0
    Assert.True(Trie.isEmpty trie)
    Assert.False(Trie.isEmpty trie1)

[<Fact>]
let ``get returns Some only for exact keys`` () =
    let trie =
        Trie.empty<int>
        |> Trie.insert "call" 0
        |> Trie.insert "me" 1
        |> Trie.insert "mind" 2
        |> Trie.insert "mid" 3
    Assert.Equal(Some 0, Trie.get trie "call")
    Assert.Equal(Some 1, Trie.get trie "me")
    Assert.Equal(Some 2, Trie.get trie "mind")
    Assert.Equal(Some 3, Trie.get trie "mid")
    Assert.Equal(None, Trie.get trie "cale")
    Assert.Equal(None, Trie.get trie "ma")
    Assert.Equal(None, Trie.get trie "midn")
    Assert.Equal(None, Trie.get trie "mea")

[<Fact>]
let ``get distinguishes complete keys from prefixes`` () =
    let trie = Trie.empty<int> |> Trie.insert "shells" 0 |> Trie.insert "sells" 1 |> Trie.insert "she" 2
    Assert.Equal(None, Trie.get trie "sell")
    Assert.Equal(Some 1, Trie.get trie "sells")
    Assert.Equal(None, Trie.get trie "shell")
    Assert.Equal(Some 2, Trie.get trie "she")

[<Fact>]
let ``has returns true only for exact keys`` () =
    let trie =
        Trie.empty<int>
        |> Trie.insert "call" 0
        |> Trie.insert "me" 1
        |> Trie.insert "mind" 2
        |> Trie.insert "mid" 3
    Assert.True(Trie.has trie "call")
    Assert.True(Trie.has trie "mid")
    Assert.False(Trie.has trie "cale")
    Assert.False(Trie.has trie "ma")
    Assert.False(Trie.has trie "midn")
    Assert.False(Trie.has trie "mea")

[<Fact>]
let ``getUnsafe returns the value or throws when the key is missing`` () =
    let trie = Trie.empty<int> |> Trie.insert "call" 0 |> Trie.insert "me" 1
    Assert.Equal(0, Trie.getUnsafe trie "call")
    Assert.Throws<System.Exception>(fun () -> Trie.getUnsafe trie "mae" |> ignore) |> ignore

[<Fact>]
let ``remove deletes a key without mutating the original trie`` () =
    let trie =
        Trie.empty<int>
        |> Trie.insert "call" 0
        |> Trie.insert "me" 1
        |> Trie.insert "mind" 2
        |> Trie.insert "mid" 3
    let trie1 = trie |> Trie.remove "call"
    let trie2 = trie1 |> Trie.remove "mea"
    Assert.Equal(Some 0, Trie.get trie "call")
    Assert.Equal(None, Trie.get trie1 "call")
    Assert.Equal(None, Trie.get trie2 "call")
    Assert.Equal<(string * int)[]>([| ("call", 0); ("me", 1); ("mid", 3); ("mind", 2) |], Trie.toEntries trie)
    Assert.Equal<(string * int)[]>([| ("me", 1); ("mid", 3); ("mind", 2) |], Trie.toEntries trie1)
    Assert.Equal<(string * int)[]>([| ("me", 1); ("mid", 3); ("mind", 2) |], Trie.toEntries trie2)

[<Fact>]
let ``keys returns keys in sorted order`` () =
    let trie = Trie.empty<int> |> Trie.insert "cab" 0 |> Trie.insert "abc" 1 |> Trie.insert "bca" 2
    Assert.Equal<string[]>([| "abc"; "bca"; "cab" |], Seq.toArray (Trie.keys trie))

[<Fact>]
let ``keys orders nested prefixes alphabetically`` () =
    let trie =
        Trie.make
            [ ("abc", 0); ("bac", 0); ("b", 0); ("ca", 0); ("cac", 0); ("c", 0); ("abb", 0)
              ("ba", 0); ("a", 0); ("bca", 0); ("cab", 0); ("dca", 0); ("ab", 0); ("adc", 0) ]
    Assert.Equal<string[]>(
        [| "a"; "ab"; "abb"; "abc"; "adc"; "b"; "ba"; "bac"; "bca"; "c"; "ca"; "cab"; "cac"; "dca" |],
        Seq.toArray (Trie.keys trie))

[<Fact>]
let ``values follow key iteration order`` () =
    let trie = Trie.empty<int> |> Trie.insert "call" 0 |> Trie.insert "me" 1 |> Trie.insert "and" 2
    Assert.Equal<int[]>([| 2; 0; 1 |], Seq.toArray (Trie.values trie))

[<Fact>]
let ``entries iterates key-value pairs in sorted order`` () =
    let trie = Trie.empty<int> |> Trie.insert "call" 0 |> Trie.insert "me" 1
    Assert.Equal<(string * int)[]>([| ("call", 0); ("me", 1) |], Seq.toArray (Trie.entries trie))

[<Fact>]
let ``toEntries returns a sorted entry array`` () =
    let trie = Trie.empty<int> |> Trie.insert "call" 0 |> Trie.insert "me" 1
    Assert.Equal<(string * int)[]>([| ("call", 0); ("me", 1) |], Trie.toEntries trie)

[<Fact>]
let ``keysWithPrefix returns matching keys in sorted order`` () =
    let trie =
        Trie.empty<int>
        |> Trie.insert "she" 0
        |> Trie.insert "shells" 1
        |> Trie.insert "sea" 2
        |> Trie.insert "sells" 3
        |> Trie.insert "by" 4
        |> Trie.insert "the" 5
        |> Trie.insert "sea" 6
        |> Trie.insert "shore" 7
    Assert.Equal<string[]>([| "she"; "shells" |], Seq.toArray (Trie.keysWithPrefix trie "she"))

[<Fact>]
let ``valuesWithPrefix returns matching values in key order`` () =
    let trie = Trie.empty<int> |> Trie.insert "shells" 0 |> Trie.insert "sells" 1 |> Trie.insert "sea" 2 |> Trie.insert "she" 3
    Assert.Equal<int[]>([| 3; 0 |], Seq.toArray (Trie.valuesWithPrefix trie "she"))

[<Fact>]
let ``entriesWithPrefix returns matching entries in key order`` () =
    let trie = Trie.empty<int> |> Trie.insert "shells" 0 |> Trie.insert "sells" 1 |> Trie.insert "sea" 2 |> Trie.insert "she" 3
    Assert.Equal<(string * int)[]>([| ("she", 3); ("shells", 0) |], Seq.toArray (Trie.entriesWithPrefix trie "she"))

[<Fact>]
let ``toEntriesWithPrefix returns matching entries as an array`` () =
    let trie = Trie.empty<int> |> Trie.insert "shells" 0 |> Trie.insert "sells" 1 |> Trie.insert "sea" 2 |> Trie.insert "she" 3
    Assert.Equal<(string * int)[]>([| ("she", 3); ("shells", 0) |], Trie.toEntriesWithPrefix trie "she")

[<Fact>]
let ``longestPrefixOf returns the longest stored key that prefixes the input`` () =
    let trie = Trie.empty<int> |> Trie.insert "shells" 0 |> Trie.insert "sells" 1 |> Trie.insert "she" 2
    Assert.Equal(None, Trie.longestPrefixOf trie "sell")
    Assert.Equal(Some("sells", 1), Trie.longestPrefixOf trie "sells")
    Assert.Equal(Some("she", 2), Trie.longestPrefixOf trie "shell")
    Assert.Equal(Some("shells", 0), Trie.longestPrefixOf trie "shellsort")

[<Fact>]
let ``map transforms values and can use keys`` () =
    let trie = Trie.empty<int> |> Trie.insert "shells" 0 |> Trie.insert "sells" 1 |> Trie.insert "she" 2
    let trieMapV = Trie.empty<int> |> Trie.insert "shells" 1 |> Trie.insert "sells" 2 |> Trie.insert "she" 3
    let trieMapK = Trie.empty<int> |> Trie.insert "shells" 6 |> Trie.insert "sells" 5 |> Trie.insert "she" 3
    Assert.True(Trie.map trie (fun v _ -> v + 1) = trieMapV)
    Assert.True(Trie.map trie (fun _ k -> k.Length) = trieMapK)

[<Fact>]
let ``filter keeps entries by value or key`` () =
    let trie = Trie.empty<int> |> Trie.insert "shells" 0 |> Trie.insert "sells" 1 |> Trie.insert "she" 2
    let trieMapV = Trie.empty<int> |> Trie.insert "she" 2
    let trieMapK = Trie.empty<int> |> Trie.insert "shells" 0 |> Trie.insert "sells" 1
    Assert.True(Trie.filter trie (fun v _ -> v > 1) = trieMapV)
    Assert.True(Trie.filter trie (fun _ k -> k.Length > 3) = trieMapK)

[<Fact>]
let ``filterMap keeps Result successes and can use keys`` () =
    let trie = Trie.empty<int> |> Trie.insert "shells" 0 |> Trie.insert "sells" 1 |> Trie.insert "she" 2
    let trieMapV = Trie.empty<int> |> Trie.insert "she" 2
    let trieMapK = Trie.empty<int> |> Trie.insert "shells" 0 |> Trie.insert "sells" 1
    Assert.True(Trie.filterMap trie (fun v _ -> if v > 1 then Ok v else Error()) = trieMapV)
    Assert.True(Trie.filterMap trie (fun v k -> if k.Length > 3 then Ok v else Error()) = trieMapK)

[<Fact>]
let ``compact keeps Some values`` () =
    let trie = Trie.empty<int option> |> Trie.insert "shells" (Some 0) |> Trie.insert "sells" None |> Trie.insert "she" (Some 2)
    let trieMapV = Trie.empty<int> |> Trie.insert "shells" 0 |> Trie.insert "she" 2
    Assert.True(Trie.compact trie = trieMapV)

[<Fact>]
let ``modify updates an existing key and ignores a missing key`` () =
    let trie = Trie.empty<int> |> Trie.insert "shells" 0 |> Trie.insert "sells" 1 |> Trie.insert "she" 2
    Assert.Equal(Some 12, Trie.get (trie |> Trie.modify "she" (fun v -> v + 10)) "she")
    Assert.True((trie |> Trie.modify "me" id) = trie)

[<Fact>]
let ``removeMany deletes all matching keys`` () =
    let trie = Trie.empty<int> |> Trie.insert "shells" 0 |> Trie.insert "sells" 1 |> Trie.insert "she" 2
    let expected = Trie.empty<int> |> Trie.insert "shells" 0
    Assert.True((trie |> Trie.removeMany [ "she"; "sells" ]) = expected)

[<Fact>]
let ``insertMany inserts all entries`` () =
    let trie = Trie.empty<int> |> Trie.insert "shells" 0 |> Trie.insert "sells" 1 |> Trie.insert "she" 2
    let trieInsert = Trie.empty<int> |> Trie.insert "shells" 0 |> Trie.insertMany [ ("sells", 1); ("she", 2) ]
    Assert.True((trie = trieInsert))

[<Fact>]
let ``reduce visits entries in key order`` () =
    let trie = Trie.empty<int> |> Trie.insert "shells" 0 |> Trie.insert "sells" 1 |> Trie.insert "she" 2
    Assert.Equal(3, trie |> Trie.reduce 0 (fun acc n _ -> acc + n))
    Assert.Equal(13, trie |> Trie.reduce 10 (fun acc n _ -> acc + n))
    Assert.Equal("sellssheshells", trie |> Trie.reduce "" (fun acc _ key -> acc + key))

[<Fact>]
let ``forEach`` () =
    let mutable value = 0
    Trie.empty<int>
    |> Trie.insert "shells" 0
    |> Trie.insert "sells" 1
    |> Trie.insert "she" 2
    |> Trie.forEach (fun n key -> value <- value + n + key.Length)
    Assert.Equal(17, value)

[<Fact>]
let ``Equal compares tries by entries`` () =
    Assert.True(Trie.empty<int> = Trie.empty<int>)
    Assert.True(Trie.make [ ("call", 0); ("me", 1) ] = Trie.make [ ("call", 0); ("me", 1) ])
