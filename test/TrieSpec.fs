module TrieSpec

open Effect
open Effect.Vitest

let private same actual expected =
    toBe (actual = expected) true

describe "Trie" (fun () ->
    test "render and empty trie" (fun () ->
        let trie = Trie.empty<int> |> Trie.insert "a" 0 |> Trie.insert "b" 1
        toBe (Trie.render trie) "Trie([[\"a\",0],[\"b\",1]])"
        toBe (Trie.size Trie.empty<string>) 0
        same (Trie.toEntries Trie.empty<string>) [||])

    test "insert returns new tries and orders entries by key" (fun () ->
        let trie1 = Trie.empty<int> |> Trie.insert "call" 0
        let trie2 = trie1 |> Trie.insert "me" 1
        let trie3 = trie2 |> Trie.insert "mind" 2
        let trie4 = trie3 |> Trie.insert "mid" 3
        same (Trie.toEntries trie1) [| ("call", 0) |]
        same (Trie.toEntries trie2) [| ("call", 0); ("me", 1) |]
        same (Trie.toEntries trie3) [| ("call", 0); ("me", 1); ("mind", 2) |]
        same (Trie.toEntries trie4) [| ("call", 0); ("me", 1); ("mid", 3); ("mind", 2) |])

    test "constructors build sorted tries" (fun () ->
        same (Trie.toEntries (Trie.fromIterable ([]: (string * int) list))) [||]
        let trie = Trie.make [ ("ca", 0); ("me", 1) ]
        same (Trie.toEntries trie) [| ("ca", 0); ("me", 1) |]
        same (Trie.fromIterable [ ("ca", 0); ("me", 1) ]) trie
        same
            (Trie.toEntries (Trie.fromIterable [ ("call", 0); ("me", 1); ("mind", 2); ("mid", 3) ]))
            [| ("call", 0); ("me", 1); ("mid", 3); ("mind", 2) |]
        same (Trie.toEntries (Trie.fromIterable [ ("shells", 0); ("she", 1) ])) [| ("she", 1); ("shells", 0) |])

    test "size and emptiness" (fun () ->
        let trie = Trie.empty<int>
        let trie1 = trie |> Trie.insert "ma" 0
        toBe (Trie.size (trie1 |> Trie.insert "b" 1)) 2
        toBe (Trie.isEmpty trie) true
        toBe (Trie.isEmpty trie1) false)

    test "get has and getUnsafe use exact keys" (fun () ->
        let trie =
            Trie.empty<int>
            |> Trie.insert "call" 0
            |> Trie.insert "me" 1
            |> Trie.insert "mind" 2
            |> Trie.insert "mid" 3

        same (Trie.get trie "call") (Some 0)
        same (Trie.get trie "mid") (Some 3)
        same (Trie.get trie "cale") None
        same (Trie.get trie "midn") None
        toBe (Trie.has trie "call") true
        toBe (Trie.has trie "mea") false
        toBe (Trie.getUnsafe trie "call") 0
        toThrow (fun () -> Trie.getUnsafe trie "mae" |> ignore))

    test "complete keys are distinguished from prefixes" (fun () ->
        let trie =
            Trie.empty<int>
            |> Trie.insert "shells" 0
            |> Trie.insert "sells" 1
            |> Trie.insert "she" 2

        same (Trie.get trie "sell") None
        same (Trie.get trie "sells") (Some 1)
        same (Trie.get trie "shell") None
        same (Trie.get trie "she") (Some 2))

    test "remove deletes a key without mutating the original" (fun () ->
        let trie =
            Trie.empty<int>
            |> Trie.insert "call" 0
            |> Trie.insert "me" 1
            |> Trie.insert "mind" 2
            |> Trie.insert "mid" 3

        let trie1 = trie |> Trie.remove "call"
        let trie2 = trie1 |> Trie.remove "mea"
        same (Trie.get trie "call") (Some 0)
        same (Trie.get trie1 "call") None
        same (Trie.get trie2 "call") None
        same (Trie.toEntries trie) [| ("call", 0); ("me", 1); ("mid", 3); ("mind", 2) |]
        same (Trie.toEntries trie1) [| ("me", 1); ("mid", 3); ("mind", 2) |])

    test "keys values and entries iterate in key order" (fun () ->
        let trie =
            Trie.make
                [ ("abc", 1)
                  ("bac", 2)
                  ("b", 3)
                  ("ca", 4)
                  ("abb", 5)
                  ("a", 6) ]

        same (Trie.keys trie |> Seq.toArray) [| "a"; "abb"; "abc"; "b"; "bac"; "ca" |]
        same (Trie.values trie |> Seq.toArray) [| 6; 5; 1; 3; 2; 4 |]
        same
            (Trie.entries trie |> Seq.toArray)
            [| ("a", 6); ("abb", 5); ("abc", 1); ("b", 3); ("bac", 2); ("ca", 4) |]
        same (Trie.toEntries trie) (Trie.entries trie |> Seq.toArray))

    test "prefix queries return sorted matches" (fun () ->
        let trie =
            Trie.empty<int>
            |> Trie.insert "she" 3
            |> Trie.insert "shells" 0
            |> Trie.insert "sea" 2
            |> Trie.insert "sells" 1
            |> Trie.insert "shore" 7

        same (Trie.keysWithPrefix trie "she" |> Seq.toArray) [| "she"; "shells" |]
        same (Trie.valuesWithPrefix trie "she" |> Seq.toArray) [| 3; 0 |]
        same (Trie.entriesWithPrefix trie "she" |> Seq.toArray) [| ("she", 3); ("shells", 0) |]
        same (Trie.toEntriesWithPrefix trie "she") [| ("she", 3); ("shells", 0) |])

    test "longestPrefixOf returns the longest stored key prefix" (fun () ->
        let trie =
            Trie.empty<int>
            |> Trie.insert "shells" 0
            |> Trie.insert "sells" 1
            |> Trie.insert "she" 2

        same (Trie.longestPrefixOf trie "sell") None
        same (Trie.longestPrefixOf trie "sells") (Some("sells", 1))
        same (Trie.longestPrefixOf trie "shell") (Some("she", 2))
        same (Trie.longestPrefixOf trie "shellsort") (Some("shells", 0)))

    test "map filter filterMap and compact transform values" (fun () ->
        let trie =
            Trie.empty<int>
            |> Trie.insert "shells" 0
            |> Trie.insert "sells" 1
            |> Trie.insert "she" 2

        same
            (Trie.map trie (fun v _ -> v + 1))
            (Trie.empty<int> |> Trie.insert "shells" 1 |> Trie.insert "sells" 2 |> Trie.insert "she" 3)
        same
            (Trie.map trie (fun _ k -> k.Length))
            (Trie.empty<int> |> Trie.insert "shells" 6 |> Trie.insert "sells" 5 |> Trie.insert "she" 3)
        same (Trie.filter trie (fun v _ -> v > 1)) (Trie.empty<int> |> Trie.insert "she" 2)
        same
            (Trie.filter trie (fun _ k -> k.Length > 3))
            (Trie.empty<int> |> Trie.insert "shells" 0 |> Trie.insert "sells" 1)
        same
            (Trie.filterMap trie (fun v _ -> if v > 1 then Ok v else Error()))
            (Trie.empty<int> |> Trie.insert "she" 2)

        let optionTrie =
            Trie.empty<int option>
            |> Trie.insert "shells" (Some 0)
            |> Trie.insert "sells" None
            |> Trie.insert "she" (Some 2)

        same (Trie.compact optionTrie) (Trie.empty<int> |> Trie.insert "shells" 0 |> Trie.insert "she" 2))

    test "modify removeMany insertMany reduce and forEach" (fun () ->
        let trie =
            Trie.empty<int>
            |> Trie.insert "shells" 0
            |> Trie.insert "sells" 1
            |> Trie.insert "she" 2

        same (Trie.get (trie |> Trie.modify "she" (fun v -> v + 10)) "she") (Some 12)
        same (trie |> Trie.modify "me" id) trie
        same
            (trie |> Trie.removeMany [ "she"; "sells" ])
            (Trie.empty<int> |> Trie.insert "shells" 0)
        same
            (Trie.empty<int> |> Trie.insert "shells" 0 |> Trie.insertMany [ ("sells", 1); ("she", 2) ])
            trie
        toBe (trie |> Trie.reduce 0 (fun acc n _ -> acc + n)) 3
        toBe (trie |> Trie.reduce "" (fun acc _ key -> acc + key)) "sellssheshells"

        let mutable value = 0
        trie |> Trie.forEach (fun n key -> value <- value + n + key.Length)
        toBe value 17
        same Trie.empty<int> Trie.empty<int>
        same (Trie.make [ ("call", 0); ("me", 1) ]) (Trie.make [ ("call", 0); ("me", 1) ])))
