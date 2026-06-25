module ChunkSpec

open Effect
open Effect.Vitest

let private same actual expected = toBe (actual = expected) true

describe "Chunk" (fun () ->
    test "structural equality" (fun () ->
        toBe (Chunk.make [ 0 ] = Chunk.make [ 0 ]) true
        toBe (Chunk.make [ 1; 2; 3 ] = Chunk.make [ 1; 2; 3 ]) true
        toBe (Chunk.make [ 1; 2; 3 ] = Chunk.make [ 1; 2 ]) false
        toBe (Chunk.make [ 1; 2 ] = Chunk.make [ 1; 2; 3 ]) false)

    test "render" (fun () ->
        toBe (Chunk.render (Chunk.make [ 0; 1; 2 ])) "Chunk([0,1,2])"
        toBe (Chunk.render (Chunk.make [ Chunk.make [ 1; 2; 3 ] ])) "Chunk([{ Values = 1,2,3 }])")

    test "modify" (fun () ->
        same (Chunk.modify Chunk.empty<int> 0 (fun n -> n * 2)) None
        same (Chunk.modify (Chunk.make [ 1; 2; 3 ]) 0 (fun n -> n * 2)) (Some(Chunk.make [ 2; 2; 3 ]))
        same (Chunk.modify (Chunk.make [ 1; 2; 3 ]) -1 (fun n -> n * 2)) None
        same (Chunk.modify (Chunk.make [ 1; 2; 3 ]) 10 (fun n -> n * 2)) None)

    test "replace" (fun () ->
        same (Chunk.replace Chunk.empty<int> 0 2) None
        same (Chunk.replace (Chunk.make [ 1; 2; 3 ]) 0 2) (Some(Chunk.make [ 2; 2; 3 ])))

    test "remove" (fun () ->
        same (Chunk.remove Chunk.empty<int> 0) Chunk.empty
        same (Chunk.remove (Chunk.make [ 1; 2; 3 ]) 0) (Chunk.make [ 2; 3 ])
        same (Chunk.remove (Chunk.make [ 1; 2; 3 ]) 10) (Chunk.make [ 1; 2; 3 ]))

    test "chunksOf" (fun () ->
        same (Chunk.chunksOf 2 Chunk.empty<int>) Chunk.empty
        same
            (Chunk.chunksOf 2 (Chunk.make [ 1; 2; 3; 4; 5 ]))
            (Chunk.make [ Chunk.make [ 1; 2 ]; Chunk.make [ 3; 4 ]; Chunk.make [ 5 ] ])
        same
            (Chunk.chunksOf 2 (Chunk.make [ 1; 2; 3; 4; 5; 6 ]))
            (Chunk.make [ Chunk.make [ 1; 2 ]; Chunk.make [ 3; 4 ]; Chunk.make [ 5; 6 ] ])
        same
            (Chunk.chunksOf 1 (Chunk.make [ 1; 2; 3; 4; 5 ]))
            (Chunk.make [ Chunk.make [ 1 ]; Chunk.make [ 2 ]; Chunk.make [ 3 ]; Chunk.make [ 4 ]; Chunk.make [ 5 ] ])
        same (Chunk.chunksOf 5 (Chunk.make [ 1; 2; 3; 4; 5 ])) (Chunk.make [ Chunk.make [ 1; 2; 3; 4; 5 ] ])
        same
            (Chunk.chunksOf 0 (Chunk.make [ 1; 2; 3; 4; 5 ]))
            (Chunk.make [ Chunk.make [ 1 ]; Chunk.make [ 2 ]; Chunk.make [ 3 ]; Chunk.make [ 4 ]; Chunk.make [ 5 ] ])
        same
            (Chunk.chunksOf -1 (Chunk.make [ 1; 2; 3; 4; 5 ]))
            (Chunk.make [ Chunk.make [ 1 ]; Chunk.make [ 2 ]; Chunk.make [ 3 ]; Chunk.make [ 4 ]; Chunk.make [ 5 ] ])
        same (Chunk.chunksOf 10 (Chunk.make [ 1; 2; 3; 4; 5 ])) (Chunk.make [ Chunk.make [ 1; 2; 3; 4; 5 ] ]))

    test "toArray returns a copy" (fun () ->
        same (Chunk.toArray Chunk.empty<int>) [||]
        same (Chunk.toArray (Chunk.make [ 1; 2; 3 ])) [| 1; 2; 3 |]
        let chunk = Chunk.make [ 1; 2; 3 ]
        let array = Chunk.toArray chunk
        array.[1] <- 4
        same (Chunk.toArray chunk) [| 1; 2; 3 |])

    test "toReadonlyArray returns a copy" (fun () ->
        same (Chunk.toReadonlyArray Chunk.empty<int>) [||]
        let mutable chunk = Chunk.empty<int>

        for i in 0..999 do
            chunk <- Chunk.appendAll (Chunk.singleton i) chunk

        same (Chunk.toReadonlyArray chunk) (Array.rev [| 0..999 |]))

    test "fromSeq" (fun () ->
        let myIterable = seq { for i in 1..5 -> i }
        same (Chunk.fromSeq myIterable) (Chunk.fromArrayUnsafe [| 1; 2; 3; 4; 5 |]))

    test "get" (fun () ->
        let chunk = Chunk.fromArrayUnsafe [| 1; 2; 3 |]
        same (Chunk.get 0 chunk) (Some 1)
        same (Chunk.get 4 chunk) None)

    test "getUnsafe" (fun () ->
        toThrow (fun () -> Chunk.getUnsafe 4 Chunk.empty<int> |> ignore)
        toThrow (fun () -> Chunk.getUnsafe 4 (Chunk.append 1 Chunk.empty) |> ignore)
        toBe (Chunk.getUnsafe 1 (Chunk.append 3 (Chunk.make [ 0; 1; 2 ]))) 1
        toBe (Chunk.getUnsafe 1 (Chunk.make [ 0; 1; 2 ])) 1
        toBe (Chunk.getUnsafe 1 (Chunk.fromArrayUnsafe [| 1; 2; 3 |])) 2)

    test "append" (fun () ->
        let a = [| 1; 2; 3 |]
        let b = [| 4; 5; 6 |]
        let mutable chunk = Chunk.fromArrayUnsafe a

        for e in b do
            chunk <- Chunk.append e chunk

        same (Chunk.toReadonlyArray chunk) (Array.append a b))

    test "prependAll" (fun () ->
        same (Chunk.prependAll Chunk.empty<int> (Chunk.make [ 1 ])) (Chunk.make [ 1 ])
        same (Chunk.prependAll (Chunk.make [ 1 ]) Chunk.empty) (Chunk.make [ 1 ])
        same (Chunk.prependAll Chunk.empty<int> (Chunk.make [ 1; 2 ])) (Chunk.make [ 1; 2 ])
        same (Chunk.prependAll (Chunk.make [ 2; 3 ]) (Chunk.make [ 1 ])) (Chunk.make [ 1; 2; 3 ])
        same (Chunk.prependAll (Chunk.make [ 3 ]) (Chunk.make [ 1; 2 ])) (Chunk.make [ 1; 2; 3 ]))

    test "prepend" (fun () ->
        let a = [| 1; 2; 3 |]
        let b = [| 7; 8; 9 |]
        let mutable chunk = Chunk.fromArrayUnsafe a

        for i in b.Length - 1 .. -1 .. 0 do
            chunk <- Chunk.prepend b.[i] chunk

        same (Chunk.toReadonlyArray chunk) (Array.append b a))

    test "take" (fun () ->
        same (Chunk.take 2 (Chunk.make [ 1; 2; 3 ])) (Chunk.make [ 1; 2 ])
        same (Chunk.take 5 (Chunk.make [ 1; 2; 3 ])) (Chunk.make [ 1; 2; 3 ])
        same (Chunk.take 3 (Chunk.take 4 (Chunk.make [ 1; 2; 3; 4; 5 ]))) (Chunk.make [ 1; 2; 3 ])
        same (Chunk.take 2 (Chunk.make [ 1 ])) (Chunk.make [ 1 ])
        same
            (Chunk.toReadonlyArray (Chunk.take 2 (Chunk.appendAll (Chunk.singleton 1) (Chunk.make [ 2; 3; 4 ]))))
            [| 1; 2 |])

    test "size from make and singleton" (fun () ->
        toBe (Chunk.size (Chunk.make [ 0; 1 ])) 2
        toBe (Chunk.size (Chunk.singleton 1)) 1)

    test "drop" (fun () ->
        let self = Chunk.make [ 0; 1 ]
        same (Chunk.drop 0 self) self
        same (Chunk.toReadonlyArray (Chunk.drop 1 (Chunk.drop 1 (Chunk.make [ 0; 1; 2; 3 ])))) [| 2; 3 |]
        same
            (Chunk.toReadonlyArray (Chunk.drop 2 (Chunk.appendAll (Chunk.make [ 1 ]) (Chunk.make [ 2; 3; 4 ]))))
            [| 3; 4 |])

    test "dropRight and dropWhile" (fun () ->
        same (Chunk.dropRight 1 (Chunk.fromArrayUnsafe [| 1; 2; 3 |])) (Chunk.fromArrayUnsafe [| 1; 2 |])
        same (Chunk.dropRight 3 (Chunk.fromArrayUnsafe [| 1; 2 |])) (Chunk.fromArrayUnsafe [||])
        same (Chunk.dropWhile (fun n -> n < 3) (Chunk.fromArrayUnsafe [| 1; 2; 3 |])) (Chunk.fromArrayUnsafe [| 3 |])
        same (Chunk.dropWhile (fun n -> n < 4) (Chunk.fromArrayUnsafe [| 1; 2; 3 |])) (Chunk.fromArrayUnsafe [||]))

    test "appendAll" (fun () ->
        same
            (Chunk.appendAll (Chunk.fromArrayUnsafe [| 0; 1 |]) (Chunk.fromArrayUnsafe [| 2; 3 |]))
            (Chunk.fromArrayUnsafe [| 0; 1; 2; 3 |])
        same
            (Chunk.appendAll (Chunk.fromArrayUnsafe [| 1; 2 |]) (Chunk.fromArrayUnsafe [| 3 |]))
            (Chunk.fromArrayUnsafe [| 1; 2; 3 |])
        same
            (Chunk.appendAll (Chunk.fromArrayUnsafe [| 1 |]) (Chunk.fromArrayUnsafe [| 2; 3; 4 |]))
            (Chunk.fromArrayUnsafe [| 1; 2; 3; 4 |])
        same (Chunk.appendAll (Chunk.fromArrayUnsafe [| 1; 2 |]) Chunk.empty) (Chunk.fromArrayUnsafe [| 1; 2 |])
        same (Chunk.appendAll Chunk.empty (Chunk.fromArrayUnsafe [| 1; 2 |])) (Chunk.fromArrayUnsafe [| 1; 2 |])

        let result =
            Chunk.empty
            |> fun c -> Chunk.appendAll c (Chunk.fromArrayUnsafe [| 1 |])
            |> fun c -> Chunk.appendAll c (Chunk.fromArrayUnsafe [| 2 |])
            |> fun c -> Chunk.appendAll c (Chunk.fromArrayUnsafe [| 3; 4 |])
            |> fun c -> Chunk.appendAll c (Chunk.fromArrayUnsafe [| 5; 6 |])

        same result (Chunk.fromArrayUnsafe [| 1; 2; 3; 4; 5; 6 |]))

    test "zip and zipWith" (fun () ->
        same (Chunk.zip Chunk.empty<int> Chunk.empty<int>) Chunk.empty
        same (Chunk.zip (Chunk.make [ 1 ]) Chunk.empty<int>) Chunk.empty
        same (Chunk.zip Chunk.empty<int> (Chunk.make [ 1 ])) Chunk.empty
        same (Chunk.toArray (Chunk.zip (Chunk.make [ 1 ]) (Chunk.make [ 2 ]))) [| (1, 2) |]

        let left = Chunk.drop 1 (Chunk.make [ -1; 0; 1 ])
        let right = Chunk.drop 1 (Chunk.make [ 1; 0; 0; 1 ])
        let zipped = Chunk.zipWith left (Chunk.take (Chunk.size left) right) (fun a b -> (a, b))
        same (Chunk.toArray zipped) [| (0, 0); (1, 0) |])

    test "last" (fun () ->
        same (Chunk.last Chunk.empty<int>) None
        same (Chunk.last (Chunk.make [ 1; 2; 3 ])) (Some 3))

    test "map and mapAccum" (fun () ->
        same (Chunk.map Chunk.empty<int> (fun n _ -> n + 1)) Chunk.empty
        same (Chunk.map (Chunk.singleton 1) (fun n _ -> n + 1)) (Chunk.singleton 2)
        same (Chunk.map (Chunk.make [ 1; 2; 3 ]) (fun n _ -> n + 1)) (Chunk.make [ 2; 3; 4 ])
        same (Chunk.map (Chunk.make [ 1; 2; 3 ]) (fun n i -> n + i)) (Chunk.make [ 1; 3; 5 ])

        let state, chunk = Chunk.mapAccum (Chunk.make [ 1; 2; 3 ]) "-" (fun s a -> (s + string a, a + 1))
        toBe state "-123"
        same chunk (Chunk.make [ 2; 3; 4 ]))

    test "partition and separate" (fun () ->
        same (Chunk.partition Chunk.empty<int> (fun n _ -> if n > 2 then Ok n else Error n)) (Chunk.empty, Chunk.empty)
        same
            (Chunk.partition (Chunk.make [ 1; 3 ]) (fun n _ -> if n > 2 then Ok n else Error n))
            (Chunk.make [ 1 ], Chunk.make [ 3 ])
        same
            (Chunk.partition (Chunk.make [ 1; 2 ]) (fun n i ->
                if n + i > 2 then Ok(n + i) else Error(sprintf "negative:%d" (n + i))))
            (Chunk.make [ "negative:1" ], Chunk.make [ 3 ])
        same (Chunk.partition Chunk.empty<Result<int, string>> (fun r _ -> r)) (Chunk.empty, Chunk.empty)
        same
            (Chunk.partition (Chunk.make [ Ok 1; Error "a"; Ok 2 ]) (fun r _ -> r))
            (Chunk.make [ "a" ], Chunk.make [ 1; 2 ])
        same (Chunk.separate Chunk.empty<Result<int, string>>) (Chunk.empty, Chunk.empty)
        same (Chunk.separate (Chunk.make [ Ok 1; Error "e"; Ok 2 ])) (Chunk.make [ "e" ], Chunk.make [ 1; 2 ]))

    test "size split tail and filters" (fun () ->
        toBe (Chunk.size Chunk.empty<int>) 0
        toBe (Chunk.size (Chunk.make [ 1; 2; 3 ])) 3
        same (Chunk.split 2 Chunk.empty<int>) Chunk.empty
        same (Chunk.split 2 (Chunk.make [ 1 ])) (Chunk.make [ Chunk.make [ 1 ] ])
        same (Chunk.split 2 (Chunk.make [ 1; 2 ])) (Chunk.make [ Chunk.make [ 1 ]; Chunk.make [ 2 ] ])
        same (Chunk.split 2 (Chunk.make [ 1; 2; 3; 4; 5 ])) (Chunk.make [ Chunk.make [ 1; 2; 3 ]; Chunk.make [ 4; 5 ] ])
        same (Chunk.split 3 (Chunk.make [ 1; 2; 3; 4; 5 ])) (Chunk.make [ Chunk.make [ 1; 2 ]; Chunk.make [ 3; 4 ]; Chunk.make [ 5 ] ])
        same (Chunk.tail Chunk.empty<int>) None
        same (Chunk.tail (Chunk.make [ 1; 2; 3 ])) (Some(Chunk.make [ 2; 3 ]))
        same (Chunk.filter (fun n -> n % 2 = 1) (Chunk.make [ 1; 2; 3 ])) (Chunk.make [ 1; 3 ])
        same (Chunk.filter Option.isSome (Chunk.make [ Some 3; None; Some 1 ])) (Chunk.make [ Some 3; Some 1 ]))

    test "filterMap compact and dedupeAdjacent" (fun () ->
        same
            (Chunk.filterMap (Chunk.make [ 1; 2; 3; 4 ]) (fun n _ -> if n % 2 = 0 then Ok(n * 2) else Error()))
            (Chunk.make [ 4; 8 ])
        same
            (Chunk.filterMap (Chunk.make [ 1; 2; 3; 4 ]) (fun n i -> if i % 2 = 0 then Ok(n + i) else Error()))
            (Chunk.make [ 1; 5 ])
        same
            (Chunk.filterMapWhile (Chunk.make [ 1; 3; 4; 5 ]) (fun n -> if n % 2 = 1 then Ok n else Error()))
            (Chunk.make [ 1; 3 ])
        same (Chunk.compact Chunk.empty<int option>) Chunk.empty
        same (Chunk.compact (Chunk.make [ Some 1; Some 2; Some 3 ])) (Chunk.make [ 1; 2; 3 ])
        same (Chunk.compact (Chunk.make [ Some 1; None; Some 3 ])) (Chunk.make [ 1; 3 ])
        same (Chunk.dedupeAdjacent Chunk.empty<int>) Chunk.empty
        same (Chunk.dedupeAdjacent (Chunk.make [ 1; 2; 3 ])) (Chunk.make [ 1; 2; 3 ])
        same (Chunk.dedupeAdjacent (Chunk.make [ 1; 2; 2; 3; 3 ])) (Chunk.make [ 1; 2; 3 ]))

    test "flatMap and set-like operations" (fun () ->
        same (Chunk.flatMap (Chunk.make [ 1 ]) (fun n _ -> Chunk.make [ n; n + 1 ])) (Chunk.make [ 1; 2 ])
        same
            (Chunk.flatMap (Chunk.make [ 1; 2; 3 ]) (fun n _ -> Chunk.make [ n; n + 1 ]))
            (Chunk.make [ 1; 2; 2; 3; 3; 4 ])
        same
            (Chunk.flatMap (Chunk.make [ 0; 1; 2 ]) (fun n i -> Chunk.make [ i; n + 10 ]))
            (Chunk.make [ 0; 10; 1; 11; 2; 12 ])
        same (Chunk.union (Chunk.make [ 1; 2; 3 ]) Chunk.empty) (Chunk.make [ 1; 2; 3 ])
        same (Chunk.union Chunk.empty (Chunk.make [ 1; 2; 3 ])) (Chunk.make [ 1; 2; 3 ])
        same (Chunk.union (Chunk.make [ 1; 2; 3 ]) (Chunk.make [ 2; 3; 4 ])) (Chunk.make [ 1; 2; 3; 4 ])
        same (Chunk.intersection (Chunk.make [ 1; 2; 3 ]) Chunk.empty) Chunk.empty
        same (Chunk.intersection Chunk.empty (Chunk.make [ 2; 3; 4 ])) Chunk.empty
        same (Chunk.intersection (Chunk.make [ 1; 2; 3 ]) (Chunk.make [ 2; 3; 4 ])) (Chunk.make [ 2; 3 ]))

    test "isEmpty and unsafe heads" (fun () ->
        toBe (Chunk.isEmpty Chunk.empty<int>) true
        toBe (Chunk.isEmpty (Chunk.make [ 1 ])) false
        toBe (Chunk.lastUnsafe (Chunk.make [ 1 ])) 1
        toBe (Chunk.lastUnsafe (Chunk.make [ 1; 2; 3 ])) 3
        toThrow (fun () -> Chunk.lastUnsafe Chunk.empty<int> |> ignore)
        toBe (Chunk.headUnsafe (Chunk.make [ 1; 2; 3 ])) 1
        toThrow (fun () -> Chunk.headUnsafe Chunk.empty<int> |> ignore))

    test "split helpers and takeWhile" (fun () ->
        same (Chunk.splitNonEmptyAt (Chunk.make [ 1; 2; 3; 4 ]) 2) (Chunk.make [ 1; 2 ], Chunk.make [ 3; 4 ])
        same (Chunk.splitNonEmptyAt (Chunk.make [ 1; 2; 3; 4 ]) 10) (Chunk.make [ 1; 2; 3; 4 ], Chunk.empty)
        same (Chunk.splitWhere Chunk.empty<int> (fun n -> n > 1)) (Chunk.empty, Chunk.empty)
        same (Chunk.splitWhere (Chunk.make [ 1; 2; 3 ]) (fun n -> n > 1)) (Chunk.make [ 1 ], Chunk.make [ 2; 3 ])
        same (Chunk.takeWhile (fun n -> n <= 2) Chunk.empty<int>) Chunk.empty
        same (Chunk.takeWhile (fun n -> n <= 2) (Chunk.make [ 1; 2; 3 ])) (Chunk.make [ 1; 2 ]))

    test "dedupe unzip reverse flatten makeBy and range" (fun () ->
        same (Chunk.dedupe Chunk.empty<int>) Chunk.empty
        same (Chunk.dedupe (Chunk.make [ 1; 2; 3 ])) (Chunk.make [ 1; 2; 3 ])
        same (Chunk.dedupe (Chunk.make [ 1; 2; 3; 2; 1; 3 ])) (Chunk.make [ 1; 2; 3 ])
        same (Chunk.unzip Chunk.empty<string * int>) (Chunk.empty, Chunk.empty)
        same (Chunk.unzip (Chunk.make [ ("a", 1); ("b", 2) ])) (Chunk.make [ "a"; "b" ], Chunk.make [ 1; 2 ])
        same (Chunk.reverse Chunk.empty<int>) Chunk.empty
        same (Chunk.reverse (Chunk.make [ 1; 2; 3 ])) (Chunk.make [ 3; 2; 1 ])
        same (Chunk.reverse (Chunk.take 3 (Chunk.make [ 1; 2; 3; 4 ]))) (Chunk.make [ 3; 2; 1 ])
        same (Chunk.flatten (Chunk.make [ Chunk.make [ 1 ]; Chunk.make [ 2 ]; Chunk.make [ 3 ] ])) (Chunk.make [ 1; 2; 3 ])
        same (Chunk.makeBy 5 (fun n -> n * 2)) (Chunk.make [ 0; 2; 4; 6; 8 ])
        same (Chunk.makeBy 2 (fun n -> n * 2)) (Chunk.make [ 0; 2 ])
        same (Chunk.range 0 0) (Chunk.make [ 0 ])
        same (Chunk.range 0 1) (Chunk.make [ 0; 1 ])
        same (Chunk.range 1 5) (Chunk.make [ 1; 2; 3; 4; 5 ])
        same (Chunk.range 10 15) (Chunk.make [ 10; 11; 12; 13; 14; 15 ])
        same (Chunk.range -1 0) (Chunk.make [ -1; 0 ])
        same (Chunk.range -5 -1) (Chunk.make [ -5; -4; -3; -2; -1 ])
        same (Chunk.range 2 1) (Chunk.make [ 2 ])
        same (Chunk.range -1 -2) (Chunk.make [ -1 ]))

    test "predicates loops sorting and equality helpers" (fun () ->
        let isPositive n = n > 0
        toBe (Chunk.some (Chunk.make [ -1; -2; 3 ]) isPositive) true
        toBe (Chunk.some (Chunk.make [ -1; -2; -3 ]) isPositive) false

        let acc = ResizeArray<string>()
        Chunk.forEach (Chunk.make [ 1; 2; 3; 4 ]) (fun n i -> acc.Add(sprintf "%d-%d" n i))
        same (acc.ToArray()) [| "1-0"; "2-1"; "3-2"; "4-3" |]

        let chunk = Chunk.make [ {| a = "a"; b = 2 |}; {| a = "b"; b = 1 |} ]
        same (Chunk.sortWith chunk (fun x -> x.b) compare) (Chunk.make [ {| a = "b"; b = 1 |}; {| a = "a"; b = 2 |} ])

        let equivalence = Chunk.makeEquivalence (fun (a: int) b -> a = b)
        toBe (equivalence Chunk.empty Chunk.empty) true
        toBe (equivalence (Chunk.make [ 1; 2; 3 ]) (Chunk.make [ 1; 2; 3 ])) true
        toBe (equivalence (Chunk.make [ 1; 2; 3 ]) (Chunk.make [ 1; 2 ])) false
        toBe (equivalence (Chunk.make [ 1; 2; 3 ]) (Chunk.make [ 1; 2; 4 ])) false)

    test "difference contains find reduce and join" (fun () ->
        let eq (a: {| id: int |}) (b: {| id: int |}) = a.id = b.id
        let objects = Chunk.make [ {| id = 1 |}; {| id = 2 |}; {| id = 3 |} ]
        same (Chunk.differenceWith eq objects (Chunk.make [ {| id = 1 |}; {| id = 2 |} ])) (Chunk.make [ {| id = 3 |} ])
        same (Chunk.differenceWith eq Chunk.empty objects) Chunk.empty
        same (Chunk.differenceWith eq objects Chunk.empty) objects
        same (Chunk.differenceWith eq objects objects) Chunk.empty

        let current = Chunk.make [ 1; 3; 5; 7; 9 ]
        same (Chunk.difference current (Chunk.make [ 1; 2; 3; 4; 5 ])) (Chunk.make [ 7; 9 ])
        same (Chunk.difference Chunk.empty current) Chunk.empty
        same (Chunk.difference current Chunk.empty) current
        same (Chunk.difference current current) Chunk.empty

        let chunk = Chunk.make [ 1; 2; 3; 4; 5 ]
        same (Chunk.takeRight 2 chunk) (Chunk.make [ 4; 5 ])
        same (Chunk.takeRight 10 chunk) chunk
        same (Chunk.takeRight 0 chunk) Chunk.empty
        same (Chunk.takeRight -1 chunk) Chunk.empty
        same (Chunk.sort (Chunk.make [ 3; 1; 4; 5; 2 ]) compare) (Chunk.make [ 1; 2; 3; 4; 5 ])
        same (Chunk.sort Chunk.empty<int> compare) Chunk.empty
        toBe (Chunk.contains chunk 3) true
        toBe (Chunk.contains chunk 10) false
        toBe (Chunk.contains Chunk.empty<int> 1) false

        let people = Chunk.make [ {| id = 1; name = "Alice" |}; {| id = 2; name = "Bob" |} ]
        let byId (a: {| id: int; name: string |}) (b: {| id: int; name: string |}) = a.id = b.id
        toBe (Chunk.containsWith byId people {| id = 1; name = "Different" |}) true
        toBe (Chunk.containsWith byId people {| id = 3; name = "Charlie" |}) false

        same (Chunk.findFirst chunk (fun n -> n > 3)) (Some 4)
        same (Chunk.findFirst chunk (fun n -> n > 10)) None
        same (Chunk.findFirstIndex chunk (fun n -> n > 3)) (Some 3)
        same (Chunk.findFirstIndex chunk (fun n -> n % 2 = 0)) (Some 1)
        same (Chunk.findFirstIndex chunk (fun n -> n > 10)) None
        same (Chunk.findLast chunk (fun n -> n < 4)) (Some 3)
        same (Chunk.findLast chunk (fun n -> n % 2 = 0)) (Some 4)
        same (Chunk.findLast chunk (fun n -> n > 10)) None
        same (Chunk.findLastIndex chunk (fun n -> n < 4)) (Some 2)
        same (Chunk.findLastIndex chunk (fun n -> n % 2 = 0)) (Some 3)
        same (Chunk.findLastIndex chunk (fun n -> n > 10)) None
        toBe (Chunk.every chunk (fun n -> n > 0)) true
        toBe (Chunk.every chunk (fun n -> n > 3)) false
        toBe (Chunk.every Chunk.empty<int> (fun n -> n > 0)) true
        toBe (Chunk.reduce chunk 0 (fun acc n _ -> acc + n)) 15
        toBe (Chunk.reduce (Chunk.make [ "a"; "b"; "c" ]) "" (fun acc w i -> acc + sprintf "%d:%s " i w)) "0:a 1:b 2:c "
        toBe (Chunk.reduceRight (Chunk.make [ 1; 2; 3; 4 ]) 0 (fun acc n _ -> acc + n)) 10
        toBe (Chunk.reduceRight (Chunk.make [ "a"; "b"; "c" ]) "" (fun acc w i -> acc + sprintf "%d:%s " i w)) "2:c 1:b 0:a "
        toBe (Chunk.join (Chunk.make [ "apple"; "banana"; "cherry" ]) ", ") "apple, banana, cherry"
        toBe (Chunk.join Chunk.empty<string> ", ") ""
        toBe (Chunk.join (Chunk.make [ "hello" ]) ", ") "hello"))
