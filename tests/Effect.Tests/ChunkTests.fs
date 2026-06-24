module Effect.Tests.ChunkTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/Chunk.test.ts
//
// Behaviour, not JS phrasing. Property-based (`fc.assert`) cases are ported as
// representative example-based facts. Tests that assert on the upstream rope
// internals (`backing._tag`, `depth`), the `isChunk` runtime guard, `toJSON`,
// `inspect`, and the `.pipe()` method are intentionally omitted — the F# port
// drops that JS-runtime machinery (see Chunk.fs doc comment).

let private arr (c: Chunk<'a>) = Chunk.toArray c

[<Fact>]
let ``structural equality`` () =
    // Effect's Equal.equals reduces to FSharp.Core structural equality here.
    Assert.True(Chunk.make [ 0 ] = Chunk.make [ 0 ])
    Assert.True(Chunk.make [ 1; 2; 3 ] = Chunk.make [ 1; 2; 3 ])
    Assert.False(Chunk.make [ 1; 2; 3 ] = Chunk.make [ 1; 2 ])
    Assert.False(Chunk.make [ 1; 2 ] = Chunk.make [ 1; 2; 3 ])

[<Fact>]
let ``toString`` () =
    Assert.Equal("Chunk([0,1,2])", Chunk.render (Chunk.make [ 0; 1; 2 ]))
    Assert.Equal("Chunk([Chunk([1,2,3])])", Chunk.render (Chunk.make [ Chunk.make [ 1; 2; 3 ] ]))

[<Fact>]
let ``modify`` () =
    Assert.Equal(None, Chunk.modify Chunk.empty 0 (fun n -> n * 2))
    Assert.Equal(Some(Chunk.make [ 2; 2; 3 ]), Chunk.modify (Chunk.make [ 1; 2; 3 ]) 0 (fun n -> n * 2))
    Assert.Equal(None, Chunk.modify (Chunk.make [ 1; 2; 3 ]) -1 (fun n -> n * 2))
    Assert.Equal(None, Chunk.modify (Chunk.make [ 1; 2; 3 ]) 10 (fun n -> n * 2))

[<Fact>]
let ``replace`` () =
    Assert.Equal(None, Chunk.replace Chunk.empty 0 2)
    Assert.Equal(Some(Chunk.make [ 2; 2; 3 ]), Chunk.replace (Chunk.make [ 1; 2; 3 ]) 0 2)

[<Fact>]
let ``remove`` () =
    Assert.Equal(Chunk.empty, Chunk.remove Chunk.empty 0)
    Assert.Equal(Chunk.make [ 2; 3 ], Chunk.remove (Chunk.make [ 1; 2; 3 ]) 0)
    // out of bounds returns same chunk
    Assert.Equal(Chunk.make [ 1; 2; 3 ], Chunk.remove (Chunk.make [ 1; 2; 3 ]) 10)

[<Fact>]
let ``chunksOf`` () =
    Assert.Equal(Chunk.empty, Chunk.chunksOf 2 Chunk.empty)
    Assert.Equal(
        Chunk.make [ Chunk.make [ 1; 2 ]; Chunk.make [ 3; 4 ]; Chunk.make [ 5 ] ],
        Chunk.chunksOf 2 (Chunk.make [ 1; 2; 3; 4; 5 ]))
    Assert.Equal(
        Chunk.make [ Chunk.make [ 1; 2 ]; Chunk.make [ 3; 4 ]; Chunk.make [ 5; 6 ] ],
        Chunk.chunksOf 2 (Chunk.make [ 1; 2; 3; 4; 5; 6 ]))
    Assert.Equal(
        Chunk.make [ Chunk.make [ 1 ]; Chunk.make [ 2 ]; Chunk.make [ 3 ]; Chunk.make [ 4 ]; Chunk.make [ 5 ] ],
        Chunk.chunksOf 1 (Chunk.make [ 1; 2; 3; 4; 5 ]))
    Assert.Equal(Chunk.make [ Chunk.make [ 1; 2; 3; 4; 5 ] ], Chunk.chunksOf 5 (Chunk.make [ 1; 2; 3; 4; 5 ]))
    // out of bounds (n <= 0) yields singletons
    Assert.Equal(
        Chunk.make [ Chunk.make [ 1 ]; Chunk.make [ 2 ]; Chunk.make [ 3 ]; Chunk.make [ 4 ]; Chunk.make [ 5 ] ],
        Chunk.chunksOf 0 (Chunk.make [ 1; 2; 3; 4; 5 ]))
    Assert.Equal(
        Chunk.make [ Chunk.make [ 1 ]; Chunk.make [ 2 ]; Chunk.make [ 3 ]; Chunk.make [ 4 ]; Chunk.make [ 5 ] ],
        Chunk.chunksOf -1 (Chunk.make [ 1; 2; 3; 4; 5 ]))
    Assert.Equal(Chunk.make [ Chunk.make [ 1; 2; 3; 4; 5 ] ], Chunk.chunksOf 10 (Chunk.make [ 1; 2; 3; 4; 5 ]))

[<Fact>]
let ``toArray`` () =
    Assert.Equal<int[]>([||], Chunk.toArray Chunk.empty<int>)
    Assert.Equal<int[]>([| 1; 2; 3 |], Chunk.toArray (Chunk.make [ 1; 2; 3 ]))
    // mutating the returned array does not affect the chunk
    let chunk = Chunk.make [ 1; 2; 3 ]
    let a = Chunk.toArray chunk
    a.[1] <- 4
    Assert.Equal<int[]>([| 1; 2; 3 |], Chunk.toArray chunk)

[<Fact>]
let ``toReadonlyArray`` () =
    Assert.Equal<int[]>([||], Chunk.toReadonlyArray Chunk.empty<int>)
    // a moderately large chunk built by repeated appendAll round-trips intact
    let mutable chunk = Chunk.empty<int>
    for i in 0 .. 999 do
        chunk <- Chunk.appendAll (Chunk.singleton i) chunk
    Assert.Equal<int[]>(Array.rev [| 0 .. 999 |], Chunk.toReadonlyArray chunk)

[<Fact>]
let ``fromSeq`` () =
    let myIterable = seq { for i in 1 .. 5 -> i }
    Assert.Equal(Chunk.fromArrayUnsafe [| 1; 2; 3; 4; 5 |], Chunk.fromSeq myIterable)

[<Fact>]
let ``get`` () =
    let chunk = Chunk.fromArrayUnsafe [| 1; 2; 3 |]
    Assert.Equal(Some 1, Chunk.get 0 chunk)
    Assert.Equal(None, Chunk.get 4 chunk)

[<Fact>]
let ``getUnsafe`` () =
    Assert.Throws<System.Exception>(fun () -> Chunk.getUnsafe 4 Chunk.empty |> ignore) |> ignore
    Assert.Throws<System.Exception>(fun () -> Chunk.getUnsafe 4 (Chunk.append 1 Chunk.empty) |> ignore) |> ignore
    // in-bounds returns the value
    Assert.Equal(1, Chunk.getUnsafe 1 (Chunk.append 3 (Chunk.make [ 0; 1; 2 ])))
    Assert.Equal(1, Chunk.getUnsafe 1 (Chunk.make [ 0; 1; 2 ]))
    Assert.Equal(2, Chunk.getUnsafe 1 (Chunk.fromArrayUnsafe [| 1; 2; 3 |]))

[<Fact>]
let ``append`` () =
    let a = [| 1; 2; 3 |]
    let b = [| 4; 5; 6 |]
    let mutable chunk = Chunk.fromArrayUnsafe a
    for e in b do
        chunk <- Chunk.append e chunk
    Assert.Equal<int[]>(Array.append a b, Chunk.toReadonlyArray chunk)

[<Fact>]
let ``prependAll`` () =
    Assert.Equal(Chunk.make [ 1 ], Chunk.prependAll Chunk.empty (Chunk.make [ 1 ]))
    Assert.Equal(Chunk.make [ 1 ], Chunk.prependAll (Chunk.make [ 1 ]) Chunk.empty)
    Assert.Equal(Chunk.make [ 1; 2 ], Chunk.prependAll Chunk.empty (Chunk.make [ 1; 2 ]))
    Assert.Equal(Chunk.make [ 1; 2; 3 ], Chunk.prependAll (Chunk.make [ 2; 3 ]) (Chunk.make [ 1 ]))
    Assert.Equal(Chunk.make [ 1; 2; 3 ], Chunk.prependAll (Chunk.make [ 3 ]) (Chunk.make [ 1; 2 ]))

[<Fact>]
let ``prepend`` () =
    let a = [| 1; 2; 3 |]
    let b = [| 7; 8; 9 |]
    let mutable chunk = Chunk.fromArrayUnsafe a
    for i in b.Length - 1 .. -1 .. 0 do
        chunk <- Chunk.prepend b.[i] chunk
    Assert.Equal<int[]>(Array.append b a, Chunk.toReadonlyArray chunk)

[<Fact>]
let ``take`` () =
    Assert.Equal(Chunk.make [ 1; 2 ], Chunk.take 2 (Chunk.make [ 1; 2; 3 ]))
    Assert.Equal(Chunk.make [ 1; 2; 3 ], Chunk.take 5 (Chunk.make [ 1; 2; 3 ]))
    Assert.Equal(Chunk.make [ 1; 2; 3 ], Chunk.take 3 (Chunk.take 4 (Chunk.make [ 1; 2; 3; 4; 5 ])))
    Assert.Equal(Chunk.make [ 1 ], Chunk.take 2 (Chunk.make [ 1 ]))
    Assert.Equal<int[]>([| 1; 2 |], Chunk.toReadonlyArray (Chunk.take 2 (Chunk.appendAll (Chunk.singleton 1) (Chunk.make [ 2; 3; 4 ]))))

[<Fact>]
let ``make returns the right size`` () =
    Assert.Equal(2, Chunk.size (Chunk.make [ 0; 1 ]))

[<Fact>]
let ``singleton has size 1`` () =
    Assert.Equal(1, Chunk.size (Chunk.singleton 1))

[<Fact>]
let ``drop`` () =
    let self = Chunk.make [ 0; 1 ]
    Assert.Equal(self, Chunk.drop 0 self)
    Assert.Equal<int[]>([| 2; 3 |], Chunk.toReadonlyArray (Chunk.drop 1 (Chunk.drop 1 (Chunk.make [ 0; 1; 2; 3 ]))))
    Assert.Equal<int[]>([| 3; 4 |], Chunk.toReadonlyArray (Chunk.drop 2 (Chunk.appendAll (Chunk.make [ 1 ]) (Chunk.make [ 2; 3; 4 ]))))

[<Fact>]
let ``dropRight`` () =
    Assert.Equal(Chunk.fromArrayUnsafe [| 1; 2 |], Chunk.dropRight 1 (Chunk.fromArrayUnsafe [| 1; 2; 3 |]))
    Assert.Equal(Chunk.fromArrayUnsafe [||], Chunk.dropRight 3 (Chunk.fromArrayUnsafe [| 1; 2 |]))

[<Fact>]
let ``dropWhile`` () =
    Assert.Equal(Chunk.fromArrayUnsafe [| 3 |], Chunk.dropWhile (fun n -> n < 3) (Chunk.fromArrayUnsafe [| 1; 2; 3 |]))
    Assert.Equal(Chunk.fromArrayUnsafe [||], Chunk.dropWhile (fun n -> n < 4) (Chunk.fromArrayUnsafe [| 1; 2; 3 |]))

[<Fact>]
let ``concat (appendAll)`` () =
    Assert.Equal(Chunk.fromArrayUnsafe [| 0; 1; 2; 3 |], Chunk.appendAll (Chunk.fromArrayUnsafe [| 0; 1 |]) (Chunk.fromArrayUnsafe [| 2; 3 |]))
    Assert.Equal(Chunk.fromArrayUnsafe [| 1; 2; 3 |], Chunk.appendAll (Chunk.fromArrayUnsafe [| 1; 2 |]) (Chunk.fromArrayUnsafe [| 3 |]))
    Assert.Equal(Chunk.fromArrayUnsafe [| 1; 2; 3; 4 |], Chunk.appendAll (Chunk.fromArrayUnsafe [| 1 |]) (Chunk.fromArrayUnsafe [| 2; 3; 4 |]))
    Assert.Equal(Chunk.fromArrayUnsafe [| 1; 2 |], Chunk.appendAll (Chunk.fromArrayUnsafe [| 1; 2 |]) Chunk.empty)
    Assert.Equal(Chunk.fromArrayUnsafe [| 1; 2 |], Chunk.appendAll Chunk.empty (Chunk.fromArrayUnsafe [| 1; 2 |]))
    // chained
    let result =
        Chunk.empty
        |> fun c -> Chunk.appendAll c (Chunk.fromArrayUnsafe [| 1 |])
        |> fun c -> Chunk.appendAll c (Chunk.fromArrayUnsafe [| 2 |])
        |> fun c -> Chunk.appendAll c (Chunk.fromArrayUnsafe [| 3; 4 |])
        |> fun c -> Chunk.appendAll c (Chunk.fromArrayUnsafe [| 5; 6 |])
    Assert.Equal(Chunk.fromArrayUnsafe [| 1; 2; 3; 4; 5; 6 |], result)

[<Fact>]
let ``zip`` () =
    Assert.Equal(Chunk.empty, Chunk.zip Chunk.empty Chunk.empty)
    Assert.Equal(Chunk.empty, Chunk.zip (Chunk.make [ 1 ]) Chunk.empty)
    Assert.Equal(Chunk.empty, Chunk.zip Chunk.empty (Chunk.make [ 1 ]))
    Assert.Equal<(int * int)[]>([| (1, 2) |], Chunk.toArray (Chunk.zip (Chunk.make [ 1 ]) (Chunk.make [ 2 ])))

[<Fact>]
let ``zipWith drops the leftover`` () =
    let left = Chunk.drop 1 (Chunk.make [ -1; 0; 1 ])
    let right = Chunk.drop 1 (Chunk.make [ 1; 0; 0; 1 ])
    let zipped = Chunk.zipWith left (Chunk.take (Chunk.size left) right) (fun a b -> (a, b))
    Assert.Equal<(int * int)[]>([| (0, 0); (1, 0) |], Chunk.toArray zipped)

[<Fact>]
let ``last`` () =
    Assert.Equal(None, Chunk.last Chunk.empty)
    Assert.Equal(Some 3, Chunk.last (Chunk.make [ 1; 2; 3 ]))

[<Fact>]
let ``map`` () =
    Assert.Equal(Chunk.empty, Chunk.map Chunk.empty (fun n _ -> n + 1))
    Assert.Equal(Chunk.singleton 2, Chunk.map (Chunk.singleton 1) (fun n _ -> n + 1))
    Assert.Equal(Chunk.make [ 2; 3; 4 ], Chunk.map (Chunk.make [ 1; 2; 3 ]) (fun n _ -> n + 1))
    Assert.Equal(Chunk.make [ 1; 3; 5 ], Chunk.map (Chunk.make [ 1; 2; 3 ]) (fun n i -> n + i))

[<Fact>]
let ``mapAccum`` () =
    let state, chunk = Chunk.mapAccum (Chunk.make [ 1; 2; 3 ]) "-" (fun s a -> (s + string a, a + 1))
    Assert.Equal("-123", state)
    Assert.Equal(Chunk.make [ 2; 3; 4 ], chunk)

[<Fact>]
let ``partition`` () =
    let p1 = Chunk.partition Chunk.empty<int> (fun n _ -> if n > 2 then Ok n else Error n)
    Assert.Equal((Chunk.empty, Chunk.empty), p1)
    let p2 = Chunk.partition (Chunk.make [ 1; 3 ]) (fun n _ -> if n > 2 then Ok n else Error n)
    Assert.Equal((Chunk.make [ 1 ], Chunk.make [ 3 ]), p2)
    let p3 =
        Chunk.partition (Chunk.make [ 1; 2 ]) (fun n i ->
            if n + i > 2 then Ok(n + i) else Error(sprintf "negative:%d" (n + i)))
    Assert.Equal((Chunk.make [ "negative:1" ], Chunk.make [ 3 ]), p3)

[<Fact>]
let ``partition (identity)`` () =
    let p1 = Chunk.partition Chunk.empty (fun r _ -> r)
    Assert.Equal((Chunk.empty, Chunk.empty), p1)
    let p2 = Chunk.partition (Chunk.make [ Ok 1; Error "a"; Ok 2 ]) (fun r _ -> r)
    Assert.Equal((Chunk.make [ "a" ], Chunk.make [ 1; 2 ]), p2)

[<Fact>]
let ``separate`` () =
    Assert.Equal((Chunk.empty, Chunk.empty), Chunk.separate Chunk.empty)
    let s = Chunk.separate (Chunk.make [ Ok 1; Error "e"; Ok 2 ])
    Assert.Equal((Chunk.make [ "e" ], Chunk.make [ 1; 2 ]), s)

[<Fact>]
let ``size`` () =
    Assert.Equal(0, Chunk.size Chunk.empty)
    Assert.Equal(3, Chunk.size (Chunk.make [ 1; 2; 3 ]))

[<Fact>]
let ``split`` () =
    Assert.Equal(Chunk.empty, Chunk.split 2 Chunk.empty)
    Assert.Equal(Chunk.make [ Chunk.make [ 1 ] ], Chunk.split 2 (Chunk.make [ 1 ]))
    Assert.Equal(Chunk.make [ Chunk.make [ 1 ]; Chunk.make [ 2 ] ], Chunk.split 2 (Chunk.make [ 1; 2 ]))
    Assert.Equal(Chunk.make [ Chunk.make [ 1; 2; 3 ]; Chunk.make [ 4; 5 ] ], Chunk.split 2 (Chunk.make [ 1; 2; 3; 4; 5 ]))
    Assert.Equal(
        Chunk.make [ Chunk.make [ 1; 2 ]; Chunk.make [ 3; 4 ]; Chunk.make [ 5 ] ],
        Chunk.split 3 (Chunk.make [ 1; 2; 3; 4; 5 ]))

[<Fact>]
let ``tail`` () =
    Assert.Equal(None, Chunk.tail Chunk.empty)
    Assert.Equal(Some(Chunk.make [ 2; 3 ]), Chunk.tail (Chunk.make [ 1; 2; 3 ]))

[<Fact>]
let ``filter`` () =
    Assert.Equal(Chunk.make [ 1; 3 ], Chunk.filter (fun n -> n % 2 = 1) (Chunk.make [ 1; 2; 3 ]))
    Assert.Equal(
        Chunk.make [ Some 3; Some 1 ],
        Chunk.filter Option.isSome (Chunk.make [ Some 3; None; Some 1 ]))

[<Fact>]
let ``filterMap`` () =
    Assert.Equal(
        Chunk.make [ 4; 8 ],
        Chunk.filterMap (Chunk.make [ 1; 2; 3; 4 ]) (fun n _ -> if n % 2 = 0 then Ok(n * 2) else Error()))
    Assert.Equal(
        Chunk.make [ 1; 5 ],
        Chunk.filterMap (Chunk.make [ 1; 2; 3; 4 ]) (fun n i -> if i % 2 = 0 then Ok(n + i) else Error()))

[<Fact>]
let ``filterMapWhile`` () =
    Assert.Equal(
        Chunk.make [ 1; 3 ],
        Chunk.filterMapWhile (Chunk.make [ 1; 3; 4; 5 ]) (fun n -> if n % 2 = 1 then Ok n else Error()))

[<Fact>]
let ``compact`` () =
    Assert.Equal(Chunk.empty, Chunk.compact Chunk.empty)
    Assert.Equal(Chunk.make [ 1; 2; 3 ], Chunk.compact (Chunk.make [ Some 1; Some 2; Some 3 ]))
    Assert.Equal(Chunk.make [ 1; 3 ], Chunk.compact (Chunk.make [ Some 1; None; Some 3 ]))

[<Fact>]
let ``dedupeAdjacent`` () =
    Assert.Equal(Chunk.empty, Chunk.dedupeAdjacent Chunk.empty)
    Assert.Equal(Chunk.make [ 1; 2; 3 ], Chunk.dedupeAdjacent (Chunk.make [ 1; 2; 3 ]))
    Assert.Equal(Chunk.make [ 1; 2; 3 ], Chunk.dedupeAdjacent (Chunk.make [ 1; 2; 2; 3; 3 ]))

[<Fact>]
let ``flatMap`` () =
    Assert.Equal(Chunk.make [ 1; 2 ], Chunk.flatMap (Chunk.make [ 1 ]) (fun n _ -> Chunk.make [ n; n + 1 ]))
    Assert.Equal(
        Chunk.make [ 1; 2; 2; 3; 3; 4 ],
        Chunk.flatMap (Chunk.make [ 1; 2; 3 ]) (fun n _ -> Chunk.make [ n; n + 1 ]))

[<Fact>]
let ``flatMap passes the element index`` () =
    // parity with map/forEach: f gets (a, index)
    Assert.Equal(
        Chunk.make [ 0; 10; 1; 11; 2; 12 ],
        Chunk.flatMap (Chunk.make [ 0; 1; 2 ]) (fun n i -> Chunk.make [ i; n + 10 ]))

[<Fact>]
let ``union`` () =
    Assert.Equal(Chunk.make [ 1; 2; 3 ], Chunk.union (Chunk.make [ 1; 2; 3 ]) Chunk.empty)
    Assert.Equal(Chunk.make [ 1; 2; 3 ], Chunk.union Chunk.empty (Chunk.make [ 1; 2; 3 ]))
    Assert.Equal(Chunk.make [ 1; 2; 3; 4 ], Chunk.union (Chunk.make [ 1; 2; 3 ]) (Chunk.make [ 2; 3; 4 ]))

[<Fact>]
let ``intersection`` () =
    Assert.Equal(Chunk.empty, Chunk.intersection (Chunk.make [ 1; 2; 3 ]) Chunk.empty)
    Assert.Equal(Chunk.empty, Chunk.intersection Chunk.empty (Chunk.make [ 2; 3; 4 ]))
    Assert.Equal(Chunk.make [ 2; 3 ], Chunk.intersection (Chunk.make [ 1; 2; 3 ]) (Chunk.make [ 2; 3; 4 ]))

[<Fact>]
let ``isEmpty`` () =
    Assert.True(Chunk.isEmpty Chunk.empty)
    Assert.False(Chunk.isEmpty (Chunk.make [ 1 ]))

[<Fact>]
let ``lastUnsafe`` () =
    Assert.Equal(1, Chunk.lastUnsafe (Chunk.make [ 1 ]))
    Assert.Equal(3, Chunk.lastUnsafe (Chunk.make [ 1; 2; 3 ]))
    let ex = Assert.Throws<System.Exception>(fun () -> Chunk.lastUnsafe Chunk.empty |> ignore)
    Assert.Equal("Index out of bounds: -1", ex.Message)

[<Fact>]
let ``splitNonEmptyAt`` () =
    Assert.Equal((Chunk.make [ 1; 2 ], Chunk.make [ 3; 4 ]), Chunk.splitNonEmptyAt (Chunk.make [ 1; 2; 3; 4 ]) 2)
    Assert.Equal((Chunk.make [ 1; 2; 3; 4 ], Chunk.empty), Chunk.splitNonEmptyAt (Chunk.make [ 1; 2; 3; 4 ]) 10)

[<Fact>]
let ``splitWhere`` () =
    Assert.Equal((Chunk.empty, Chunk.empty), Chunk.splitWhere Chunk.empty (fun n -> n > 1))
    Assert.Equal((Chunk.make [ 1 ], Chunk.make [ 2; 3 ]), Chunk.splitWhere (Chunk.make [ 1; 2; 3 ]) (fun n -> n > 1))

[<Fact>]
let ``takeWhile`` () =
    Assert.Equal(Chunk.empty, Chunk.takeWhile (fun n -> n <= 2) Chunk.empty)
    Assert.Equal(Chunk.make [ 1; 2 ], Chunk.takeWhile (fun n -> n <= 2) (Chunk.make [ 1; 2; 3 ]))

[<Fact>]
let ``dedupe`` () =
    Assert.Equal(Chunk.empty, Chunk.dedupe Chunk.empty)
    Assert.Equal(Chunk.make [ 1; 2; 3 ], Chunk.dedupe (Chunk.make [ 1; 2; 3 ]))
    Assert.Equal(Chunk.make [ 1; 2; 3 ], Chunk.dedupe (Chunk.make [ 1; 2; 3; 2; 1; 3 ]))

[<Fact>]
let ``unzip`` () =
    Assert.Equal((Chunk.empty, Chunk.empty), Chunk.unzip Chunk.empty)
    Assert.Equal(
        (Chunk.make [ "a"; "b" ], Chunk.make [ 1; 2 ]),
        Chunk.unzip (Chunk.make [ ("a", 1); ("b", 2) ]))

[<Fact>]
let ``reverse`` () =
    Assert.Equal(Chunk.empty, Chunk.reverse Chunk.empty)
    Assert.Equal(Chunk.make [ 3; 2; 1 ], Chunk.reverse (Chunk.make [ 1; 2; 3 ]))
    Assert.Equal(Chunk.make [ 3; 2; 1 ], Chunk.reverse (Chunk.take 3 (Chunk.make [ 1; 2; 3; 4 ])))

[<Fact>]
let ``flatten`` () =
    Assert.Equal(
        Chunk.make [ 1; 2; 3 ],
        Chunk.flatten (Chunk.make [ Chunk.make [ 1 ]; Chunk.make [ 2 ]; Chunk.make [ 3 ] ]))

[<Fact>]
let ``makeBy`` () =
    Assert.Equal(Chunk.make [ 0; 2; 4; 6; 8 ], Chunk.makeBy 5 (fun n -> n * 2))
    Assert.Equal(Chunk.make [ 0; 2 ], Chunk.makeBy 2 (fun n -> n * 2))

[<Fact>]
let ``range`` () =
    Assert.Equal(Chunk.make [ 0 ], Chunk.range 0 0)
    Assert.Equal(Chunk.make [ 0; 1 ], Chunk.range 0 1)
    Assert.Equal(Chunk.make [ 1; 2; 3; 4; 5 ], Chunk.range 1 5)
    Assert.Equal(Chunk.make [ 10; 11; 12; 13; 14; 15 ], Chunk.range 10 15)
    Assert.Equal(Chunk.make [ -1; 0 ], Chunk.range -1 0)
    Assert.Equal(Chunk.make [ -5; -4; -3; -2; -1 ], Chunk.range -5 -1)
    // out of bound
    Assert.Equal(Chunk.make [ 2 ], Chunk.range 2 1)
    Assert.Equal(Chunk.make [ -1 ], Chunk.range -1 -2)

[<Fact>]
let ``some`` () =
    let isPositive n = n > 0
    Assert.True(Chunk.some (Chunk.make [ -1; -2; 3 ]) isPositive)
    Assert.False(Chunk.some (Chunk.make [ -1; -2; -3 ]) isPositive)

[<Fact>]
let ``forEach`` () =
    let acc = ResizeArray<string>()
    Chunk.forEach (Chunk.make [ 1; 2; 3; 4 ]) (fun n i -> acc.Add(sprintf "%d-%d" n i))
    Assert.Equal<string[]>([| "1-0"; "2-1"; "3-2"; "4-3" |], acc.ToArray())

[<Fact>]
let ``sortWith`` () =
    let chunk = Chunk.make [ {| a = "a"; b = 2 |}; {| a = "b"; b = 1 |} ]
    Assert.Equal(
        Chunk.make [ {| a = "b"; b = 1 |}; {| a = "a"; b = 2 |} ],
        Chunk.sortWith chunk (fun x -> x.b) compare)

[<Fact>]
let ``makeEquivalence`` () =
    let equivalence = Chunk.makeEquivalence (fun (a: int) b -> a = b)
    Assert.True(equivalence Chunk.empty Chunk.empty)
    Assert.True(equivalence (Chunk.make [ 1; 2; 3 ]) (Chunk.make [ 1; 2; 3 ]))
    Assert.False(equivalence (Chunk.make [ 1; 2; 3 ]) (Chunk.make [ 1; 2 ]))
    Assert.False(equivalence (Chunk.make [ 1; 2; 3 ]) (Chunk.make [ 1; 2; 4 ]))

[<Fact>]
let ``differenceWith`` () =
    let eq (a: {| id: int |}) (b: {| id: int |}) = a.id = b.id
    let chunk = Chunk.make [ {| id = 1 |}; {| id = 2 |}; {| id = 3 |} ]
    Assert.Equal(
        Chunk.make [ {| id = 3 |} ],
        Chunk.differenceWith eq chunk (Chunk.make [ {| id = 1 |}; {| id = 2 |} ]))
    Assert.Equal(Chunk.empty, Chunk.differenceWith eq Chunk.empty chunk)
    Assert.Equal(chunk, Chunk.differenceWith eq chunk Chunk.empty)
    Assert.Equal(Chunk.empty, Chunk.differenceWith eq chunk chunk)

[<Fact>]
let ``difference`` () =
    let curr = Chunk.make [ 1; 3; 5; 7; 9 ]
    Assert.Equal(Chunk.make [ 7; 9 ], Chunk.difference curr (Chunk.make [ 1; 2; 3; 4; 5 ]))
    Assert.Equal(Chunk.empty, Chunk.difference Chunk.empty curr)
    Assert.Equal(curr, Chunk.difference curr Chunk.empty)
    Assert.Equal(Chunk.empty, Chunk.difference curr curr)

// --- wave-3 gap-fill combinators ---
// No dedicated upstream test cases exist for these; the assertions are ported
// from the `console.log` examples in the upstream Chunk.ts doc comments.

[<Fact>]
let ``headUnsafe`` () =
    Assert.Equal(1, Chunk.headUnsafe (Chunk.make [ 1; 2; 3 ]))
    Assert.Throws<System.Exception>(fun () -> Chunk.headUnsafe (Chunk.empty: Chunk<int>) |> ignore)
    |> ignore

[<Fact>]
let ``takeRight`` () =
    let chunk = Chunk.make [ 1; 2; 3; 4; 5 ]
    Assert.Equal(Chunk.make [ 4; 5 ], Chunk.takeRight 2 chunk)
    Assert.Equal(chunk, Chunk.takeRight 10 chunk)
    Assert.Equal(Chunk.empty, Chunk.takeRight 0 chunk)
    Assert.Equal(Chunk.empty, Chunk.takeRight -1 chunk)

[<Fact>]
let ``sort`` () =
    Assert.Equal(Chunk.make [ 1; 2; 3; 4; 5 ], Chunk.sort (Chunk.make [ 3; 1; 4; 5; 2 ]) compare)
    Assert.Equal(Chunk.empty, Chunk.sort (Chunk.empty: Chunk<int>) compare)

[<Fact>]
let ``contains`` () =
    let chunk = Chunk.make [ 1; 2; 3; 4; 5 ]
    Assert.True(Chunk.contains chunk 3)
    Assert.False(Chunk.contains chunk 10)
    Assert.False(Chunk.contains (Chunk.empty: Chunk<int>) 1)

[<Fact>]
let ``containsWith`` () =
    let chunk = Chunk.make [ {| id = 1; name = "Alice" |}; {| id = 2; name = "Bob" |} ]
    let byId (a: {| id: int; name: string |}) (b: {| id: int; name: string |}) = a.id = b.id
    Assert.True(Chunk.containsWith byId chunk {| id = 1; name = "Different" |})
    Assert.False(Chunk.containsWith byId chunk {| id = 3; name = "Charlie" |})

[<Fact>]
let ``findFirst`` () =
    let chunk = Chunk.make [ 1; 2; 3; 4; 5 ]
    Assert.Equal(Some 4, Chunk.findFirst chunk (fun n -> n > 3))
    Assert.Equal(None, Chunk.findFirst chunk (fun n -> n > 10))

[<Fact>]
let ``findFirstIndex`` () =
    let chunk = Chunk.make [ 1; 2; 3; 4; 5 ]
    Assert.Equal(Some 3, Chunk.findFirstIndex chunk (fun n -> n > 3))
    Assert.Equal(Some 1, Chunk.findFirstIndex chunk (fun n -> n % 2 = 0))
    Assert.Equal(None, Chunk.findFirstIndex chunk (fun n -> n > 10))

[<Fact>]
let ``findLast`` () =
    let chunk = Chunk.make [ 1; 2; 3; 4; 5 ]
    Assert.Equal(Some 3, Chunk.findLast chunk (fun n -> n < 4))
    Assert.Equal(Some 4, Chunk.findLast chunk (fun n -> n % 2 = 0))
    Assert.Equal(None, Chunk.findLast chunk (fun n -> n > 10))

[<Fact>]
let ``findLastIndex`` () =
    let chunk = Chunk.make [ 1; 2; 3; 4; 5 ]
    Assert.Equal(Some 2, Chunk.findLastIndex chunk (fun n -> n < 4))
    Assert.Equal(Some 3, Chunk.findLastIndex chunk (fun n -> n % 2 = 0))
    Assert.Equal(None, Chunk.findLastIndex chunk (fun n -> n > 10))

[<Fact>]
let ``every`` () =
    let chunk = Chunk.make [ 1; 2; 3; 4; 5 ]
    Assert.True(Chunk.every chunk (fun n -> n > 0))
    Assert.False(Chunk.every chunk (fun n -> n > 3))
    Assert.True(Chunk.every (Chunk.empty: Chunk<int>) (fun n -> n > 0)) // vacuously true

[<Fact>]
let ``reduce`` () =
    Assert.Equal(15, Chunk.reduce (Chunk.make [ 1; 2; 3; 4; 5 ]) 0 (fun acc n _ -> acc + n))
    // index-aware, left to right
    Assert.Equal("0:a 1:b 2:c ", Chunk.reduce (Chunk.make [ "a"; "b"; "c" ]) "" (fun acc w i -> acc + sprintf "%d:%s " i w))

[<Fact>]
let ``reduceRight`` () =
    Assert.Equal(10, Chunk.reduceRight (Chunk.make [ 1; 2; 3; 4 ]) 0 (fun acc n _ -> acc + n))
    // right to left, with original index
    Assert.Equal("2:c 1:b 0:a ", Chunk.reduceRight (Chunk.make [ "a"; "b"; "c" ]) "" (fun acc w i -> acc + sprintf "%d:%s " i w))

[<Fact>]
let ``join`` () =
    Assert.Equal("apple, banana, cherry", Chunk.join (Chunk.make [ "apple"; "banana"; "cherry" ]) ", ")
    Assert.Equal("", Chunk.join (Chunk.empty: Chunk<string>) ", ")
    Assert.Equal("hello", Chunk.join (Chunk.make [ "hello" ]) ", ")
