module Effect.Tests.MutableHashMapTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/MutableHashMap.test.ts
//
// The upstream `Key`/`Value` classes implement Effect's `Equal`/`Hash`; here
// they are plain F# records, which get structural equality/hashing for free —
// exactly what the backing structural `Dictionary` keys on. Tests that only
// exercise JS-runtime machinery (`isMutableHashMap`, `toJSON`, `inspect`, and
// the JS `pipe`) are intentionally not ported.

type Key = { A: int; B: int }
type Value = { C: int; D: int }

let private key a b = { A = a; B = b }
let private value c d = { C = c; D = d }

[<Fact>]
let ``toString renders entries`` () =
    let map = MutableHashMap.make [ (0, "a"); (1, "b") ]
    Assert.Equal("MutableHashMap([[0,\"a\"],[1,\"b\"]])", MutableHashMap.render map)

[<Fact>]
let ``make`` () =
    let map = MutableHashMap.make [ (key 0 0, value 0 0); (key 1 1, value 1 1) ]
    Assert.Equal(2, MutableHashMap.size map)
    Assert.True(MutableHashMap.has map (key 0 0))
    Assert.True(MutableHashMap.has map (key 1 1))

[<Fact>]
let ``fromIterable`` () =
    let map = MutableHashMap.fromIterable [ (key 0 0, value 0 0); (key 1 1, value 1 1) ]
    Assert.Equal(2, MutableHashMap.size map)
    Assert.True(MutableHashMap.has map (key 0 0))
    Assert.True(MutableHashMap.has map (key 1 1))

[<Fact>]
let ``iterate keeps distinct reference keys`` () =
    // Two distinct objects compare by reference (no structural equality), so both
    // are retained — mirrors upstream's `Hello` class with identity equality.
    let a = obj ()
    let b = obj ()
    let map = MutableHashMap.make [ (a, 0); (b, 0) ]
    Assert.Equal(2, Seq.length (MutableHashMap.toSeq map))

[<Fact>]
let ``get returns the latest value for an equal key`` () =
    let map = MutableHashMap.empty ()
    MutableHashMap.set map (key 0 0) (value 0 0) |> ignore
    MutableHashMap.set map (key 0 0) (value 1 1) |> ignore
    Assert.Equal(Some(value 1 1), MutableHashMap.get map (key 0 0))

[<Fact>]
let ``has`` () =
    let map =
        MutableHashMap.make
            [ (key 0 0, value 0 0)
              (key 0 0, value 1 1)
              (key 1 1, value 2 2)
              (key 1 1, value 3 3)
              (key 0 0, value 4 4) ]

    Assert.True(MutableHashMap.has map (key 0 0))
    Assert.True(MutableHashMap.has map (key 1 1))
    Assert.False(MutableHashMap.has map (key 4 4))

[<Fact>]
let ``keys`` () =
    let map = MutableHashMap.empty ()
    MutableHashMap.set map (key 0 0) (value 0 0) |> ignore
    MutableHashMap.set map (key 1 1) (value 1 1) |> ignore
    Assert.Equal<Key list>([ key 0 0; key 1 1 ], List.ofSeq (MutableHashMap.keys map))

[<Fact>]
let ``modifyAt updates, inserts and removes`` () =
    let map = MutableHashMap.empty ()
    MutableHashMap.set map (key 0 0) (value 0 0) |> ignore
    MutableHashMap.set map (key 1 1) (value 1 1) |> ignore

    MutableHashMap.modifyAt map (key 0 0) (fun _ -> Some(value 0 1)) |> ignore
    Assert.Equal(2, MutableHashMap.size map)
    Assert.Equal(Some(value 0 1), MutableHashMap.get map (key 0 0))

    MutableHashMap.modifyAt map (key 2 2) (function
        | None -> Some(value 2 2)
        | some -> some)
    |> ignore

    Assert.Equal(3, MutableHashMap.size map)
    Assert.Equal(Some(value 2 2), MutableHashMap.get map (key 2 2))

    MutableHashMap.modifyAt map (key 2 2) (fun _ -> None) |> ignore
    Assert.Equal(2, MutableHashMap.size map)

[<Fact>]
let ``remove deletes an equal key in place`` () =
    let map = MutableHashMap.empty ()
    MutableHashMap.set map (key 0 0) (value 0 0) |> ignore
    MutableHashMap.set map (key 1 1) (value 1 1) |> ignore

    Assert.Equal(2, MutableHashMap.size map)
    Assert.True(MutableHashMap.has map (key 1 1))

    MutableHashMap.remove map (key 1 1) |> ignore

    Assert.Equal(1, MutableHashMap.size map)
    Assert.False(MutableHashMap.has map (key 1 1))

[<Fact>]
let ``set overwrites equal keys without changing size`` () =
    let map = MutableHashMap.empty ()
    MutableHashMap.set map (key 0 0) (value 0 0) |> ignore
    MutableHashMap.set map (key 0 0) (value 1 1) |> ignore
    MutableHashMap.set map (key 1 1) (value 2 2) |> ignore
    MutableHashMap.set map (key 1 1) (value 3 3) |> ignore
    MutableHashMap.set map (key 0 0) (value 4 4) |> ignore

    Assert.Equal<(Key * Value) list>(
        [ (key 0 0, value 4 4); (key 1 1, value 3 3) ],
        List.ofSeq (MutableHashMap.toSeq map)
    )

[<Fact>]
let ``size`` () =
    let map = MutableHashMap.empty ()
    MutableHashMap.set map (key 0 0) (value 0 0) |> ignore
    MutableHashMap.set map (key 0 0) (value 1 1) |> ignore
    MutableHashMap.set map (key 1 1) (value 2 2) |> ignore
    MutableHashMap.set map (key 1 1) (value 3 3) |> ignore
    MutableHashMap.set map (key 0 0) (value 4 4) |> ignore
    Assert.Equal(2, MutableHashMap.size map)

[<Fact>]
let ``modify updates existing keys and ignores missing keys`` () =
    let map = MutableHashMap.empty ()
    MutableHashMap.set map (key 0 0) (value 0 0) |> ignore
    MutableHashMap.set map (key 1 1) (value 1 1) |> ignore

    MutableHashMap.modify map (key 0 0) (fun v -> value (v.C + 1) (v.D + 1)) |> ignore
    Assert.Equal(Some(value 1 1), MutableHashMap.get map (key 0 0))

    // Missing key (after removal) is left untouched.
    MutableHashMap.remove map (key 0 0) |> ignore
    Assert.Equal(None, MutableHashMap.get map (key 0 0))
