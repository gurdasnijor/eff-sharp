module Effect.Tests.HashTests

open Xunit
open Effect

// No upstream Hash.test.ts exists; these Facts pin the ported numeric
// algorithm's invariants (determinism + the documented formulas) from
// repos/effect-smol/packages/effect/src/Hash.ts.

[<Fact>]
let ``string is deterministic`` () =
    Assert.Equal(Hash.string "test", Hash.string "test")
    Assert.NotEqual(Hash.string "hello", Hash.string "world")

[<Fact>]
let ``combine matches (self * 53) xor b`` () =
    let a = Hash.string "hello"
    let b = Hash.string "world"
    Assert.Equal((a * 53) ^^^ b, Hash.combine b a)

[<Fact>]
let ``optimize matches the bit formula`` () =
    let n = 1234567890
    Assert.Equal((n &&& 0xbfffffff) ||| (int (uint32 n >>> 1) &&& 0x40000000), Hash.optimize n)

[<Fact>]
let ``number is deterministic and handles specials`` () =
    Assert.Equal(Hash.number 100.0, Hash.number 100.0)
    Assert.Equal(Hash.string "NaN", Hash.number nan)
    Assert.Equal(Hash.string "Infinity", Hash.number System.Double.PositiveInfinity)
    Assert.Equal(Hash.string "-Infinity", Hash.number System.Double.NegativeInfinity)

[<Fact>]
let ``array hashes equal sequences equally and is order-insensitive (XOR)`` () =
    let h xs =
        Hash.array (xs |> List.map (fun n -> Hash.number (float n)))

    Assert.Equal(h [ 1; 2; 3 ], h [ 1; 2; 3 ])
    // XOR fold => reordered inputs collide (documented gotcha).
    Assert.Equal(h [ 1; 2; 3 ], h [ 3; 2; 1 ])

[<Fact>]
let ``hash dispatches primitives to the numeric/string hashes`` () =
    Assert.Equal(Hash.string "hello", Hash.hash (box "hello"))
    Assert.Equal(Hash.number 42.0, Hash.hash (box 42.0))
    Assert.Equal(Hash.string "null", Hash.hash null)

[<Fact>]
let ``hash is stable for structurally equal composites`` () =
    Assert.Equal(Hash.hash (box [ 1; 2; 3 ]), Hash.hash (box [ 1; 2; 3 ]))
    Assert.Equal(Hash.hash (box {| name = "John"; age = 30 |}), Hash.hash (box {| name = "John"; age = 30 |}))
