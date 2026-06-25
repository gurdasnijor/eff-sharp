module HashSpec

open Effect
open Effect.Vitest

describe "Hash" (fun () ->
    test "string is deterministic" (fun () ->
        toBe (Hash.string "test") (Hash.string "test")
        toBe (Hash.string "hello" <> Hash.string "world") true)

    test "combine matches (self * 53) xor b" (fun () ->
        let a = Hash.string "hello"
        let b = Hash.string "world"
        toBe (Hash.combine b a) ((a * 53) ^^^ b))

    test "optimize matches the bit formula" (fun () ->
        let n = 1234567890
        let expected = (n &&& 0xbfffffff) ||| (int (uint32 n >>> 1) &&& 0x40000000)
        toBe (Hash.optimize n) expected)

    test "number is deterministic and handles specials" (fun () ->
        toBe (Hash.number 100.0) (Hash.number 100.0)
        toBe (Hash.number nan) (Hash.string "NaN")
        toBe (Hash.number System.Double.PositiveInfinity) (Hash.string "Infinity")
        toBe (Hash.number System.Double.NegativeInfinity) (Hash.string "-Infinity"))

    test "array hashes equal sequences equally and is order-insensitive" (fun () ->
        let h xs = Hash.array (xs |> List.map (fun n -> Hash.number (float n)))
        toBe (h [ 1; 2; 3 ]) (h [ 1; 2; 3 ])
        toBe (h [ 1; 2; 3 ]) (h [ 3; 2; 1 ]))

    test "hash dispatches primitives to the numeric/string hashes" (fun () ->
        toBe (Hash.hash (box "hello")) (Hash.string "hello")
        toBe (Hash.hash (box 42.0)) (Hash.number 42.0)
        toBe (Hash.hash null) (Hash.string "null"))

    test "hash is stable for structurally equal composites" (fun () ->
        toBe (Hash.hash (box [ 1; 2; 3 ])) (Hash.hash (box [ 1; 2; 3 ]))
        toBe (Hash.hash (box {| name = "John"; age = 30 |})) (Hash.hash (box {| name = "John"; age = 30 |}))))

