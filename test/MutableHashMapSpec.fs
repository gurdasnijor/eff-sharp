module MutableHashMapSpec

open Effect
open Effect.Vitest

type Key = { A: int; B: int }
type Value = { C: int; D: int }

let private key a b = { A = a; B = b }
let private value c d = { C = c; D = d }

describe "MutableHashMap" (fun () ->
    test "render renders entries" (fun () ->
        let map = MutableHashMap.make [ (0, "a"); (1, "b") ]
        toBe (MutableHashMap.render map) "MutableHashMap([[0,\"a\"],[1,\"b\"]])")

    test "make" (fun () ->
        let map = MutableHashMap.make [ (key 0 0, value 0 0); (key 1 1, value 1 1) ]
        toBe (MutableHashMap.size map) 2
        toBe (MutableHashMap.has map (key 0 0)) true
        toBe (MutableHashMap.has map (key 1 1)) true)

    test "fromIterable" (fun () ->
        let map = MutableHashMap.fromIterable [ (key 0 0, value 0 0); (key 1 1, value 1 1) ]
        toBe (MutableHashMap.size map) 2
        toBe (MutableHashMap.has map (key 0 0)) true
        toBe (MutableHashMap.has map (key 1 1)) true)

    test "get returns the latest value for an equal key" (fun () ->
        let map = MutableHashMap.empty ()
        MutableHashMap.set map (key 0 0) (value 0 0) |> ignore
        MutableHashMap.set map (key 0 0) (value 1 1) |> ignore
        toEqual (MutableHashMap.get map (key 0 0)) (Some(value 1 1)))

    test "has" (fun () ->
        let map =
            MutableHashMap.make
                [ (key 0 0, value 0 0)
                  (key 0 0, value 1 1)
                  (key 1 1, value 2 2)
                  (key 1 1, value 3 3)
                  (key 0 0, value 4 4) ]

        toBe (MutableHashMap.has map (key 0 0)) true
        toBe (MutableHashMap.has map (key 1 1)) true
        toBe (MutableHashMap.has map (key 4 4)) false)

    test "keys" (fun () ->
        let map = MutableHashMap.empty ()
        MutableHashMap.set map (key 0 0) (value 0 0) |> ignore
        MutableHashMap.set map (key 1 1) (value 1 1) |> ignore
        toEqual (List.ofSeq (MutableHashMap.keys map)) [ key 0 0; key 1 1 ])

    test "modifyAt updates, inserts and removes" (fun () ->
        let map = MutableHashMap.empty ()
        MutableHashMap.set map (key 0 0) (value 0 0) |> ignore
        MutableHashMap.set map (key 1 1) (value 1 1) |> ignore

        MutableHashMap.modifyAt map (key 0 0) (fun _ -> Some(value 0 1)) |> ignore
        toBe (MutableHashMap.size map) 2
        toEqual (MutableHashMap.get map (key 0 0)) (Some(value 0 1))

        MutableHashMap.modifyAt map (key 2 2) (function
            | None -> Some(value 2 2)
            | some -> some)
        |> ignore

        toBe (MutableHashMap.size map) 3
        toEqual (MutableHashMap.get map (key 2 2)) (Some(value 2 2))

        MutableHashMap.modifyAt map (key 2 2) (fun _ -> None) |> ignore
        toBe (MutableHashMap.size map) 2)

    test "remove deletes an equal key in place" (fun () ->
        let map = MutableHashMap.empty ()
        MutableHashMap.set map (key 0 0) (value 0 0) |> ignore
        MutableHashMap.set map (key 1 1) (value 1 1) |> ignore

        toBe (MutableHashMap.size map) 2
        toBe (MutableHashMap.has map (key 1 1)) true

        MutableHashMap.remove map (key 1 1) |> ignore

        toBe (MutableHashMap.size map) 1
        toBe (MutableHashMap.has map (key 1 1)) false)

    test "set overwrites equal keys without changing size" (fun () ->
        let map = MutableHashMap.empty ()
        MutableHashMap.set map (key 0 0) (value 0 0) |> ignore
        MutableHashMap.set map (key 0 0) (value 1 1) |> ignore
        MutableHashMap.set map (key 1 1) (value 2 2) |> ignore
        MutableHashMap.set map (key 1 1) (value 3 3) |> ignore
        MutableHashMap.set map (key 0 0) (value 4 4) |> ignore

        toEqual (List.ofSeq (MutableHashMap.toSeq map)) [ (key 0 0, value 4 4); (key 1 1, value 3 3) ])

    test "size" (fun () ->
        let map = MutableHashMap.empty ()
        MutableHashMap.set map (key 0 0) (value 0 0) |> ignore
        MutableHashMap.set map (key 0 0) (value 1 1) |> ignore
        MutableHashMap.set map (key 1 1) (value 2 2) |> ignore
        MutableHashMap.set map (key 1 1) (value 3 3) |> ignore
        MutableHashMap.set map (key 0 0) (value 4 4) |> ignore
        toBe (MutableHashMap.size map) 2)

    test "modify updates existing keys and ignores missing keys" (fun () ->
        let map = MutableHashMap.empty ()
        MutableHashMap.set map (key 0 0) (value 0 0) |> ignore
        MutableHashMap.set map (key 1 1) (value 1 1) |> ignore

        MutableHashMap.modify map (key 0 0) (fun v -> value (v.C + 1) (v.D + 1)) |> ignore

        toEqual (MutableHashMap.get map (key 0 0)) (Some(value 1 1))

        MutableHashMap.remove map (key 0 0) |> ignore
        toEqual (MutableHashMap.get map (key 0 0)) None))
