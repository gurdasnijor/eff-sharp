module HashMapSpec

open Effect
open Effect.Vitest

let private same actual expected = toBe (actual = expected) true

type private Person = { Name: string; Age: int }

describe "HashMap" (fun () ->
    test "empty has no entries" (fun () ->
        let map = HashMap.empty<string, int>
        toBe (HashMap.isEmpty map) true
        toBe (HashMap.size map) 0)

    test "make builds a map from pairs" (fun () ->
        let map = HashMap.make [ ("a", 1); ("b", 2); ("c", 3) ]
        toBe (HashMap.size map) 3
        same (HashMap.get map "a") (Some 1)
        same (HashMap.get map "b") (Some 2)
        same (HashMap.get map "c") (Some 3))

    test "fromIterable builds a map from a sequence" (fun () ->
        let map =
            HashMap.fromIterable (
                seq {
                    "a", 1
                    "b", 2
                    "c", 3
                }
            )

        toBe (HashMap.size map) 3
        same (HashMap.get map "a") (Some 1))

    test "get and has reflect membership" (fun () ->
        let map = HashMap.make [ ("a", 1); ("b", 2) ]
        same (HashMap.get map "c") None
        toBe (HashMap.has map "a") true
        toBe (HashMap.has map "c") false)

    test "set adds and overwrites without mutating the original" (fun () ->
        let map1 = HashMap.make [ ("a", 1) ]
        let map2 = HashMap.set map1 "b" 2
        toBe (HashMap.size map2) 2
        same (HashMap.get map2 "b") (Some 2)
        toBe (HashMap.size map1) 1
        toBe (HashMap.has map1 "b") false

        let map3 = HashMap.make [ ("a", 1); ("b", 2) ]
        let map4 = HashMap.set map3 "a" 10
        same (HashMap.get map4 "a") (Some 10)
        same (HashMap.get map3 "a") (Some 1))

    test "remove deletes existing keys without mutating the original" (fun () ->
        let map1 = HashMap.make [ ("a", 1); ("b", 2); ("c", 3) ]
        let map2 = HashMap.remove map1 "b"
        toBe (HashMap.size map2) 2
        toBe (HashMap.has map2 "b") false
        toBe (HashMap.size map1) 3
        toBe (HashMap.has map1 "b") true

        let map3 = HashMap.remove map1 "z"
        same map3 map1
        toBe (HashMap.size map3) 3)

    test "getUnsafe returns the value or throws" (fun () ->
        let map = HashMap.make [ ("a", 1); ("b", 2) ]
        toBe (HashMap.getUnsafe map "a") 1
        toThrow (fun () -> HashMap.getUnsafe map "z" |> ignore))

    test "keys values and entries expose the contents" (fun () ->
        let map = HashMap.make [ ("a", 1); ("b", 2); ("c", 3) ]
        same (HashMap.keys map |> List.ofSeq |> List.sort) [ "a"; "b"; "c" ]
        same (HashMap.values map |> List.ofSeq |> List.sort) [ 1; 2; 3 ]
        same (HashMap.toValues map |> List.sort) [ 1; 2; 3 ]
        same (HashMap.entries map |> List.ofSeq |> List.sort) [ ("a", 1); ("b", 2); ("c", 3) ]
        same (HashMap.toEntries map |> List.sort) [ ("a", 1); ("b", 2); ("c", 3) ])

    test "bulk updates remove insert and overwrite entries" (fun () ->
        let map1 = HashMap.make [ ("a", 1); ("b", 2); ("c", 3); ("d", 4) ]
        let map2 = HashMap.removeMany map1 [ "b"; "d" ]
        toBe (HashMap.size map2) 2
        toBe (HashMap.has map2 "a") true
        toBe (HashMap.has map2 "b") false
        toBe (HashMap.has map2 "c") true
        toBe (HashMap.has map2 "d") false

        let map3 = HashMap.make [ ("a", 1); ("b", 2) ]
        let map4 = HashMap.setMany map3 [ ("c", 3); ("d", 4); ("a", 10) ]
        toBe (HashMap.size map4) 4
        same (HashMap.get map4 "a") (Some 10)
        same (HashMap.get map4 "b") (Some 2)
        same (HashMap.get map4 "c") (Some 3)
        same (HashMap.get map4 "d") (Some 4))

    test "union prefers the second map on collisions" (fun () ->
        let map1 = HashMap.make [ ("a", 1); ("b", 2) ]
        let map2 = HashMap.make [ ("b", 20); ("c", 3) ]
        let map3 = HashMap.union map1 map2
        toBe (HashMap.size map3) 3
        same (HashMap.get map3 "a") (Some 1)
        same (HashMap.get map3 "b") (Some 20)
        same (HashMap.get map3 "c") (Some 3))

    test "map transforms values" (fun () ->
        let map1 = HashMap.make [ ("a", 1); ("b", 2); ("c", 3) ]
        let map2 = HashMap.map map1 (fun value _key -> value * 2)
        same (HashMap.get map2 "a") (Some 2)
        same (HashMap.get map2 "b") (Some 4)
        same (HashMap.get map2 "c") (Some 6))

    test "flatMap expands and flattens" (fun () ->
        let map1 = HashMap.make [ ("a", 1); ("b", 2) ]
        let map2 = HashMap.flatMap map1 (fun value key -> HashMap.make [ (key + "1", value); (key + "2", value * 2) ])
        toBe (HashMap.size map2) 4
        same (HashMap.get map2 "a1") (Some 1)
        same (HashMap.get map2 "a2") (Some 2)
        same (HashMap.get map2 "b1") (Some 2)
        same (HashMap.get map2 "b2") (Some 4))

    test "filter compact and filterMap keep matching entries" (fun () ->
        let map1 = HashMap.make [ ("a", 1); ("b", 2); ("c", 3); ("d", 4) ]
        let map2 = HashMap.filter map1 (fun value _key -> value % 2 = 0)
        toBe (HashMap.size map2) 2
        toBe (HashMap.has map2 "b") true
        toBe (HashMap.has map2 "d") true
        toBe (HashMap.has map2 "a") false

        let map3 = HashMap.make [ ("a", Some 1); ("b", None); ("c", Some 3) ]
        let map4 = HashMap.compact map3
        toBe (HashMap.size map4) 2
        same (HashMap.get map4 "a") (Some 1)
        toBe (HashMap.has map4 "b") false
        same (HashMap.get map4 "c") (Some 3)

        let map5 = HashMap.filterMap map1 (fun value _key -> if value % 2 = 0 then Ok(value * 2) else Error())
        toBe (HashMap.size map5) 2
        same (HashMap.get map5 "b") (Some 4)
        same (HashMap.get map5 "d") (Some 8)

        let map6 = HashMap.filterMap map1 (fun value key -> if key < "c" then Ok(sprintf "%s:%d" key value) else Error())
        toBe (HashMap.size map6) 2
        same (HashMap.get map6 "a") (Some "a:1")
        same (HashMap.get map6 "b") (Some "b:2")
        toBe (HashMap.has map6 "c") false)

    test "search operations inspect entries" (fun () ->
        let map = HashMap.make [ ("a", 1); ("b", 2); ("c", 3) ]
        same (HashMap.findFirst map (fun value _key -> value = 3)) (Some("c", 3))
        same (HashMap.findFirst map (fun value _key -> value > 5)) None
        toBe (HashMap.some map (fun value _key -> value > 2)) true
        toBe (HashMap.some map (fun value _key -> value > 5)) false
        toBe (HashMap.every map (fun value _key -> value > 0)) true
        toBe (HashMap.every map (fun value _key -> value > 1)) false
        toBe (HashMap.hasBy map (fun value key -> key = "b" && value = 2)) true
        toBe (HashMap.hasBy map (fun value key -> key = "b" && value = 5)) false)

    test "modifyAt updates inserts and removes" (fun () ->
        let map1 = HashMap.make [ ("a", 1); ("b", 2) ]

        let updated =
            HashMap.modifyAt map1 "a" (function
                | Some v -> Some(v * 2)
                | None -> None)

        same (HashMap.get updated "a") (Some 2)

        let inserted =
            HashMap.modifyAt (HashMap.make [ ("a", 1) ]) "b" (function
                | Some v -> Some v
                | None -> Some 10)

        same (HashMap.get inserted "b") (Some 10)

        let removed = HashMap.modifyAt map1 "a" (fun _ -> None)
        toBe (HashMap.has removed "a") false
        toBe (HashMap.has removed "b") true)

    test "modify updates an existing key" (fun () ->
        let map1 = HashMap.make [ ("a", 1); ("b", 2) ]
        let map2 = HashMap.modify map1 "a" (fun value -> value * 3)
        same (HashMap.get map2 "a") (Some 3)
        same (HashMap.get map2 "b") (Some 2))

    test "reduce folds values and forEach visits entries" (fun () ->
        let map = HashMap.make [ ("a", 1); ("b", 2); ("c", 3) ]
        toBe (HashMap.reduce map 0 (fun acc value _key -> acc + value)) 6

        let collected = ResizeArray<string * int>()
        HashMap.forEach (HashMap.make [ ("a", 1); ("b", 2) ]) (fun value key -> collected.Add(key, value))
        same (collected |> List.ofSeq |> List.sort) [ ("a", 1); ("b", 2) ])

    test "structural equality and hash ignore insertion order" (fun () ->
        let map1 = HashMap.make [ ("a", 1); ("b", 2) ]
        let map2 = HashMap.make [ ("b", 2); ("a", 1) ]
        let map3 = HashMap.make [ ("a", 1); ("b", 3) ]
        toBe (map1 = map2) true
        toBe (map1 = map3) false
        toBe (hash map1) (hash map2))

    test "structurally equal record keys address the same entry" (fun () ->
        let alice1 = { Name = "Alice"; Age = 25 }
        let alice2 = { Name = "Alice"; Age = 25 }
        let bob = { Name = "Bob"; Age = 30 }

        let map = HashMap.make [ (alice1, "value1"); (bob, "value3") ]
        same (HashMap.get map alice2) (Some "value1")
        toBe (HashMap.has map alice2) true

        let map2 = HashMap.set map alice2 "updated"
        same (HashMap.get map2 alice1) (Some "updated")
        toBe (HashMap.size map2) 2)

    test "handles many inserts lookups and removals" (fun () ->
        let mutable map = HashMap.empty<int, string>

        for i in 0..999 do
            map <- HashMap.set map i (sprintf "value%d" i)

        toBe (HashMap.size map) 1000
        same (HashMap.get map 42) (Some "value42")

        for i in 0..499 do
            map <- HashMap.remove map i

        toBe (HashMap.size map) 500
        toBe (HashMap.has map 0) false
        toBe (HashMap.has map 999) true))
