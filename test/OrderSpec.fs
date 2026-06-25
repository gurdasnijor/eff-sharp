module OrderSpec

open Effect
open Effect.Vitest

let private same actual expected =
    toBe (actual = expected) true

describe "Order" (fun () ->
    test "struct-like ordering and tuple2" (fun () ->
        let byA = Order.String |> Order.mapInput (fun (a, _b) -> a)
        let byB = Order.String |> Order.mapInput (fun (_a, b) -> b)
        let structOrder = Order.combine byB byA
        toBe (structOrder ("a", "b") ("a", "c")) -1
        toBe (structOrder ("a", "b") ("a", "b")) 0
        toBe (structOrder ("a", "c") ("a", "b")) 1

        let tupleOrder = Order.tuple2 Order.String Order.String
        toBe (tupleOrder ("a", "b") ("a", "c")) -1
        toBe (tupleOrder ("a", "b") ("a", "b")) 0
        toBe (tupleOrder ("a", "b") ("a", "a")) 1
        toBe (tupleOrder ("a", "b") ("b", "a")) -1)

    test "mapInput and primitive orders" (fun () ->
        let byLength = Order.Number |> Order.mapInput (fun (s: string) -> float s.Length)
        toBe (byLength "a" "b") 0
        toBe (byLength "a" "bb") -1
        toBe (byLength "aa" "b") 1

        let o = Order.Number
        toBe (o 1.0 1.0) 0
        toBe (o 1.0 2.0) -1
        toBe (o 2.0 1.0) 1
        toBe (o 0.0 -0.0) 0
        toBe (o nan nan) 0
        toBe (o nan 1.0) -1
        toBe (o 1.0 nan) 1)

    test "Date compares by timestamp" (fun () ->
        let epoch = System.DateTime(2020, 1, 1)
        toBe (Order.Date epoch (epoch.AddMilliseconds 1.0)) -1
        toBe (Order.Date epoch epoch) 0
        toBe (Order.Date (epoch.AddMilliseconds 1.0) epoch) 1)

    test "clamp and bounds predicates" (fun () ->
        let clamp x = Order.clamp Order.Number 1.0 10.0 x
        toBe (clamp 2.0) 2.0
        toBe (clamp 20.0) 10.0
        toBe (clamp -10.0) 1.0

        let between x = Order.isBetween Order.Number 1.0 10.0 x
        toBe (between 2.0) true
        toBe (between 10.0) true
        toBe (between 20.0) false
        toBe (between -10.0) false)

    test "flip and comparison predicates" (fun () ->
        let flipped = Order.flip Order.Number
        toBe (flipped 1.0 2.0) 1
        toBe (flipped 2.0 1.0) -1
        toBe (flipped 2.0 2.0) 0
        toBe (Order.isLessThan Order.Number 0.0 1.0) true
        toBe (Order.isLessThan Order.Number 1.0 1.0) false
        toBe (Order.isLessThanOrEqualTo Order.Number 1.0 1.0) true
        toBe (Order.isGreaterThan Order.Number 2.0 1.0) true
        toBe (Order.isGreaterThan Order.Number 1.0 1.0) false
        toBe (Order.isGreaterThanOrEqualTo Order.Number 1.0 1.0) true)

    test "min and max return the first value when equal" (fun () ->
        let order = Order.Number |> Order.mapInput (fun (r: int ref) -> float r.Value)
        let first = ref 1
        let second = ref 1
        same (Order.min order (ref 1) (ref 2)).Value 1
        same (Order.min order (ref 2) (ref 1)).Value 1
        toBe (obj.ReferenceEquals(first, Order.min order first second)) true
        same (Order.max order (ref 1) (ref 2)).Value 2
        same (Order.max order (ref 2) (ref 1)).Value 2
        toBe (obj.ReferenceEquals(first, Order.max order first second)) true)

    test "combine and array orders" (fun () ->
        let tuples = [ (2, "c"); (1, "b"); (2, "a"); (1, "c") ]
        let byFst = Order.Number |> Order.mapInput (fun (x, _) -> float x)
        let bySnd = Order.String |> Order.mapInput (fun (_, y) -> y)
        same (tuples |> List.sortWith (Order.combine bySnd byFst)) [ (1, "b"); (1, "c"); (2, "a"); (2, "c") ]
        same (tuples |> List.sortWith (Order.combine byFst bySnd)) [ (2, "a"); (1, "b"); (1, "c"); (2, "c") ]

        let arrayOrder = Order.Array Order.Number
        toBe (arrayOrder [ 1.0; 2.0 ] [ 1.0; 3.0 ]) -1
        toBe (arrayOrder [ 1.0; 2.0 ] [ 1.0; 2.0; 3.0 ]) -1
        toBe (arrayOrder [ 1.0; 2.0; 3.0 ] [ 1.0; 2.0 ]) 1
        toBe (arrayOrder [ 1.0; 2.0 ] [ 1.0; 2.0 ]) 0))
