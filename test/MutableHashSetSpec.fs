module MutableHashSetSpec

open Effect
open Effect.Vitest

describe "MutableHashSet" (fun () ->
    test "make and fromIterable de-duplicate" (fun () ->
        let set = MutableHashSet.make [ "apple"; "banana"; "apple"; "cherry" ]
        toBe (MutableHashSet.size set) 3
        toEqual (List.ofSeq (MutableHashSet.toSeq set)) [ "apple"; "banana"; "cherry" ])

    test "add ignores duplicates" (fun () ->
        let set = MutableHashSet.empty ()
        MutableHashSet.add set "apple" |> ignore
        MutableHashSet.add set "banana" |> ignore
        MutableHashSet.add set "apple" |> ignore
        toBe (MutableHashSet.size set) 2
        toBe (MutableHashSet.has set "apple") true
        toBe (MutableHashSet.has set "grape") false)

    test "remove deletes a value in place" (fun () ->
        let set = MutableHashSet.make [ "apple"; "banana"; "cherry" ]
        MutableHashSet.remove set "banana" |> ignore
        toBe (MutableHashSet.size set) 2
        toBe (MutableHashSet.has set "banana") false
        MutableHashSet.remove set "grape" |> ignore
        toBe (MutableHashSet.size set) 2)

    test "clear empties the set" (fun () ->
        let set = MutableHashSet.make [ "apple"; "banana"; "cherry" ]
        MutableHashSet.clear set |> ignore
        toBe (MutableHashSet.size set) 0
        toBe (MutableHashSet.has set "apple") false
        MutableHashSet.add set "new" |> ignore
        toBe (MutableHashSet.size set) 1))

