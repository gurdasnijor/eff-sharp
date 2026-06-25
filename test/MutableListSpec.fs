module MutableListSpec

open Effect
open Effect.Vitest

describe "MutableList" (fun () ->
    test "appendAll returns 0 and leaves an empty list empty" (fun () ->
        let list = MutableList.make<int> ()
        toBe (MutableList.appendAll list []) 0
        toBe list.Length 0
        toEqual list.Head None
        toEqual list.Tail None
        toEqual (MutableList.take list) None)

    test "appendAll with empty iterables preserves later append order" (fun () ->
        let list = MutableList.make<int> ()
        MutableList.appendAll list [] |> ignore
        MutableList.append list 1
        MutableList.appendAll list [] |> ignore
        MutableList.append list 2
        toEqual (MutableList.takeAll list |> Array.toList) [ 1; 2 ]
        toEqual (MutableList.take list) None)

    test "appendAllUnsafe with an empty array is a no-op" (fun () ->
        let list = MutableList.make<int> ()
        MutableList.appendAll list [ 1 ] |> ignore
        toBe (MutableList.appendAllUnsafe list [||]) 0
        MutableList.append list 2
        toEqual (MutableList.takeAll list |> Array.toList) [ 1; 2 ]
        toEqual (MutableList.take list) None)

    test "filter keeps matching values in place and updates length" (fun () ->
        let list = MutableList.make<int> ()
        MutableList.appendAll list [ 1; 2; 3; 4; 5 ] |> ignore
        MutableList.filter list (fun n _ -> n % 2 = 0)
        toEqual (MutableList.toArrayN list 2 |> Array.toList) [ 2; 4 ]
        toBe list.Length 2)

    test "remove deletes all strictly equal values and updates length" (fun () ->
        let list = MutableList.make<string> ()
        MutableList.appendAll list [ "apple"; "banana"; "apple"; "cherry"; "apple" ] |> ignore
        MutableList.remove list "apple"
        toEqual (MutableList.toArrayN list 2 |> Array.toList) [ "banana"; "cherry" ]
        toBe list.Length 2)

    test "append, prepend and take in FIFO order" (fun () ->
        let list = MutableList.make<int> ()
        MutableList.append list 1
        MutableList.append list 2
        MutableList.prepend list 0
        toBe list.Length 3
        toEqual (MutableList.take list) (Some 0)
        toEqual (MutableList.take list) (Some 1)
        toEqual (MutableList.take list) (Some 2)
        toEqual (MutableList.take list) None)

    test "takeN takes a bounded prefix and removes it" (fun () ->
        let list = MutableList.make<int> ()
        MutableList.appendAll list [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] |> ignore
        toEqual (MutableList.takeN list 3 |> Array.toList) [ 1; 2; 3 ]
        toBe list.Length 7
        toEqual (MutableList.takeN list 20 |> Array.toList) [ 4; 5; 6; 7; 8; 9; 10 ]
        toBe list.Length 0
        toEqual (MutableList.takeN list 5 |> Array.toList) [])

    test "prependAll places elements at the front in order" (fun () ->
        let list = MutableList.make<int> ()
        MutableList.append list 4
        MutableList.append list 5
        MutableList.prependAll list [ 1; 2; 3 ]
        toBe list.Length 5
        toEqual (MutableList.takeAll list |> Array.toList) [ 1; 2; 3; 4; 5 ])

    test "toArray snapshots without consuming" (fun () ->
        let list = MutableList.make<int> ()
        MutableList.appendAll list [ 1; 2; 3 ] |> ignore
        toEqual (MutableList.toArray list |> Array.toList) [ 1; 2; 3 ]
        toBe list.Length 3)

    test "clear resets the list" (fun () ->
        let list = MutableList.make<int> ()
        MutableList.appendAll list [ 1; 2; 3; 4; 5 ] |> ignore
        MutableList.clear list
        toBe list.Length 0
        toEqual (MutableList.take list) None
        MutableList.append list 42
        toBe list.Length 1))
