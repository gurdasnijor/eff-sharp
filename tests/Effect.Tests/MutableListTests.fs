module Effect.Tests.MutableListTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/MutableList.test.ts
//
// The upstream `Empty` sentinel returned by `take` is lowered to F#'s `option`,
// so `take` of an empty list is `None` (was `MutableList.Empty`).

[<Fact>]
let ``appendAll returns 0 and leaves an empty list empty`` () =
    let list = MutableList.make<int> ()
    Assert.Equal(0, MutableList.appendAll list [])
    Assert.Equal(0, list.Length)
    Assert.Equal(None, list.Head)
    Assert.Equal(None, list.Tail)
    Assert.Equal(None, MutableList.take list)

[<Fact>]
let ``appendAll with empty iterables preserves later append order`` () =
    let list = MutableList.make<int> ()
    MutableList.appendAll list [] |> ignore
    MutableList.append list 1
    MutableList.appendAll list [] |> ignore
    MutableList.append list 2
    Assert.Equal<int[]>([| 1; 2 |], MutableList.takeAll list)
    Assert.Equal(None, MutableList.take list)

[<Fact>]
let ``appendAllUnsafe with an empty array is a no-op`` () =
    let list = MutableList.make<int> ()
    MutableList.appendAll list [ 1 ] |> ignore
    Assert.Equal(0, MutableList.appendAllUnsafe list [||])
    MutableList.append list 2
    Assert.Equal<int[]>([| 1; 2 |], MutableList.takeAll list)
    Assert.Equal(None, MutableList.take list)

[<Fact>]
let ``filter keeps matching values in place and updates length`` () =
    let list = MutableList.make<int> ()
    MutableList.appendAll list [ 1; 2; 3; 4; 5 ] |> ignore
    MutableList.filter list (fun n _ -> n % 2 = 0)
    Assert.Equal<int[]>([| 2; 4 |], MutableList.toArrayN list 2)
    Assert.Equal(2, list.Length)

[<Fact>]
let ``remove deletes all strictly equal values and updates length`` () =
    let list = MutableList.make<string> ()
    MutableList.appendAll list [ "apple"; "banana"; "apple"; "cherry"; "apple" ] |> ignore
    MutableList.remove list "apple"
    Assert.Equal<string[]>([| "banana"; "cherry" |], MutableList.toArrayN list 2)
    Assert.Equal(2, list.Length)

// --- additional behaviour from MutableList.ts docs (no upstream test) ---

[<Fact>]
let ``append, prepend and take in FIFO order`` () =
    let list = MutableList.make<int> ()
    MutableList.append list 1
    MutableList.append list 2
    MutableList.prepend list 0
    Assert.Equal(3, list.Length)
    Assert.Equal(Some 0, MutableList.take list)
    Assert.Equal(Some 1, MutableList.take list)
    Assert.Equal(Some 2, MutableList.take list)
    Assert.Equal(None, MutableList.take list)

[<Fact>]
let ``takeN takes a bounded prefix and removes it`` () =
    let list = MutableList.make<int> ()
    MutableList.appendAll list [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] |> ignore
    Assert.Equal<int[]>([| 1; 2; 3 |], MutableList.takeN list 3)
    Assert.Equal(7, list.Length)
    Assert.Equal<int[]>([| 4; 5; 6; 7; 8; 9; 10 |], MutableList.takeN list 20)
    Assert.Equal(0, list.Length)
    Assert.Equal<int[]>([||], MutableList.takeN list 5)

[<Fact>]
let ``prependAll places elements at the front in order`` () =
    let list = MutableList.make<int> ()
    MutableList.append list 4
    MutableList.append list 5
    MutableList.prependAll list [ 1; 2; 3 ]
    Assert.Equal(5, list.Length)
    Assert.Equal<int[]>([| 1; 2; 3; 4; 5 |], MutableList.takeAll list)

[<Fact>]
let ``toArray snapshots without consuming`` () =
    let list = MutableList.make<int> ()
    MutableList.appendAll list [ 1; 2; 3 ] |> ignore
    Assert.Equal<int[]>([| 1; 2; 3 |], MutableList.toArray list)
    Assert.Equal(3, list.Length)

[<Fact>]
let ``clear resets the list`` () =
    let list = MutableList.make<int> ()
    MutableList.appendAll list [ 1; 2; 3; 4; 5 ] |> ignore
    MutableList.clear list
    Assert.Equal(0, list.Length)
    Assert.Equal(None, MutableList.take list)
    MutableList.append list 42
    Assert.Equal(1, list.Length)
