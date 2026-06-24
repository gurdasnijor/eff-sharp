module Effect.Tests.MutableHashSetTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/MutableHashSet.test.ts
//
// The upstream file only tests `isMutableHashSet` (distinguishing from a native
// `Set`), a JS-runtime guard with no F# meaning — intentionally not ported.
// These cases instead cover the documented behaviour from MutableHashSet.ts.

[<Fact>]
let ``make and fromIterable de-duplicate`` () =
    let set = MutableHashSet.make [ "apple"; "banana"; "apple"; "cherry" ]
    Assert.Equal(3, MutableHashSet.size set)
    Assert.Equal<string list>([ "apple"; "banana"; "cherry" ], List.ofSeq (MutableHashSet.toSeq set))

[<Fact>]
let ``add ignores duplicates`` () =
    let set = MutableHashSet.empty ()
    MutableHashSet.add set "apple" |> ignore
    MutableHashSet.add set "banana" |> ignore
    MutableHashSet.add set "apple" |> ignore
    Assert.Equal(2, MutableHashSet.size set)
    Assert.True(MutableHashSet.has set "apple")
    Assert.False(MutableHashSet.has set "grape")

[<Fact>]
let ``remove deletes a value in place`` () =
    let set = MutableHashSet.make [ "apple"; "banana"; "cherry" ]
    MutableHashSet.remove set "banana" |> ignore
    Assert.Equal(2, MutableHashSet.size set)
    Assert.False(MutableHashSet.has set "banana")
    // Removing a missing value is a no-op.
    MutableHashSet.remove set "grape" |> ignore
    Assert.Equal(2, MutableHashSet.size set)

[<Fact>]
let ``clear empties the set`` () =
    let set = MutableHashSet.make [ "apple"; "banana"; "cherry" ]
    MutableHashSet.clear set |> ignore
    Assert.Equal(0, MutableHashSet.size set)
    Assert.False(MutableHashSet.has set "apple")
    // Still usable after clearing.
    MutableHashSet.add set "new" |> ignore
    Assert.Equal(1, MutableHashSet.size set)
