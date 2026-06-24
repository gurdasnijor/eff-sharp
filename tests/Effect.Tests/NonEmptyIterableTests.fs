module Effect.Tests.NonEmptyIterableTests

open Xunit
open Effect

// No upstream test file exists (repos/effect-smol/.../test/NonEmptyIterable.test.ts
// is absent); these cases port the doc-comment examples from NonEmptyIterable.ts.

[<Fact>]
let ``unprepend splits head and rest`` () =
    let first, rest = NonEmptyIterable.unprepend [ 1; 2; 3; 4; 5 ]
    Assert.Equal(1, first)
    Assert.Equal<int list>([ 2; 3; 4; 5 ], List.ofSeq rest)

[<Fact>]
let ``unprepend on a single element yields empty rest`` () =
    let first, rest = NonEmptyIterable.unprepend [ "h" ]
    Assert.Equal("h", first)
    Assert.Empty(rest)

[<Fact>]
let ``unprepend works over any seq`` () =
    let first, rest = NonEmptyIterable.unprepend (seq { 10; 20; 30 })
    Assert.Equal(10, first)
    Assert.Equal<int list>([ 20; 30 ], List.ofSeq rest)

[<Fact>]
let ``unprepend throws on empty`` () =
    Assert.Throws<System.Exception>(fun () ->
        NonEmptyIterable.unprepend (Seq.empty<int>) |> ignore)
    |> ignore
