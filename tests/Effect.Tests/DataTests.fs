module Effect.Tests.DataTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/Data.test.ts.
// The upstream tests exercise JS class machinery (Data.Class, TaggedClass,
// taggedEnum, Error, TaggedError). In F# the substance — structural equality,
// tagging, exhaustive matching — is provided by records and discriminated
// unions, so each `describe` block is ported onto the idiomatic native type and
// the corresponding Data helper (`equals`, `tagOf`, `isTag`).

// Data.Class<A> -> an F# record (structural equality built in)
type Person = { Name: string; Age: int }

[<Fact>]
let ``Class: assigns fields`` () =
    let p = { Name = "Alice"; Age = 30 }
    Assert.Equal("Alice", p.Name)
    Assert.Equal(30, p.Age)

[<Fact>]
let ``Class: structural equality via Data.equals`` () =
    let a = { Name = "Bob"; Age = 25 }
    let b = { Name = "Bob"; Age = 25 }
    Assert.True(Data.equals a b)

[<Fact>]
let ``Class: not equal when fields differ`` () =
    let a = { Name = "Bob"; Age = 25 }
    let b = { Name = "Bob"; Age = 26 }
    Assert.False(Data.equals a b)

[<Fact>]
let ``Class: supports pipe`` () =
    let p = { Name = "C"; Age = 1 }
    Assert.Equal("C", p |> (fun x -> x.Name))

// Data.TaggedClass / Data.TaggedEnum -> a discriminated union
type Shape =
    | Circle of radius: float
    | Rect of width: float * height: float

[<Fact>]
let ``taggedEnum: constructors carry their tag and fields`` () =
    let c = Circle 5.0
    Assert.Equal("Circle", Data.tagOf c)
    match c with
    | Circle r -> Assert.Equal(5.0, r)
    | _ -> failwith "expected Circle"

    let r = Rect(3.0, 4.0)
    Assert.Equal("Rect", Data.tagOf r)
    match r with
    | Rect(w, h) ->
        Assert.Equal(3.0, w)
        Assert.Equal(4.0, h)
    | _ -> failwith "expected Rect"

[<Fact>]
let ``taggedEnum: isTag mirrors $is`` () =
    Assert.True(Data.isTag "Circle" (Circle 1.0))
    Assert.False(Data.isTag "Circle" (Rect(1.0, 1.0)))

[<Fact>]
let ``taggedEnum: $match lowers to a native match`` () =
    // $match -> native `match` (CONVENTIONS §2)
    let area =
        function
        | Circle radius -> System.Math.PI * radius ** 2.0
        | Rect(width, height) -> width * height
    Assert.Equal(System.Math.PI * 25.0, area (Circle 5.0))
    Assert.Equal(12.0, area (Rect(3.0, 4.0)))

[<Fact>]
let ``taggedEnum: different tags are not equal`` () =
    // structural equality includes the tag
    Assert.False(Data.equals (Circle 1.0) (Rect(1.0, 1.0)))

// Data.Error / Data.TaggedError -> CLR exception base classes
type MyError(code: int, message: string) =
    inherit Data.Error(message)
    member _.Code = code

type NotFound(resource: string) =
    inherit Data.TaggedError("NotFound")
    member _.Resource = resource

[<Fact>]
let ``Error: is a System.Exception carrying fields`` () =
    let e = MyError(404, "not found")
    Assert.IsAssignableFrom<System.Exception>(e) |> ignore
    Assert.Equal(404, e.Code)
    Assert.Equal("not found", e.Message)

[<Fact>]
let ``TaggedError: sets tag and is a System.Exception`` () =
    let e = NotFound("/users")
    Assert.Equal("NotFound", e.Tag)
    Assert.Equal("/users", e.Resource)
    Assert.IsAssignableFrom<System.Exception>(e) |> ignore

[<Fact>]
let ``TaggedError: distinct tags are distinguishable`` () =
    let a = NotFound("x") :> Data.TaggedError
    let b = Data.TaggedError("Forbidden")
    Assert.NotEqual<string>(a.Tag, b.Tag)
