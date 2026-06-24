module Effect.Tests.BrandTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/Brand.test.ts
// The Schema-backed `check`/`all` cases are skipped (Schema is unported); the
// predicate-based `make` and `nominal` surface is covered here.

let private isInteger (n: float) : Result<unit, string> =
    if n = System.Math.Truncate n then Ok () else Error (sprintf "Expected %g to be an integer" n)

[<Fact>]
let ``creates nominal brands without runtime validation`` () =
    let myNumber = Brand.nominal<float> ()
    Assert.Equal(1.0, myNumber.Construct 1.0)
    Assert.Equal(1.1, myNumber.Construct 1.1)
    Assert.Equal(-1.0, myNumber.Construct -1.0)

    Assert.True(myNumber.Is 1.0)
    Assert.True(myNumber.Is 1.1)
    Assert.True(myNumber.Is -1.0)

    Assert.Equal(Some 1.0, myNumber.Option 1.0)
    Assert.Equal(Some 1.1, myNumber.Option 1.1)

    Assert.Equal(Ok 1.0, myNumber.Result 1.0)
    Assert.Equal(Ok 1.1, myNumber.Result 1.1)

[<Fact>]
let ``validates custom predicates created with make`` () =
    let int' = Brand.make isInteger

    Assert.Equal(1.0, int'.Construct 1.0)
    Assert.Throws<BrandException>(fun () -> int'.Construct 1.1 |> ignore) |> ignore
    Assert.Equal(-1.0, int'.Construct -1.0)

    Assert.True(int'.Is 1.0)
    Assert.False(int'.Is 1.1)
    Assert.True(int'.Is -1.0)

    Assert.Equal(Some 1.0, int'.Option 1.0)
    Assert.Equal(None, int'.Option 1.1)
    Assert.Equal(Some -1.0, int'.Option -1.0)

    Assert.Equal(Ok 1.0, int'.Result 1.0)
    Assert.Equal(Ok -1.0, int'.Result -1.0)

[<Fact>]
let ``make failures carry the predicate message`` () =
    let int' = Brand.make isInteger
    match int'.Result 1.1 with
    | Error e -> Assert.Equal("Expected 1.1 to be an integer", e.Message)
    | Ok _ -> Assert.Fail("expected failure")

[<Fact>]
let ``BrandError formats as BrandError(message)`` () =
    let int' = Brand.make isInteger
    match int'.Result 1.1 with
    | Error e -> Assert.Equal("BrandError(Expected 1.1 to be an integer)", string e)
    | Ok _ -> Assert.Fail("expected failure")

[<Fact>]
let ``Construct throws a BrandException carrying the structured BrandError`` () =
    let int' = Brand.make isInteger
    let ex = Assert.Throws<BrandException>(fun () -> int'.Construct 1.1 |> ignore)
    // The structured error is recoverable (not lost to a bare System.Exception).
    Assert.Equal("Expected 1.1 to be an integer", ex.Data0.Message)
    Assert.Equal("BrandError(Expected 1.1 to be an integer)", ex.Message)
