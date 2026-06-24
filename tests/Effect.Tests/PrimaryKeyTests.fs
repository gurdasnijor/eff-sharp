module Effect.Tests.PrimaryKeyTests

open Xunit
open Effect

// No upstream PrimaryKey.test.ts exists; these cover the protocol surface from
// the PrimaryKey.ts doc examples (ProductId / OrderId).

type private ProductId(category: string, id: int) =
    interface IPrimaryKey with
        member _.PrimaryKey = sprintf "%s-%d" category id

[<Fact>]
let ``value reads the stable identifier`` () =
    let productId = ProductId("electronics", 42)
    Assert.Equal("electronics-42", PrimaryKey.value (productId :> IPrimaryKey))

[<Fact>]
let ``isPrimaryKey narrows IPrimaryKey values`` () =
    Assert.True(PrimaryKey.isPrimaryKey (box (ProductId("electronics", 42))))
    Assert.False(PrimaryKey.isPrimaryKey (box 5))
    Assert.False(PrimaryKey.isPrimaryKey (box "not a key"))
