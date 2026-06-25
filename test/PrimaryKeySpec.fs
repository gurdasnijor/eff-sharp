module PrimaryKeySpec

open Effect
open Effect.Vitest

type private ProductId(category: string, id: int) =
    interface IPrimaryKey with
        member _.PrimaryKey = sprintf "%s-%d" category id

describe "PrimaryKey" (fun () ->
    test "value reads the stable identifier" (fun () ->
        let productId = ProductId("electronics", 42)
        toBe (PrimaryKey.value (productId :> IPrimaryKey)) "electronics-42")

    test "isPrimaryKey narrows IPrimaryKey values" (fun () ->
        toBe (PrimaryKey.isPrimaryKey (box (ProductId("electronics", 42)))) true
        toBe (PrimaryKey.isPrimaryKey (box 5)) false
        toBe (PrimaryKey.isPrimaryKey (box "not a key")) false))
