module Effect.Tests.HashRingTests

open Xunit
open Effect

// No upstream HashRing.test.ts exists (see HashRing.fs doc comment); these are
// F# smoke tests for construction, routing stability, membership, the
// consistent-hashing remap guarantee, and shard layout.

type private Node =
    { Name: string }

    interface IPrimaryKey with
        member this.PrimaryKey = this.Name

let private node name = { Name = name }

// --- construction / empty ---

[<Fact>]
let ``empty ring routes nothing`` () =
    let ring = HashRing.make None
    Assert.Equal(None, HashRing.get ring "anything")
    Assert.Equal(None, HashRing.getShards ring 8)

// --- add / has / get ---

[<Fact>]
let ``added nodes are members and route deterministically`` () =
    let ring = HashRing.make None
    HashRing.addMany ring [ node "a"; node "b"; node "c" ] None |> ignore

    Assert.True(HashRing.has ring (node "a"))
    Assert.True(HashRing.has ring (node "b"))
    Assert.False(HashRing.has ring (node "z"))

    // routing is deterministic and lands on a registered node
    let first = HashRing.get ring "user-42"
    let second = HashRing.get ring "user-42"
    Assert.Equal(first, second)
    Assert.True(Option.isSome first)
    let names = HashRing.nodes ring |> Seq.map (fun n -> n.Name) |> Set.ofSeq
    Assert.Contains((Option.get first).Name, names)

// --- remove ---

[<Fact>]
let ``remove drops a node and keeps routing on the rest`` () =
    let ring = HashRing.make None
    HashRing.addMany ring [ node "a"; node "b"; node "c" ] None |> ignore
    HashRing.remove ring (node "b") |> ignore

    Assert.False(HashRing.has ring (node "b"))
    Assert.Equal(2, HashRing.nodes ring |> Seq.length)

    // every input still routes, and never to the removed node
    for i in 0..50 do
        let routed = HashRing.get ring (sprintf "key-%d" i)
        Assert.True(Option.isSome routed)
        Assert.NotEqual<string>("b", (Option.get routed).Name)

[<Fact>]
let ``remove of an absent node is a no-op`` () =
    let ring = HashRing.make None
    HashRing.add ring (node "a") None |> ignore
    HashRing.remove ring (node "missing") |> ignore
    Assert.Equal(1, HashRing.nodes ring |> Seq.length)

// --- consistent hashing remap guarantee ---

[<Fact>]
let ``adding a node remaps only a minority of keys`` () =
    let ring = HashRing.make None
    HashRing.addMany ring [ node "a"; node "b"; node "c" ] None |> ignore

    let keys = [ for i in 0..199 -> sprintf "key-%d" i ]
    let before = keys |> List.map (fun k -> k, (HashRing.get ring k |> Option.get).Name)

    HashRing.add ring (node "d") None |> ignore
    let after = keys |> List.map (fun k -> k, (HashRing.get ring k |> Option.get).Name)

    let unchanged =
        List.zip before after
        |> List.filter (fun ((_, b), (_, a)) -> b = a)
        |> List.length

    // Adding a 4th node should move well under half the keys.
    Assert.True(unchanged > 100, sprintf "expected majority stable, got %d/200" unchanged)

// --- shards ---

[<Fact>]
let ``getShards assigns every shard to a registered node`` () =
    let ring = HashRing.make None
    HashRing.addMany ring [ node "a"; node "b"; node "c" ] None |> ignore

    let shards = HashRing.getShards ring 12 |> Option.get
    Assert.Equal(12, shards.Length)
    let names = HashRing.nodes ring |> Seq.map (fun n -> n.Name) |> Set.ofSeq

    for s in shards do
        Assert.Contains(s.Name, names)

[<Fact>]
let ``a heavier node owns more shards`` () =
    let ring = HashRing.make None
    HashRing.add ring (node "light") None |> ignore
    HashRing.add ring (node "heavy") (Some 5.0) |> ignore

    let shards = HashRing.getShards ring 60 |> Option.get
    let heavyCount = shards |> Array.filter (fun s -> s.Name = "heavy") |> Array.length
    let lightCount = shards |> Array.filter (fun s -> s.Name = "light") |> Array.length
    Assert.Equal(60, heavyCount + lightCount)
    Assert.True(heavyCount > lightCount, sprintf "heavy=%d light=%d" heavyCount lightCount)

// --- reweighting ---

[<Fact>]
let ``re-adding a node with a new weight updates it in place`` () =
    let ring = HashRing.make None
    HashRing.add ring (node "a") None |> ignore
    HashRing.add ring (node "a") (Some 3.0) |> ignore
    Assert.Equal(1, HashRing.nodes ring |> Seq.length)
    Assert.True(HashRing.has ring (node "a"))
