module HashRingSpec

open Effect
open Effect.Vitest

type private Node =
    { Name: string }

    interface IPrimaryKey with
        member this.PrimaryKey = this.Name

let private node name = { Name = name }

let private same actual expected =
    toBe (actual = expected) true

describe "HashRing" (fun () ->
    test "empty ring routes nothing" (fun () ->
        let ring = HashRing.make None
        same (HashRing.get ring "anything") None
        same (HashRing.getShards ring 8) None)

    test "added nodes are members and route deterministically" (fun () ->
        let ring = HashRing.make None
        HashRing.addMany ring [ node "a"; node "b"; node "c" ] None |> ignore

        toBe (HashRing.has ring (node "a")) true
        toBe (HashRing.has ring (node "b")) true
        toBe (HashRing.has ring (node "z")) false

        let first = HashRing.get ring "user-42"
        let second = HashRing.get ring "user-42"
        same first second
        toBe (Option.isSome first) true

        let names = HashRing.nodes ring |> Seq.map (fun n -> n.Name) |> Set.ofSeq
        toBe (Set.contains (Option.get first).Name names) true)

    test "remove drops a node and keeps routing on the rest" (fun () ->
        let ring = HashRing.make None
        HashRing.addMany ring [ node "a"; node "b"; node "c" ] None |> ignore
        HashRing.remove ring (node "b") |> ignore

        toBe (HashRing.has ring (node "b")) false
        toBe (HashRing.nodes ring |> Seq.length) 2

        for i in 0..50 do
            let routed = HashRing.get ring (sprintf "key-%d" i)
            toBe (Option.isSome routed) true
            toBe ((Option.get routed).Name = "b") false)

    test "remove of an absent node is a no-op" (fun () ->
        let ring = HashRing.make None
        HashRing.add ring (node "a") None |> ignore
        HashRing.remove ring (node "missing") |> ignore
        toBe (HashRing.nodes ring |> Seq.length) 1)

    test "adding a node remaps only a minority of keys" (fun () ->
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

        toBeGreaterThan (float unchanged) 100.0)

    test "getShards assigns every shard to a registered node" (fun () ->
        let ring = HashRing.make None
        HashRing.addMany ring [ node "a"; node "b"; node "c" ] None |> ignore

        let shards = HashRing.getShards ring 12 |> Option.get
        toBe shards.Length 12
        let names = HashRing.nodes ring |> Seq.map (fun n -> n.Name) |> Set.ofSeq

        for s in shards do
            toBe (Set.contains s.Name names) true)

    test "weights and re-adding update shard ownership" (fun () ->
        let ring = HashRing.make None
        HashRing.add ring (node "light") None |> ignore
        HashRing.add ring (node "heavy") (Some 5.0) |> ignore

        let shards = HashRing.getShards ring 60 |> Option.get
        let heavyCount = shards |> Array.filter (fun s -> s.Name = "heavy") |> Array.length
        let lightCount = shards |> Array.filter (fun s -> s.Name = "light") |> Array.length
        toBe (heavyCount + lightCount) 60
        toBeGreaterThan (float heavyCount) (float lightCount)

        let ring2 = HashRing.make None
        HashRing.add ring2 (node "a") None |> ignore
        HashRing.add ring2 (node "a") (Some 3.0) |> ignore
        toBe (HashRing.nodes ring2 |> Seq.length) 1
        toBe (HashRing.has ring2 (node "a")) true))
