namespace Effect

open System.Collections.Generic

/// `HashRing` — a weighted consistent-hashing ring.
///
/// Port of repos/effect-smol/packages/effect/src/HashRing.ts. A ring assigns
/// string inputs to nodes so that adding/removing/reweighting a node only remaps
/// a small fraction of inputs. Like upstream this is a genuinely *stateful*
/// structure: `add`/`addMany`/`remove` mutate and return the same instance.
///
/// Decoupling notes:
///   * Upstream identifies nodes through the `PrimaryKey` module. That module is
///     out of scope here, so we depend instead on a tiny local `IPrimaryKey`
///     interface (a node exposes a stable `PrimaryKey` string). Any node type can
///     opt in by implementing it.
///   * Upstream hashes ring points and inputs with the `Hash` module. Per the
///     decoupling rule we avoid it and use a self-contained FNV-1a string hash;
///     the ring only requires a deterministic, well-distributed hash, which this
///     provides.
///
/// No upstream `*.test.ts` exists for this module; HashRingTests.fs adds F# smoke
/// tests covering construction, routing stability, membership and shard layout.
///
/// Omissions: `isHashRing` (`unknown`-narrowing guard, CONVENTIONS §3) and the
/// `Pipeable`/`Inspectable` plumbing (CONVENTIONS §4).

/// A node identity: a stable primary-key string. (replaces upstream PrimaryKey)
type IPrimaryKey =
    abstract member PrimaryKey: string

/// A mutable weighted consistent-hashing ring over nodes of type `'A`.
type HashRing<'A when 'A :> IPrimaryKey> =
    private
        { BaseWeight: int
          mutable TotalWeightCache: float
          Nodes: Dictionary<string, 'A * float>
          mutable Ring: (int * string) array }

[<RequireQualifiedAccess>]
module HashRing =


    /// FNV-1a 32-bit string hash, reinterpreted as a (possibly negative) int.
    /// Self-contained replacement for the upstream `Hash.string`.
    let private hashString (s: string) : int =
        let mutable h = 2166136261u
        for c in s do
            h <- (h ^^^ uint32 (uint16 c)) * 16777619u
        int h

    /// Create an empty ring. `baseWeight` is the virtual-point density for a
    /// weight-1 node (default 128, clamped to >= 1). (HashRing.make)
    let make (baseWeight: int option) : HashRing<'A> =
        { BaseWeight = max (defaultArg baseWeight 128) 1
          TotalWeightCache = 0.0
          Nodes = Dictionary<string, 'A * float>()
          Ring = [||] }

    /// Push `points` virtual entries per key onto the ring and re-sort by hash.
    let private addNodesToRing (self: HashRing<'A>) (keys: string list) (points: int) : unit =
        let additions = ResizeArray<int * string>()
        for i in points .. -1 .. 1 do
            for key in keys do
                additions.Add(hashString (sprintf "%s:%d" key i), key)
        let combined = Array.append self.Ring (additions.ToArray())
        Array.sortInPlaceBy fst combined
        self.Ring <- combined

    /// Add or update several nodes at a shared weight (clamped to >= 0.1).
    /// Mutates and returns the same ring. (HashRing.addMany)
    let addMany (self: HashRing<'A>) (nodes: seq<'A>) (weight: float option) : HashRing<'A> =
        let weight = max (defaultArg weight 1.0) 0.1
        let keys = ResizeArray<string>()
        let toRemove = HashSet<string>()
        for node in nodes do
            let key = (node :> IPrimaryKey).PrimaryKey
            match self.Nodes.TryGetValue key with
            | true, (existingNode, existingWeight) ->
                // Re-point only when the weight actually changes.
                if existingWeight <> weight then
                    toRemove.Add key |> ignore
                    self.TotalWeightCache <- self.TotalWeightCache - existingWeight + weight
                    self.Nodes.[key] <- (existingNode, weight)
                    keys.Add key
            | _ ->
                self.Nodes.[key] <- (node, weight)
                self.TotalWeightCache <- self.TotalWeightCache + weight
                keys.Add key
        if toRemove.Count > 0 then
            self.Ring <- self.Ring |> Array.filter (fun (_, n) -> not (toRemove.Contains n))
        addNodesToRing self (List.ofSeq keys) (int (System.Math.Round(weight * float self.BaseWeight)))
        self

    /// Add or update a single node. Mutates and returns the same ring. (HashRing.add)
    let add (self: HashRing<'A>) (node: 'A) (weight: float option) : HashRing<'A> =
        addMany self (Seq.singleton node) weight

    /// Remove a node by primary key; no-op when absent. (HashRing.remove)
    let remove (self: HashRing<'A>) (node: 'A) : HashRing<'A> =
        let key = (node :> IPrimaryKey).PrimaryKey
        match self.Nodes.TryGetValue key with
        | true, (_, w) ->
            self.Nodes.Remove key |> ignore
            self.Ring <- self.Ring |> Array.filter (fun (_, n) -> n <> key)
            self.TotalWeightCache <- self.TotalWeightCache - w
        | _ -> ()
        self

    /// Whether a node with the same primary key is registered. (HashRing.has)
    let has (self: HashRing<'A>) (node: 'A) : bool =
        self.Nodes.ContainsKey((node :> IPrimaryKey).PrimaryKey)

    /// Binary-search the ring for the entry nearest `hash`, optionally skipping
    /// excluded node keys. Returns `(ringIndex, distance)`.
    let private getIndexForInput (self: HashRing<'A>) (hash: int) (exclude: HashSet<string> option) : int * int64 =
        let ring = self.Ring
        let len = ring.Length
        let dist i = abs (int64 (fst ring.[i]) - int64 hash)
        let mutable lo = 0
        let mutable hi = len - 1
        while lo <= hi do
            let mid = (lo + hi) / 2
            if fst ring.[mid] >= hash then hi <- mid - 1 else lo <- mid + 1
        let a = if lo = len then lo - 1 else lo
        match exclude with
        | None ->
            let b = lo - 1
            if b < 0 then (a, dist a)
            else
                let distA = dist a
                let distB = dist b
                if distA <= distB then (a, distA) else (b, distB)
        | Some ex when not (ex.Contains(snd ring.[a])) -> (a, dist a)
        | Some ex ->
            let range = max lo (len - lo)
            let mutable result = (a, dist a)
            let mutable found = false
            let mutable i = 1
            while not found && i < range do
                let idx1 = lo - i
                if idx1 >= 0 && idx1 < len && not (ex.Contains(snd ring.[idx1])) then
                    result <- (idx1, dist idx1)
                    found <- true
                else
                    let idx2 = lo + i
                    if idx2 >= 0 && idx2 < len && not (ex.Contains(snd ring.[idx2])) then
                        result <- (idx2, dist idx2)
                        found <- true
                i <- i + 1
            result

    /// Route a single input string to the node responsible for it, or `None`
    /// when the ring is empty. (HashRing.get)
    let get (self: HashRing<'A>) (input: string) : 'A option =
        if self.Ring.Length = 0 then
            None
        else
            let index = fst (getIndexForInput self (hashString input) None)
            let node = snd self.Ring.[index]
            Some(fst self.Nodes.[node])

    let private shardHash (shard: int) : int = hashString (sprintf "shard-%d" shard)

    /// Compute a balanced ownership of `count` shards across the ring, or `None`
    /// when the ring is empty. (HashRing.getShards)
    let getShards (self: HashRing<'A>) (count: int) : 'A array option =
        if self.Ring.Length = 0 then
            None
        else
            let shards = Array.zeroCreate<'A> count
            let allocations = Dictionary<string, int>()
            let remaining = HashSet<int>()
            let exclude = HashSet<string>()
            let allocOf node =
                match allocations.TryGetValue node with
                | true, c -> c
                | _ -> 0
            let maxPerNode weight =
                max 1 (int (floor (float count * (weight / self.TotalWeightCache))))

            // First pass: closest node per shard, skipping nodes already at max.
            let distances = Array.zeroCreate<int * string * int64> count
            for shard in 0 .. count - 1 do
                let index, distance = getIndexForInput self (shardHash shard) None
                let node = snd self.Ring.[index]
                distances.[shard] <- (shard, node, distance)
                remaining.Add shard |> ignore
            Array.sortInPlaceBy (fun (_, _, d) -> d) distances
            for i in 0 .. count - 1 do
                let shard, node, _ = distances.[i]
                if not (exclude.Contains node) then
                    let value, weight = self.Nodes.[node]
                    shards.[shard] <- value
                    remaining.Remove shard |> ignore
                    let nodeCount = allocOf node + 1
                    allocations.[node] <- nodeCount
                    if nodeCount >= maxPerNode weight then
                        exclude.Add node |> ignore

            // Second pass: place leftover shards, still avoiding maxed nodes
            // unless every node is maxed.
            let mutable allAtMax = exclude.Count = self.Nodes.Count
            for shard in List.ofSeq remaining do
                let ex = if allAtMax then None else Some exclude
                let index = fst (getIndexForInput self (shardHash shard) ex)
                let node = snd self.Ring.[index]
                let value, weight = self.Nodes.[node]
                shards.[shard] <- value
                if not allAtMax then
                    let nodeCount = allocOf node + 1
                    allocations.[node] <- nodeCount
                    if nodeCount >= maxPerNode weight then
                        exclude.Add node |> ignore
                        if exclude.Count = self.Nodes.Count then
                            allAtMax <- true
            Some shards

    /// The registered nodes. (HashRing iteration)
    let nodes (self: HashRing<'A>) : seq<'A> =
        self.Nodes.Values |> Seq.map fst
