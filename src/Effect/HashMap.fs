namespace Effect

/// `HashMap` — an immutable key/value map.
///
/// Port of repos/effect-smol/packages/effect/src/HashMap.ts. Upstream backs this
/// with a Hash Array Mapped Trie (HAMT) and resolves keys through the `Equal` /
/// `Hash` modules. Per the porting conventions we instead reuse FSharp.Core's
/// persistent `Map<'K,'V>`, which already provides:
///   * structural, order-independent equality of the whole map (so two maps with
///     the same entries compare equal regardless of insertion order), and
///   * key lookup via F#'s built-in structural `comparison` —
/// satisfying the decoupling rule (no dependency on the `Equal`/`Hash` modules).
///
/// Omissions vs. upstream (all deliberate; F# makes them dead weight):
///   * `isHashMap` and other `unknown`-narrowing runtime guards (CONVENTIONS §3) —
///     a value is statically a `HashMap` or it is not.
///   * `getHash` / `hasHash` / `modifyHash` — precomputed-hash variants only make
///     sense with an exposed HAMT + `Hash` module; the hash is an internal detail
///     here, so the plain `get`/`has`/`modifyAt` cover the behaviour.
///   * `beginMutation` / `endMutation` / `mutate` — the transient-mutation window
///     is a HAMT performance affordance; FSharp.Core's `Map` is already a cheap
///     persistent structure, so batched updates use `setMany`/`removeMany`.
///   * HKT / `Pipeable` / `Inspectable` plumbing (CONVENTIONS §4).
type HashMap<'K, 'V when 'K: comparison> = private { entries: Map<'K, 'V> }

[<RequireQualifiedAccess>]
module HashMap =


    // --- constructors ---

    /// The empty map. (HashMap.empty)
    let empty<'K, 'V when 'K: comparison> : HashMap<'K, 'V> = { entries = Map.empty }

    /// A map from an iterable of key/value pairs; later pairs win. (HashMap.fromIterable)
    let fromIterable (entries: seq<'K * 'V>) : HashMap<'K, 'V> = { entries = Map.ofSeq entries }

    /// A map from a list of key/value pairs. (HashMap.make)
    let make (entries: ('K * 'V) list) : HashMap<'K, 'V> = fromIterable entries

    // --- elements / getters ---

    /// Whether the map has no entries. (HashMap.isEmpty)
    let isEmpty (self: HashMap<'K, 'V>) : bool = Map.isEmpty self.entries

    /// The number of entries. (HashMap.size)
    let size (self: HashMap<'K, 'V>) : int = self.entries.Count

    /// Safe lookup. (HashMap.get)
    let get (self: HashMap<'K, 'V>) (key: 'K) : 'V option = Map.tryFind key self.entries

    /// Whether the key has an entry. (HashMap.has)
    let has (self: HashMap<'K, 'V>) (key: 'K) : bool = Map.containsKey key self.entries

    /// Unsafe lookup; throws when the key is absent. (HashMap.getUnsafe)
    let getUnsafe (self: HashMap<'K, 'V>) (key: 'K) : 'V =
        match Map.tryFind key self.entries with
        | Some v -> v
        | None -> failwith "HashMap.getUnsafe: key not found"

    /// Whether any entry matches the predicate `(value, key)`. (HashMap.hasBy)
    let hasBy (self: HashMap<'K, 'V>) (predicate: 'V -> 'K -> bool) : bool =
        Map.exists (fun k v -> predicate v k) self.entries

    // --- transforming ---

    /// Set a key to a value, returning a new map. (HashMap.set)
    let set (self: HashMap<'K, 'V>) (key: 'K) (value: 'V) : HashMap<'K, 'V> =
        { entries = Map.add key value self.entries }

    /// Remove a key. Returns the same map (by reference) when the key is absent. (HashMap.remove)
    let remove (self: HashMap<'K, 'V>) (key: 'K) : HashMap<'K, 'V> =
        match Map.containsKey key self.entries with
        | true -> { entries = Map.remove key self.entries }
        | false -> self

    /// Remove every listed key. (HashMap.removeMany)
    let removeMany (self: HashMap<'K, 'V>) (keys: seq<'K>) : HashMap<'K, 'V> =
        { entries = keys |> Seq.fold (fun acc k -> Map.remove k acc) self.entries }

    /// Set every listed pair; later pairs win. (HashMap.setMany)
    let setMany (self: HashMap<'K, 'V>) (entries: seq<'K * 'V>) : HashMap<'K, 'V> =
        { entries = entries |> Seq.fold (fun acc (k, v) -> Map.add k v acc) self.entries }

    /// Combine two maps; entries from `that` win on key collisions. (HashMap.union)
    let union (self: HashMap<'K, 'V>) (that: HashMap<'K, 'V>) : HashMap<'K, 'V> =
        { entries = Map.fold (fun acc k v -> Map.add k v acc) self.entries that.entries }

    /// Insert/replace/remove a key from `f` applied to its current value. (HashMap.modifyAt)
    let modifyAt (self: HashMap<'K, 'V>) (key: 'K) (f: 'V option -> 'V option) : HashMap<'K, 'V> =
        { entries = Map.change key f self.entries }

    /// Update an existing key in place; a no-op when absent. (HashMap.modify)
    let modify (self: HashMap<'K, 'V>) (key: 'K) (f: 'V -> 'V) : HashMap<'K, 'V> =
        match Map.tryFind key self.entries with
        | Some v -> { entries = Map.add key (f v) self.entries }
        | None -> self

    // --- mapping / sequencing ---

    /// Map values, with the key available as `(value, key)`. (HashMap.map)
    let map (self: HashMap<'K, 'V>) (f: 'V -> 'K -> 'A) : HashMap<'K, 'A> =
        { entries = Map.map (fun k v -> f v k) self.entries }

    /// Map each entry to a map and flatten; later entries win. (HashMap.flatMap)
    let flatMap (self: HashMap<'K, 'A>) (f: 'A -> 'K -> HashMap<'K, 'B>) : HashMap<'K, 'B> =
        let folded =
            Map.fold
                (fun acc k v -> Map.fold (fun a k2 v2 -> Map.add k2 v2 a) acc (f v k).entries)
                Map.empty
                self.entries
        { entries = folded }

    // --- traversing / folding ---

    /// Apply `f (value, key)` for its side effects. (HashMap.forEach)
    let forEach (self: HashMap<'K, 'V>) (f: 'V -> 'K -> unit) : unit =
        Map.iter (fun k v -> f v k) self.entries

    /// Fold over entries as `(acc, value, key)`. (HashMap.reduce)
    let reduce (self: HashMap<'K, 'V>) (zero: 'Z) (f: 'Z -> 'V -> 'K -> 'Z) : 'Z =
        Map.fold (fun acc k v -> f acc v k) zero self.entries

    // --- filtering ---

    /// Keep entries matching the predicate `(value, key)`. (HashMap.filter)
    let filter (self: HashMap<'K, 'A>) (predicate: 'A -> 'K -> bool) : HashMap<'K, 'A> =
        { entries = Map.filter (fun k v -> predicate v k) self.entries }

    /// Drop `None` values from a map of options. (HashMap.compact)
    let compact (self: HashMap<'K, 'A option>) : HashMap<'K, 'A> =
        let folded =
            Map.fold
                (fun acc k v ->
                    match v with
                    | Some a -> Map.add k a acc
                    | None -> acc)
                Map.empty
                self.entries
        { entries = folded }

    /// Map and keep only `Ok` results. (HashMap.filterMap)
    let filterMap (self: HashMap<'K, 'A>) (f: 'A -> 'K -> Result<'B, 'X>) : HashMap<'K, 'B> =
        let folded =
            Map.fold
                (fun acc k v ->
                    match f v k with
                    | Ok b -> Map.add k b acc
                    | Error _ -> acc)
                Map.empty
                self.entries
        { entries = folded }

    // --- searching ---

    /// First entry `(key, value)` matching the predicate `(value, key)`. (HashMap.findFirst)
    let findFirst (self: HashMap<'K, 'A>) (predicate: 'A -> 'K -> bool) : ('K * 'A) option =
        Map.tryPick (fun k v -> if predicate v k then Some(k, v) else None) self.entries

    /// Whether any entry matches the predicate `(value, key)`. (HashMap.some)
    let some (self: HashMap<'K, 'A>) (predicate: 'A -> 'K -> bool) : bool =
        Map.exists (fun k v -> predicate v k) self.entries

    /// Whether every entry matches the predicate `(value, key)`. (HashMap.every)
    let every (self: HashMap<'K, 'A>) (predicate: 'A -> 'K -> bool) : bool =
        Map.forall (fun k v -> predicate v k) self.entries

    // --- iteration / conversion ---

    /// The keys, ascending by key. (HashMap.keys)
    let keys (self: HashMap<'K, 'V>) : seq<'K> = Map.keys self.entries

    /// The values, ordered by key. (HashMap.values)
    let values (self: HashMap<'K, 'V>) : seq<'V> = Map.values self.entries

    /// The values as a list. (HashMap.toValues)
    let toValues (self: HashMap<'K, 'V>) : 'V list = Map.values self.entries |> List.ofSeq

    /// The entries, ordered by key. (HashMap.entries)
    let entries (self: HashMap<'K, 'V>) : seq<'K * 'V> = self.entries |> Map.toSeq

    /// The entries as a list. (HashMap.toEntries)
    let toEntries (self: HashMap<'K, 'V>) : ('K * 'V) list = self.entries |> Map.toList
