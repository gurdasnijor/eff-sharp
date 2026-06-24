namespace Effect

open System.Collections.Generic

/// Port of repos/effect-smol/packages/effect/src/MutableHashMap.ts — a mutable
/// key/value map that updates in place.
///
/// In Effect (TypeScript) this type juggles a native `Map` (reference/primitive
/// keys) plus hash buckets keyed by Effect's `Hash`/`Equal` (structural keys),
/// because JS `Map` only does reference equality. F# does not need that split:
/// a `Dictionary` built with `HashIdentity.Structural` already hashes and
/// compares keys by F#'s built-in structural equality, covering both reference
/// and structural keys uniformly. So the upstream `backing`/`buckets` pair, the
/// `referentialKeysCache`, `isSimpleKey`, and the explicit `Hash.hash` calls are
/// collapsed into a single backing `Dictionary` (decoupling rule: no dependency
/// on the `Equal`/`Hash` modules).
///
/// Omissions (intentional, JS-runtime-only): `isMutableHashMap` (a `hasProperty`
/// guard over `unknown` — F#'s static types make it dead weight), the `toJSON` /
/// `NodeInspectSymbol` inspection hooks, and the `pipe`/dual data-last currying
/// (F# pipes with `|>`; every function here takes `self` first).
type MutableHashMap<'K, 'V when 'K: equality> = { Backing: Dictionary<'K, 'V> }

[<RequireQualifiedAccess>]
module MutableHashMap =

    /// Upstream brand constant (kept as a literal for parity).
    [<Literal>]
    let TypeId = "~effect/collections/MutableHashMap"

    // --- constructors ---

    /// Creates an empty mutable hash map. (MutableHashMap.empty)
    let empty<'K, 'V when 'K: equality> () : MutableHashMap<'K, 'V> =
        { Backing = Dictionary<'K, 'V>(HashIdentity.Structural) }

    /// Inserts or replaces the value for `key`, mutating in place. Returns the
    /// same map. (MutableHashMap.set)
    let set (self: MutableHashMap<'K, 'V>) (key: 'K) (value: 'V) : MutableHashMap<'K, 'V> =
        self.Backing.[key] <- value
        self

    /// Creates a map from a sequence of key/value pairs. (MutableHashMap.fromIterable)
    let fromIterable (entries: seq<'K * 'V>) : MutableHashMap<'K, 'V> =
        let self = empty ()
        for (k, v) in entries do
            set self k v |> ignore
        self

    /// Creates a map from explicit key/value pairs. (MutableHashMap.make)
    let make (entries: ('K * 'V) list) : MutableHashMap<'K, 'V> = fromIterable entries

    // --- elements ---

    /// Looks up `key`, returning `Some value` when present. (MutableHashMap.get)
    let get (self: MutableHashMap<'K, 'V>) (key: 'K) : 'V option =
        match self.Backing.TryGetValue key with
        | true, v -> Some v
        | _ -> None

    /// Whether `key` is present. (MutableHashMap.has)
    let has (self: MutableHashMap<'K, 'V>) (key: 'K) : bool = self.Backing.ContainsKey key

    /// The keys, in insertion order. (MutableHashMap.keys)
    let keys (self: MutableHashMap<'K, 'V>) : seq<'K> = self.Backing.Keys :> seq<'K>

    /// The values, in insertion order. (MutableHashMap.values)
    let values (self: MutableHashMap<'K, 'V>) : seq<'V> = self.Backing.Values :> seq<'V>

    /// The number of entries. (MutableHashMap.size)
    let size (self: MutableHashMap<'K, 'V>) : int = self.Backing.Count

    /// Whether the map has no entries. (MutableHashMap.isEmpty)
    let isEmpty (self: MutableHashMap<'K, 'V>) : bool = self.Backing.Count = 0

    /// All entries as a sequence of pairs, in insertion order.
    let toSeq (self: MutableHashMap<'K, 'V>) : seq<'K * 'V> =
        self.Backing |> Seq.map (fun kv -> kv.Key, kv.Value)

    // --- mutations ---

    /// Removes `key` if present, mutating in place. Returns the same map.
    /// (MutableHashMap.remove)
    let remove (self: MutableHashMap<'K, 'V>) (key: 'K) : MutableHashMap<'K, 'V> =
        self.Backing.Remove key |> ignore
        self

    /// Applies `f` to the value at `key` if it exists; missing keys are left
    /// unchanged. Returns the same map. (MutableHashMap.modify)
    let modify (self: MutableHashMap<'K, 'V>) (key: 'K) (f: 'V -> 'V) : MutableHashMap<'K, 'V> =
        match self.Backing.TryGetValue key with
        | true, v -> self.Backing.[key] <- f v
        | _ -> ()
        self

    /// Updates or removes `key` from a function over its current optional value:
    /// `None` removes (if present), `Some v` sets. Returns the same map.
    /// (MutableHashMap.modifyAt)
    let modifyAt
        (self: MutableHashMap<'K, 'V>)
        (key: 'K)
        (f: 'V option -> 'V option)
        : MutableHashMap<'K, 'V> =
        let current = get self key
        match f current with
        | None -> if Option.isSome current then remove self key |> ignore
        | Some v -> set self key v |> ignore
        self

    /// Removes every entry, mutating in place. Returns the same map.
    /// (MutableHashMap.clear)
    let clear (self: MutableHashMap<'K, 'V>) : MutableHashMap<'K, 'V> =
        self.Backing.Clear()
        self

    // --- traversing ---

    /// Runs `f value key` for each entry, in insertion order. The value comes
    /// first to match `Map.prototype.forEach`. (MutableHashMap.forEach)
    let forEach (self: MutableHashMap<'K, 'V>) (f: 'V -> 'K -> unit) : unit =
        for kv in self.Backing do
            f kv.Value kv.Key

    // --- rendering (matches Effect's toString) ---

    let private renderValue (value: obj) : string =
        match value with
        | null -> "undefined"
        | :? string as s -> "\"" + s + "\""
        | other -> string other

    /// `MutableHashMap([[0,"a"],[1,"b"]])`
    let render (self: MutableHashMap<'K, 'V>) : string =
        self.Backing
        |> Seq.map (fun kv -> sprintf "[%s,%s]" (renderValue (box kv.Key)) (renderValue (box kv.Value)))
        |> String.concat ","
        |> sprintf "MutableHashMap([%s])"
