namespace Effect

/// Port of repos/effect-smol/packages/effect/src/Trie.ts.
///
/// A `Trie<'V>` is an immutable map from `string` keys to values whose defining
/// behaviour is that iteration (and every derived operation) visits entries in
/// sorted key order, plus prefix-oriented queries (`keysWithPrefix`,
/// `longestPrefixOf`, ...).
///
/// Upstream implements this as a ternary search trie (`internal/trie.ts`) for
/// memory efficiency and fast prefix traversal. That data structure is an
/// implementation detail; its observable contract — sorted-by-key iteration,
/// structural equality by entries, and prefix search — is provided directly by
/// FSharp.Core's `Map<string, 'V>`, which is an ordered immutable map with
/// structural equality. So here a `Trie` simply wraps a `Map`, and prefix
/// queries are computed over the already-sorted keys.
///
/// Omissions (per CONVENTIONS.md): the trie node internals and their tags, the
/// JS `TypeId`/`isTrie` runtime guard, `toJSON`/`inspect`, and the `.pipe()`
/// method. Effect's `Equal`/`Hash` are satisfied by `Map`'s structural
/// equality (see the porting brief's decoupling rule).
type Trie<'V> = { Entries: Map<string, 'V> }

[<RequireQualifiedAccess>]
module Trie =

    // --- constructors ---

    let empty<'V> : Trie<'V> = { Entries = Map.empty }

    /// Insert (or overwrite) a key/value pair. Subject-last for pipelining.
    let insert (key: string) (value: 'V) (self: Trie<'V>) : Trie<'V> =
        { Entries = Map.add key value self.Entries }

    /// Insert all of the given entries.
    let insertMany (entries: (string * 'V) seq) (self: Trie<'V>) : Trie<'V> =
        { Entries = (self.Entries, entries) ||> Seq.fold (fun m (k, v) -> Map.add k v m) }

    /// A trie from a sequence of entries.
    let fromIterable (entries: (string * 'V) seq) : Trie<'V> = insertMany entries empty

    /// A trie from the given entries.
    let make (entries: (string * 'V) list) : Trie<'V> = fromIterable entries

    // --- size / emptiness ---

    let size (self: Trie<'V>) : int = self.Entries.Count
    let isEmpty (self: Trie<'V>) : bool = self.Entries.IsEmpty

    // --- lookup ---

    /// The value stored at the exact key, or `None`.
    let get (self: Trie<'V>) (key: string) : 'V option = Map.tryFind key self.Entries

    /// Whether the exact key is stored.
    let has (self: Trie<'V>) (key: string) : bool = Map.containsKey key self.Entries

    /// The value stored at the exact key; throws if missing.
    let getUnsafe (self: Trie<'V>) (key: string) : 'V =
        match Map.tryFind key self.Entries with
        | Some v -> v
        | None -> failwithf "Key not found: %s" key

    // --- removal ---

    let remove (key: string) (self: Trie<'V>) : Trie<'V> =
        { Entries = Map.remove key self.Entries }

    let removeMany (keys: string seq) (self: Trie<'V>) : Trie<'V> =
        { Entries = (self.Entries, keys) ||> Seq.fold (fun m k -> Map.remove k m) }

    // --- enumeration (always in sorted key order) ---

    /// Keys in sorted order.
    let keys (self: Trie<'V>) : string seq = Map.keys self.Entries

    /// Values in key order.
    let values (self: Trie<'V>) : 'V seq = Map.values self.Entries

    /// Entries in key order.
    let entries (self: Trie<'V>) : (string * 'V) seq = Map.toSeq self.Entries

    /// Entries as a sorted array.
    let toEntries (self: Trie<'V>) : (string * 'V)[] = Map.toArray self.Entries

    // --- prefix queries ---

    let private withPrefix (self: Trie<'V>) (prefix: string) : (string * 'V)[] =
        self.Entries
        |> Map.toArray
        |> Array.filter (fun (k, _) -> k.StartsWith(prefix, System.StringComparison.Ordinal))

    /// Keys having the given prefix, in sorted order.
    let keysWithPrefix (self: Trie<'V>) (prefix: string) : string seq =
        withPrefix self prefix |> Array.map fst :> string seq

    /// Values whose keys have the given prefix, in key order.
    let valuesWithPrefix (self: Trie<'V>) (prefix: string) : 'V seq =
        withPrefix self prefix |> Array.map snd :> 'V seq

    /// Entries whose keys have the given prefix, in key order.
    let entriesWithPrefix (self: Trie<'V>) (prefix: string) : (string * 'V) seq =
        withPrefix self prefix :> (string * 'V) seq

    /// Entries whose keys have the given prefix, as an array.
    let toEntriesWithPrefix (self: Trie<'V>) (prefix: string) : (string * 'V)[] =
        withPrefix self prefix

    /// The longest stored key that is a prefix of `input`, with its value.
    let longestPrefixOf (self: Trie<'V>) (input: string) : (string * 'V) option =
        self.Entries
        |> Map.toSeq
        |> Seq.filter (fun (k, _) -> input.StartsWith(k, System.StringComparison.Ordinal))
        |> Seq.sortByDescending (fun (k, _) -> k.Length)
        |> Seq.tryHead

    // --- transformations ---

    /// Map values; `f` receives `(value, key)`.
    let map (self: Trie<'V>) (f: 'V -> string -> 'W) : Trie<'W> =
        { Entries = self.Entries |> Map.map (fun k v -> f v k) }

    /// Keep entries where `f (value, key)` holds.
    let filter (self: Trie<'V>) (f: 'V -> string -> bool) : Trie<'V> =
        { Entries = self.Entries |> Map.filter (fun k v -> f v k) }

    /// Keep the `Ok` results of `f (value, key)`, discarding `Error`s.
    let filterMap (self: Trie<'V>) (f: 'V -> string -> Result<'W, 'X>) : Trie<'W> =
        let folder (acc: Map<string, 'W>) (k: string) (v: 'V) =
            match f v k with
            | Ok w -> Map.add k w acc
            | Error _ -> acc
        { Entries = Map.fold folder Map.empty self.Entries }

    /// Keep the `Some` values of an option-valued trie.
    let compact (self: Trie<'V option>) : Trie<'V> =
        let folder (acc: Map<string, 'V>) (k: string) (v: 'V option) =
            match v with
            | Some w -> Map.add k w acc
            | None -> acc
        { Entries = Map.fold folder Map.empty self.Entries }

    /// Apply `f` to the value at `key` if present; otherwise unchanged.
    let modify (key: string) (f: 'V -> 'V) (self: Trie<'V>) : Trie<'V> =
        match Map.tryFind key self.Entries with
        | Some v -> { Entries = Map.add key (f v) self.Entries }
        | None -> self

    // --- folds ---

    /// Fold over entries in key order; `f` receives `(acc, value, key)`.
    let reduce (zero: 'Acc) (f: 'Acc -> 'V -> string -> 'Acc) (self: Trie<'V>) : 'Acc =
        Map.fold (fun acc k v -> f acc v k) zero self.Entries

    /// Visit each entry in key order; `f` receives `(value, key)`.
    let forEach (f: 'V -> string -> unit) (self: Trie<'V>) : unit =
        self.Entries |> Map.iter (fun k v -> f v k)

    // --- rendering (matches Effect's toString) ---

    let rec private renderValue (v: obj) : string =
        match v with
        | null -> "null"
        | :? string as s -> "\"" + s + "\""
        | _ -> string v

    /// `Trie([["a",0],["b",1]])`, entries in key order.
    let render (self: Trie<'V>) : string =
        let parts =
            self.Entries
            |> Map.toList
            |> List.map (fun (k, v) -> sprintf "[\"%s\",%s]" k (renderValue (box v)))
        "Trie([" + String.concat "," parts + "])"
