namespace Effect

/// Port of repos/effect-smol/packages/effect/src/TxHashMap.ts — a `HashMap` held
/// in transactional state. A `TxHashMap<'k,'v>` keeps its current `HashMap` in a
/// `TxRef`; every op delegates to `TxRef.get`/`modify`/`update`, inheriting STM
/// semantics. Ops return `Stm<_>`; `make`/`empty` return `Effect`.
///
/// (`HashMap` in this port is a `Map`-backed structure, hence the `comparison`
/// constraint on keys.)
type TxHashMap<'k, 'v when 'k: comparison> = internal { Ref: TxRef<HashMap<'k, 'v>> }

[<RequireQualifiedAccess>]
module TxHashMap =

    // --- constructors ---

    /// Create a `TxHashMap` from an initial `HashMap`. (TxHashMap.make)
    let make (initial: HashMap<'k, 'v>) : Effect<TxHashMap<'k, 'v>, 'E, 'R> =
        TxRef.make initial |> Effect.map (fun ref -> { Ref = ref })

    /// Create an empty `TxHashMap`. (TxHashMap.empty)
    let empty () : Effect<TxHashMap<'k, 'v>, 'E, 'R> = make HashMap.empty

    /// Create from key/value pairs. (TxHashMap.fromIterable)
    let fromIterable (entries: ('k * 'v) list) : Effect<TxHashMap<'k, 'v>, 'E, 'R> = make (HashMap.make entries)

    // --- transactional operations ---

    /// Snapshot the whole map. (TxHashMap.snapshot)
    let getAll (self: TxHashMap<'k, 'v>) : Stm<HashMap<'k, 'v>> = TxRef.get self.Ref

    /// Look up a key. (TxHashMap.get)
    let get (self: TxHashMap<'k, 'v>) (key: 'k) : Stm<'v option> =
        stm {
            let! m = TxRef.get self.Ref
            return HashMap.get m key
        }

    /// Whether a key is present. (TxHashMap.has)
    let has (self: TxHashMap<'k, 'v>) (key: 'k) : Stm<bool> =
        stm {
            let! m = TxRef.get self.Ref
            return HashMap.has m key
        }

    /// Insert/replace a key. (TxHashMap.set)
    let set (self: TxHashMap<'k, 'v>) (key: 'k) (value: 'v) : Stm<unit> =
        TxRef.update self.Ref (fun m -> HashMap.set m key value)

    /// Remove a key. (TxHashMap.remove)
    let remove (self: TxHashMap<'k, 'v>) (key: 'k) : Stm<unit> =
        TxRef.update self.Ref (fun m -> HashMap.remove m key)

    /// Number of entries. (TxHashMap.size)
    let size (self: TxHashMap<'k, 'v>) : Stm<int> = stm { let! m = TxRef.get self.Ref in return HashMap.size m }

    /// Whether the map is empty. (TxHashMap.isEmpty)
    let isEmpty (self: TxHashMap<'k, 'v>) : Stm<bool> = stm { let! m = TxRef.get self.Ref in return HashMap.isEmpty m }
