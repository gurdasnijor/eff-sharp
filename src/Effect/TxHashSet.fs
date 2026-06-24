namespace Effect

/// Port of repos/effect-smol/packages/effect/src/TxHashSet.ts — a `HashSet` held
/// in transactional state. A `TxHashSet<'v>` keeps its current `HashSet` in a
/// `TxRef`; every op delegates to `TxRef.get`/`update`, inheriting STM semantics.
/// Ops return `Stm<_>`; `make`/`empty` return `Effect`.
///
/// (`HashSet` in this port is a `Set`-backed structure, hence the `comparison`
/// constraint on elements.)
type TxHashSet<'v when 'v: comparison> = internal { Ref: TxRef<HashSet<'v>> }

[<RequireQualifiedAccess>]
module TxHashSet =

    // --- constructors ---

    /// Create a `TxHashSet` from an initial `HashSet`. (TxHashSet.make)
    let make (initial: HashSet<'v>) : Effect<TxHashSet<'v>, 'E, 'R> =
        TxRef.make initial |> Effect.map (fun ref -> { Ref = ref })

    /// Create an empty `TxHashSet`. (TxHashSet.empty)
    let empty () : Effect<TxHashSet<'v>, 'E, 'R> = make HashSet.empty

    /// Create from any sequence. (TxHashSet.fromIterable)
    let fromIterable (values: seq<'v>) : Effect<TxHashSet<'v>, 'E, 'R> = make (HashSet.fromIterable values)

    // --- transactional operations ---

    /// Snapshot the whole set. (TxHashSet.snapshot)
    let getAll (self: TxHashSet<'v>) : Stm<HashSet<'v>> = TxRef.get self.Ref

    /// Whether a value is present. (TxHashSet.has)
    let has (self: TxHashSet<'v>) (value: 'v) : Stm<bool> =
        stm {
            let! s = TxRef.get self.Ref
            return HashSet.has s value
        }

    /// Add a value. (TxHashSet.add)
    let add (self: TxHashSet<'v>) (value: 'v) : Stm<unit> =
        TxRef.update self.Ref (fun s -> HashSet.add s value)

    /// Remove a value. (TxHashSet.remove)
    let remove (self: TxHashSet<'v>) (value: 'v) : Stm<unit> =
        TxRef.update self.Ref (fun s -> HashSet.remove s value)

    /// Number of elements. (TxHashSet.size)
    let size (self: TxHashSet<'v>) : Stm<int> = stm { let! s = TxRef.get self.Ref in return HashSet.size s }

    /// Whether the set is empty. (TxHashSet.isEmpty)
    let isEmpty (self: TxHashSet<'v>) : Stm<bool> = stm { let! s = TxRef.get self.Ref in return HashSet.isEmpty s }
