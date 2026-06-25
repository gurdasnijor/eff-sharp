namespace Effect

/// Port of repos/effect-smol/packages/effect/src/MutableHashSet.ts — a mutable
/// set of unique values, updated in place.
///
/// As upstream, it is built on `MutableHashMap`: each value is stored as a key
/// mapped to `true`, so uniqueness follows the map's (structural) equality.
///
/// Omissions (intentional, JS-runtime-only): `isMutableHashSet` (a `hasProperty`
/// guard over `unknown` — meaningless under F#'s static types), the
/// `toJSON`/`NodeInspectSymbol` inspection hooks, and the `pipe`/dual data-last
/// currying. Every function takes `self` first.
type MutableHashSet<'V when 'V: equality> = { KeyMap: MutableHashMap<'V, bool> }

[<RequireQualifiedAccess>]
module MutableHashSet =


    // --- constructors ---

    /// Creates an empty mutable hash set. (MutableHashSet.empty)
    let empty<'V when 'V: equality> () : MutableHashSet<'V> = { KeyMap = MutableHashMap.empty () }

    /// Adds `value`, mutating in place; duplicates are ignored. Returns the same
    /// set. (MutableHashSet.add)
    let add (self: MutableHashSet<'V>) (value: 'V) : MutableHashSet<'V> =
        MutableHashMap.set self.KeyMap value true |> ignore
        self

    /// Creates a set from a sequence of values, de-duplicating.
    /// (MutableHashSet.fromIterable)
    let fromIterable (values: seq<'V>) : MutableHashSet<'V> =
        let self = empty ()

        for v in values do
            add self v |> ignore

        self

    /// Creates a set from explicit values, de-duplicating. (MutableHashSet.make)
    let make (values: 'V list) : MutableHashSet<'V> = fromIterable values

    // --- elements ---

    /// Whether `value` is a member. (MutableHashSet.has)
    let has (self: MutableHashSet<'V>) (value: 'V) : bool = MutableHashMap.has self.KeyMap value

    /// The number of unique values. (MutableHashSet.size)
    let size (self: MutableHashSet<'V>) : int = MutableHashMap.size self.KeyMap

    /// The values, in insertion order.
    let toSeq (self: MutableHashSet<'V>) : seq<'V> = MutableHashMap.keys self.KeyMap

    // --- mutations ---

    /// Removes `value` if present, mutating in place. Returns the same set.
    /// (MutableHashSet.remove)
    let remove (self: MutableHashSet<'V>) (value: 'V) : MutableHashSet<'V> =
        MutableHashMap.remove self.KeyMap value |> ignore
        self

    /// Removes every value, mutating in place. Returns the same set.
    /// (MutableHashSet.clear)
    let clear (self: MutableHashSet<'V>) : MutableHashSet<'V> =
        MutableHashMap.clear self.KeyMap |> ignore
        self
