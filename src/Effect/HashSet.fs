namespace Effect

/// `HashSet` — an immutable set of unique values.
///
/// Port of repos/effect-smol/packages/effect/src/HashSet.ts. Upstream stores
/// values in a `HashMap` (HAMT) keyed by the `Equal`/`Hash` modules. Per the
/// porting conventions we reuse FSharp.Core's persistent `Set<'V>`, which gives
/// uniqueness, order-independent structural equality, and all the set algebra
/// (union/intersection/difference/subset) out of the box, resolving membership
/// through F#'s built-in structural `comparison` — so we never touch the
/// `Equal`/`Hash` modules (decoupling rule).
///
/// Omissions vs. upstream (deliberate): `isHashSet` and other `unknown`-narrowing
/// runtime guards (CONVENTIONS §3), and the HKT / `Pipeable` / `Inspectable`
/// plumbing (CONVENTIONS §4).
type HashSet<'V when 'V: comparison> = private { values: Set<'V> }

[<RequireQualifiedAccess>]
module HashSet =

    /// Upstream brand constant (kept for parity).
    [<Literal>]
    let TypeId = "~effect/HashSet"

    // --- constructors ---

    /// The empty set. (HashSet.empty)
    let empty<'V when 'V: comparison> : HashSet<'V> = { values = Set.empty }

    /// A set from an iterable; duplicates collapse. (HashSet.fromIterable)
    let fromIterable (values: seq<'V>) : HashSet<'V> = { values = Set.ofSeq values }

    /// A set from a list of values; duplicates collapse. (HashSet.make)
    let make (values: 'V list) : HashSet<'V> = fromIterable values

    // --- getters / elements ---

    /// The number of values. (HashSet.size)
    let size (self: HashSet<'V>) : int = self.values.Count

    /// Whether the set has no values. (HashSet.isEmpty)
    let isEmpty (self: HashSet<'V>) : bool = Set.isEmpty self.values

    /// Whether the value is a member. (HashSet.has)
    let has (self: HashSet<'V>) (value: 'V) : bool = Set.contains value self.values

    // --- mutations (persistent) ---

    /// Add a value. Returns the same set (by reference) when already present. (HashSet.add)
    let add (self: HashSet<'V>) (value: 'V) : HashSet<'V> =
        match Set.contains value self.values with
        | true -> self
        | false -> { values = Set.add value self.values }

    /// Remove a value. Returns the same set (by reference) when absent. (HashSet.remove)
    let remove (self: HashSet<'V>) (value: 'V) : HashSet<'V> =
        match Set.contains value self.values with
        | true -> { values = Set.remove value self.values }
        | false -> self

    // --- set algebra ---

    /// Values present in either set. (HashSet.union)
    let union (self: HashSet<'V>) (that: HashSet<'V>) : HashSet<'V> =
        { values = Set.union self.values that.values }

    /// Values present in both sets. (HashSet.intersection)
    let intersection (self: HashSet<'V>) (that: HashSet<'V>) : HashSet<'V> =
        { values = Set.intersect self.values that.values }

    /// Values in `self` but not in `that`. (HashSet.difference)
    let difference (self: HashSet<'V>) (that: HashSet<'V>) : HashSet<'V> =
        { values = Set.difference self.values that.values }

    /// Whether every value of `self` is in `that`. (HashSet.isSubset)
    let isSubset (self: HashSet<'V>) (that: HashSet<'V>) : bool =
        Set.isSubset self.values that.values

    // --- functional ops ---

    /// Map every value; duplicate results collapse. (HashSet.map)
    let map (self: HashSet<'V>) (f: 'V -> 'U) : HashSet<'U> =
        { values = Set.map f self.values }

    /// Keep values matching the predicate. (HashSet.filter)
    let filter (self: HashSet<'V>) (predicate: 'V -> bool) : HashSet<'V> =
        { values = Set.filter predicate self.values }

    /// Whether any value matches the predicate. (HashSet.some)
    let some (self: HashSet<'V>) (predicate: 'V -> bool) : bool =
        Set.exists predicate self.values

    /// Whether every value matches the predicate (vacuously true when empty). (HashSet.every)
    let every (self: HashSet<'V>) (predicate: 'V -> bool) : bool =
        Set.forall predicate self.values

    /// Fold over the values. (HashSet.reduce)
    let reduce (self: HashSet<'V>) (zero: 'U) (f: 'U -> 'V -> 'U) : 'U =
        Set.fold f zero self.values

    // --- iteration / conversion ---

    /// The values as a seq, ascending. (HashSet iteration)
    let toSeq (self: HashSet<'V>) : seq<'V> = Set.toSeq self.values

    /// The values as a list, ascending.
    let toList (self: HashSet<'V>) : 'V list = Set.toList self.values
