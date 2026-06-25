namespace Effect

/// Port of repos/effect-smol/packages/effect/src/Chunk.ts.
///
/// A `Chunk<'A>` is an immutable, ordered sequence. Upstream backs it with a
/// balanced rope of `IArray`/`IConcat`/`ISingleton`/`ISlice` nodes so that
/// append/prepend/concat are cheap and structure-sharing. That persistent-rope
/// machinery is a performance optimisation of the TS runtime, not part of the
/// observable contract, so here a `Chunk` is simply a record wrapping an F#
/// array. F# arrays give us structural equality/comparison/hashing for free,
/// which is exactly what the upstream `Equal`/`Hash` instances provide.
///
/// Omissions (noted per CONVENTIONS.md):
///   * `isChunk` and the `TypeId` brand — JS runtime guards over `unknown`,
///     dead weight under F#'s static types.
///   * The `backing`/`depth`/`ISingleton` internals and their tests — they
///     assert on the rope representation, which we intentionally don't have.
///   * `toJSON`/`inspect`/`.pipe()` — JS serialization / fluent-method plumbing.
///   * HKT/typeclass plumbing (`Covariant`, `dual`, etc.).
/// The `Equal`/`Hash` traits are satisfied with FSharp.Core structural equality
/// (see the decoupling rule in the porting brief).
type Chunk<'A> = { Values: 'A[] }

[<RequireQualifiedAccess>]
module Chunk =

    // --- constructors ---

    /// The empty chunk.
    let empty<'A> : Chunk<'A> = { Values = Array.empty }

    /// A chunk of a single element (upstream `Chunk.of`; `of` is an F# keyword).
    let singleton (a: 'A) : Chunk<'A> = { Values = [| a |] }

    /// A chunk from the given elements.
    let make (xs: 'A list) : Chunk<'A> = { Values = List.toArray xs }

    /// Wrap an array without copying (upstream `fromArrayUnsafe`). The caller
    /// must not mutate the array afterwards.
    let fromArrayUnsafe (xs: 'A[]) : Chunk<'A> = { Values = xs }

    /// A chunk from any sequence (upstream `fromIterable`).
    let fromSeq (xs: 'A seq) : Chunk<'A> = { Values = Seq.toArray xs }

    // --- conversions ---

    /// A fresh array copy; mutating it does not affect the chunk.
    let toArray (self: Chunk<'A>) : 'A[] = Array.copy self.Values

    /// Same as `toArray` (F# has no separate readonly-array type).
    let toReadonlyArray (self: Chunk<'A>) : 'A[] = Array.copy self.Values

    // --- size / emptiness ---

    let size (self: Chunk<'A>) : int = self.Values.Length
    let isEmpty (self: Chunk<'A>) : bool = Array.isEmpty self.Values
    let isNonEmpty (self: Chunk<'A>) : bool = not (Array.isEmpty self.Values)

    // --- indexing ---

    /// The element at `i`, or `None` if out of bounds.
    let get (i: int) (self: Chunk<'A>) : 'A option = Array.tryItem i self.Values

    /// The element at `i`; throws if out of bounds (upstream `getUnsafe`).
    let getUnsafe (i: int) (self: Chunk<'A>) : 'A =
        if i >= 0 && i < self.Values.Length then
            self.Values.[i]
        else
            failwithf "Index out of bounds: %d" i

    let head (self: Chunk<'A>) : 'A option = Array.tryHead self.Values

    /// The first element; throws if the chunk is empty (upstream `headUnsafe`).
    let headUnsafe (self: Chunk<'A>) : 'A = getUnsafe 0 self

    let last (self: Chunk<'A>) : 'A option = Array.tryLast self.Values
    let lastUnsafe (self: Chunk<'A>) : 'A = getUnsafe (self.Values.Length - 1) self

    /// Every element after the first, or `None` for an empty chunk.
    let tail (self: Chunk<'A>) : Chunk<'A> option =
        match self.Values.Length with
        | 0 -> None
        | _ -> Some { Values = Array.sub self.Values 1 (self.Values.Length - 1) }

    // --- append / prepend / concat ---

    let append (a: 'A) (self: Chunk<'A>) : Chunk<'A> =
        { Values = Array.append self.Values [| a |] }

    let prepend (a: 'A) (self: Chunk<'A>) : Chunk<'A> =
        { Values = Array.append [| a |] self.Values }

    /// `self` followed by `that`.
    let appendAll (self: Chunk<'A>) (that: Chunk<'A>) : Chunk<'A> =
        { Values = Array.append self.Values that.Values }

    /// `that` followed by `self`.
    let prependAll (self: Chunk<'A>) (that: Chunk<'A>) : Chunk<'A> =
        { Values = Array.append that.Values self.Values }

    // --- taking / dropping ---

    let take (n: int) (self: Chunk<'A>) : Chunk<'A> =
        { Values = Array.truncate (max 0 n) self.Values }

    let drop (n: int) (self: Chunk<'A>) : Chunk<'A> =
        let len = self.Values.Length

        if n <= 0 then self
        elif n >= len then empty
        else { Values = Array.sub self.Values n (len - n) }

    let dropRight (n: int) (self: Chunk<'A>) : Chunk<'A> =
        take (self.Values.Length - max 0 n) self

    /// The last `n` elements (upstream `takeRight`); `n <= 0` gives the empty
    /// chunk, `n >= size` gives the whole chunk.
    let takeRight (n: int) (self: Chunk<'A>) : Chunk<'A> = drop (self.Values.Length - n) self

    let takeWhile (predicate: 'A -> bool) (self: Chunk<'A>) : Chunk<'A> =
        { Values = Array.takeWhile predicate self.Values }

    let dropWhile (predicate: 'A -> bool) (self: Chunk<'A>) : Chunk<'A> =
        { Values = Array.skipWhile predicate self.Values }

    // --- splitting ---

    let splitAt (n: int) (self: Chunk<'A>) : Chunk<'A> * Chunk<'A> = take n self, drop n self

    /// Split a non-empty chunk at `n` (normalised to at least 1).
    let splitNonEmptyAt (self: Chunk<'A>) (n: int) : Chunk<'A> * Chunk<'A> =
        let n' = max 1 n

        if n' >= self.Values.Length then
            self, empty
        else
            take n' self, drop n' self

    /// Split on the first element matching the predicate; the match goes right.
    let splitWhere (self: Chunk<'A>) (predicate: 'A -> bool) : Chunk<'A> * Chunk<'A> =
        let i =
            match Array.tryFindIndex predicate self.Values with
            | Some idx -> idx
            | None -> self.Values.Length

        splitAt i self

    /// Group into consecutive chunks of `n` elements (the last may be smaller).
    /// Mirrors upstream: `n <= 1` yields singleton chunks.
    let chunksOf (n: int) (self: Chunk<'A>) : Chunk<Chunk<'A>> =
        let groups = ResizeArray<Chunk<'A>>()
        let current = ResizeArray<'A>()

        for a in self.Values do
            current.Add a

            if current.Count >= n then
                groups.Add { Values = current.ToArray() }
                current.Clear()

        if current.Count > 0 then
            groups.Add { Values = current.ToArray() }

        { Values = groups.ToArray() }

    /// Split into up to `n` chunks, distributing elements in order.
    let split (n: int) (self: Chunk<'A>) : Chunk<Chunk<'A>> =
        let len = self.Values.Length
        let groupSize = int (ceil (float len / floor (float n)))
        chunksOf groupSize self

    // --- transformations ---

    let map (self: Chunk<'A>) (f: 'A -> int -> 'B) : Chunk<'B> =
        { Values = self.Values |> Array.mapi (fun i a -> f a i) }

    /// Stateful map; returns the final state and the mapped chunk.
    let mapAccum (self: Chunk<'A>) (s: 'S) (f: 'S -> 'A -> 'S * 'B) : 'S * Chunk<'B> =
        let mutable state = s
        let out = Array.zeroCreate self.Values.Length

        self.Values
        |> Array.iteri (fun i a ->
            let s', b = f state a
            state <- s'
            out.[i] <- b)

        state, { Values = out }

    /// Map each element to a chunk and concatenate. `f` gets `(a, index)`
    /// (parity with `map`/`forEach`).
    let flatMap (self: Chunk<'A>) (f: 'A -> int -> Chunk<'B>) : Chunk<'B> =
        { Values = self.Values |> Array.mapi (fun i a -> (f a i).Values) |> Array.concat }

    let flatten (self: Chunk<Chunk<'A>>) : Chunk<'A> =
        { Values = self.Values |> Array.collect (fun c -> c.Values) }

    let reverse (self: Chunk<'A>) : Chunk<'A> = { Values = Array.rev self.Values }

    // --- filtering ---

    let filter (predicate: 'A -> bool) (self: Chunk<'A>) : Chunk<'A> =
        { Values = Array.filter predicate self.Values }

    /// Keep the `Ok` results of `f`, discarding `Error`s. `f` gets `(a, index)`.
    let filterMap (self: Chunk<'A>) (f: 'A -> int -> Result<'B, 'X>) : Chunk<'B> =
        let out = ResizeArray<'B>()

        self.Values
        |> Array.iteri (fun i a ->
            match f a i with
            | Ok b -> out.Add b
            | Error _ -> ())

        { Values = out.ToArray() }

    /// Like `filterMap`, but stop at the first `Error`.
    let filterMapWhile (self: Chunk<'A>) (f: 'A -> Result<'B, 'X>) : Chunk<'B> =
        let out = ResizeArray<'B>()
        let mutable go = true
        let mutable i = 0

        while go && i < self.Values.Length do
            match f self.Values.[i] with
            | Ok b ->
                out.Add b
                i <- i + 1
            | Error _ -> go <- false

        { Values = out.ToArray() }

    /// Keep the `Some` values of an `option` chunk.
    let compact (self: Chunk<'A option>) : Chunk<'A> =
        { Values = self.Values |> Array.choose id }

    /// Split with a `Result`-returning function into `(excluded, satisfying)`,
    /// i.e. `(Error`s, `Ok`s`)`. `f` gets `(a, index)`.
    let partition (self: Chunk<'A>) (f: 'A -> int -> Result<'Pass, 'Fail>) : Chunk<'Fail> * Chunk<'Pass> =
        let fails = ResizeArray<'Fail>()
        let passes = ResizeArray<'Pass>()

        self.Values
        |> Array.iteri (fun i a ->
            match f a i with
            | Ok p -> passes.Add p
            | Error e -> fails.Add e)

        { Values = fails.ToArray() }, { Values = passes.ToArray() }

    /// Separate a chunk of `Result`s into `(failures, successes)`.
    let separate (self: Chunk<Result<'A, 'E>>) : Chunk<'E> * Chunk<'A> = partition self (fun r _ -> r)

    // --- zipping ---

    let zipWith (self: Chunk<'A>) (that: Chunk<'B>) (f: 'A -> 'B -> 'C) : Chunk<'C> =
        let n = min self.Values.Length that.Values.Length
        { Values = Array.init n (fun i -> f self.Values.[i] that.Values.[i]) }

    let zip (self: Chunk<'A>) (that: Chunk<'B>) : Chunk<'A * 'B> = zipWith self that (fun a b -> a, b)

    let unzip (self: Chunk<'A * 'B>) : Chunk<'A> * Chunk<'B> =
        let a, b = Array.unzip self.Values
        { Values = a }, { Values = b }

    // --- element-wise edits ---

    /// Apply `f` to the element at `i`, or `None` if out of bounds.
    let modify (self: Chunk<'A>) (i: int) (f: 'A -> 'A) : Chunk<'A> option =
        if i >= 0 && i < self.Values.Length then
            let copy = Array.copy self.Values
            copy.[i] <- f copy.[i]
            Some { Values = copy }
        else
            None

    /// Replace the element at `i`, or `None` if out of bounds.
    let replace (self: Chunk<'A>) (i: int) (b: 'A) : Chunk<'A> option = modify self i (fun _ -> b)

    /// Delete the element at `i`; out-of-bounds returns the chunk unchanged.
    let remove (self: Chunk<'A>) (i: int) : Chunk<'A> =
        if i >= 0 && i < self.Values.Length then
            { Values =
                Array.append (Array.sub self.Values 0 i) (Array.sub self.Values (i + 1) (self.Values.Length - i - 1)) }
        else
            self

    // --- predicates / iteration ---

    let some (self: Chunk<'A>) (predicate: 'A -> bool) : bool = Array.exists predicate self.Values

    /// Whether `predicate` holds for every element (vacuously `true` when empty).
    let every (self: Chunk<'A>) (predicate: 'A -> bool) : bool = Array.forall predicate self.Values

    let forEach (self: Chunk<'A>) (f: 'A -> int -> unit) : unit =
        self.Values |> Array.iteri (fun i a -> f a i)

    /// Whether the chunk contains `a` (FSharp.Core structural equality).
    let contains (self: Chunk<'A>) (a: 'A) : bool = Array.contains a self.Values

    /// Whether the chunk contains an element equivalent to `a` under
    /// `isEquivalent`.
    let containsWith (isEquivalent: 'A -> 'A -> bool) (self: Chunk<'A>) (a: 'A) : bool =
        self.Values |> Array.exists (isEquivalent a)

    /// The first element satisfying `predicate`, or `None`.
    let findFirst (self: Chunk<'A>) (predicate: 'A -> bool) : 'A option = Array.tryFind predicate self.Values

    /// The index of the first element satisfying `predicate`, or `None`.
    let findFirstIndex (self: Chunk<'A>) (predicate: 'A -> bool) : int option =
        Array.tryFindIndex predicate self.Values

    /// The last element satisfying `predicate`, or `None`.
    let findLast (self: Chunk<'A>) (predicate: 'A -> bool) : 'A option = Array.tryFindBack predicate self.Values

    /// The index of the last element satisfying `predicate`, or `None`.
    let findLastIndex (self: Chunk<'A>) (predicate: 'A -> bool) : int option =
        Array.tryFindIndexBack predicate self.Values

    // --- folding ---

    /// Left-to-right fold; `f` gets `(acc, element, index)`.
    let reduce (self: Chunk<'A>) (b: 'B) (f: 'B -> 'A -> int -> 'B) : 'B =
        let mutable acc = b

        for i in 0 .. self.Values.Length - 1 do
            acc <- f acc self.Values.[i] i

        acc

    /// Right-to-left fold; `f` gets `(acc, element, index)` where `index` is the
    /// element's original position.
    let reduceRight (self: Chunk<'A>) (b: 'B) (f: 'B -> 'A -> int -> 'B) : 'B =
        let mutable acc = b

        for i in self.Values.Length - 1 .. -1 .. 0 do
            acc <- f acc self.Values.[i] i

        acc

    /// Join a chunk of strings with `sep` between elements.
    let join (self: Chunk<string>) (sep: string) : string = String.concat sep self.Values

    // --- ordering ---

    /// Sort the chunk with `order` (a three-way comparison such as `compare`,
    /// upstream's `Order`). Stable.
    let sort (self: Chunk<'A>) (order: 'A -> 'A -> int) : Chunk<'A> =
        { Values = Array.sortWith order self.Values }

    /// Sort by the key projected with `f`, ordering keys with `order`
    /// (a three-way comparison such as `compare`). Stable.
    let sortWith (self: Chunk<'A>) (f: 'A -> 'B) (order: 'B -> 'B -> int) : Chunk<'A> =
        { Values = Array.sortWith (fun a b -> order (f a) (f b)) self.Values }

    // --- set-like operations (use FSharp.Core structural equality) ---

    /// Remove duplicates, keeping the first occurrence of each value.
    let dedupe (self: Chunk<'A>) : Chunk<'A> = { Values = Array.distinct self.Values }

    /// Remove runs of adjacent identical elements.
    let dedupeAdjacent (self: Chunk<'A>) : Chunk<'A> =
        let out = ResizeArray<'A>()

        for a in self.Values do
            if out.Count = 0 || out.[out.Count - 1] <> a then
                out.Add a

        { Values = out.ToArray() }

    /// Concatenation with duplicates removed (first occurrence wins).
    let union (self: Chunk<'A>) (that: Chunk<'A>) : Chunk<'A> =
        { Values = Array.distinct (Array.append self.Values that.Values) }

    /// Elements of `self` that also appear in `that`, in `self`'s order.
    let intersection (self: Chunk<'A>) (that: Chunk<'A>) : Chunk<'A> =
        { Values = self.Values |> Array.filter (fun a -> Array.contains a that.Values) }

    /// Elements of `self` not present in `that`, in `self`'s order.
    let difference (self: Chunk<'A>) (that: Chunk<'A>) : Chunk<'A> =
        { Values = self.Values |> Array.filter (fun a -> not (Array.contains a that.Values)) }

    /// `difference` under a caller-supplied equivalence.
    let differenceWith (isEquivalent: 'A -> 'A -> bool) (self: Chunk<'A>) (that: Chunk<'A>) : Chunk<'A> =
        { Values =
            self.Values
            |> Array.filter (fun a -> not (that.Values |> Array.exists (isEquivalent a))) }

    // --- generators ---

    /// A chunk of length `max 1 n` with element `i` built by `f i`.
    let makeBy (n: int) (f: int -> 'A) : Chunk<'A> = { Values = Array.init (max 1 n) f }

    /// Consecutive integers from `start` to `end` inclusive; if `start > end`,
    /// a single-element chunk `[start]`.
    let range (start: int) (endInclusive: int) : Chunk<int> =
        if start <= endInclusive then
            makeBy (endInclusive - start + 1) (fun i -> start + i)
        else
            singleton start

    // --- equivalence ---

    /// Build a chunk equivalence from an element equivalence.
    let makeEquivalence (isEquivalent: 'A -> 'A -> bool) : Chunk<'A> -> Chunk<'A> -> bool =
        fun a b ->
            a.Values.Length = b.Values.Length
            && Array.forall2 isEquivalent a.Values b.Values

    // --- rendering (matches Effect's toString) ---

    let rec private renderObj (v: obj) : string =
        match v with
        | null -> "null"
        | :? string as s -> "\"" + s + "\""
        | _ ->
            let t = v.GetType()

            if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Chunk<obj>> then
                let values = t.GetProperty("Values").GetValue(v) :?> System.Array
                let parts = [ for x in values -> renderObj (box x) ]
                "Chunk([" + String.concat "," parts + "])"
            else
                string v

    /// `Chunk([0,1,2])`, recursing into nested chunks.
    let render (self: Chunk<'A>) : string =
        let parts = self.Values |> Array.map (fun a -> renderObj (box a))
        "Chunk([" + String.concat "," parts + "])"
