namespace Effect

/// Port of repos/effect-smol/packages/effect/src/TxPriorityQueue.ts — a
/// transactional priority queue whose state lives in a `TxRef<Chunk<'a>>`.
/// Elements are kept ascending by the `Order` supplied at construction; `take`
/// and `peek` return the smallest. The retrying `take`/`peek` wait
/// transactionally while the queue is empty.
///
/// Porting adaptations (per CONVENTIONS):
///   - Like the rest of our STM layer, each public op is its OWN atomic
///     transaction (`TxRef.atomically`), rather than upstream's ambient
///     `Effect.tx` over a generator. Observable per-op behaviour is identical;
///     callers needing several ops in one transaction compose the underlying
///     `Stm` values (exposed implicitly via the same `TxRef` engine).
///   - Collection results are returned as F# `'a list` (upstream `Array`).
///   - JS-runtime machinery (`TypeId`, `Proto`, `toJSON`, `isTxPriorityQueue`,
///     `pipe`/`dual`) is dropped; ops take `self` first.
type TxPriorityQueue<'a> =
    internal
        { Ref: TxRef<Chunk<'a>>
          Order: Order<'a> }

[<RequireQualifiedAccess>]
module TxPriorityQueue =

    // --- internal helpers ---

    /// Sort an arbitrary sequence into a `Chunk` by the queue's order.
    let private sorted (ord: Order<'a>) (xs: seq<'a>) : Chunk<'a> =
        xs |> Seq.toList |> List.sortWith ord |> Chunk.make

    /// Insert `value` keeping ascending order, after any equal elements
    /// (stable). Mirrors upstream's binary-search `insertSorted`.
    let private insertSorted (ord: Order<'a>) (chunk: Chunk<'a>) (value: 'a) : Chunk<'a> =
        let arr = Chunk.toArray chunk
        let mutable lo = 0
        let mutable hi = arr.Length

        while lo < hi do
            let mid = (lo + hi) / 2
            if ord arr.[mid] value <= 0 then lo <- mid + 1 else hi <- mid

        Chunk.make (List.ofArray (Array.concat [ arr.[0 .. lo - 1]; [| value |]; arr.[lo..] ]))

    // --- constructors ---

    /// Create an empty queue with the given ordering. (TxPriorityQueue.empty)
    let empty (order: Order<'a>) : Effect<TxPriorityQueue<'a>, 'E, 'R> =
        TxRef.make (Chunk.empty: Chunk<'a>) |> Effect.map (fun ref -> { Ref = ref; Order = order })

    /// Create a queue from an iterable of elements. (TxPriorityQueue.fromIterable)
    let fromIterable (order: Order<'a>) (xs: seq<'a>) : Effect<TxPriorityQueue<'a>, 'E, 'R> =
        TxRef.make (sorted order xs) |> Effect.map (fun ref -> { Ref = ref; Order = order })

    /// Create a queue from a list of elements. (TxPriorityQueue.make — upstream's
    /// variadic constructor; F# takes an explicit list.)
    let make (order: Order<'a>) (elements: 'a list) : Effect<TxPriorityQueue<'a>, 'E, 'R> =
        fromIterable order elements

    // --- getters ---

    /// Number of elements in the queue. (TxPriorityQueue.size)
    let size (self: TxPriorityQueue<'a>) : Effect<int, 'E, 'R> =
        TxRef.atomically (TxRef.get self.Ref) |> Effect.map Chunk.size

    /// `true` if the queue is empty. (TxPriorityQueue.isEmpty)
    let isEmpty (self: TxPriorityQueue<'a>) : Effect<bool, 'E, 'R> = size self |> Effect.map (fun n -> n = 0)

    /// `true` if the queue has at least one element. (TxPriorityQueue.isNonEmpty)
    let isNonEmpty (self: TxPriorityQueue<'a>) : Effect<bool, 'E, 'R> = size self |> Effect.map (fun n -> n > 0)

    /// Observe the smallest element without removing it, retrying while empty.
    /// (TxPriorityQueue.peek)
    let peek (self: TxPriorityQueue<'a>) : Effect<'a, 'E, 'R> =
        TxRef.atomically (
            stm {
                let! chunk = TxRef.get self.Ref

                match Chunk.head chunk with
                | None -> return! TxRef.retry
                | Some head -> return head
            }
        )

    /// Observe the smallest element, `None` when empty. (TxPriorityQueue.peekOption)
    let peekOption (self: TxPriorityQueue<'a>) : Effect<'a option, 'E, 'R> =
        TxRef.atomically (TxRef.get self.Ref) |> Effect.map Chunk.head

    // --- mutations ---

    /// Insert an element in sorted position. (TxPriorityQueue.offer)
    let offer (self: TxPriorityQueue<'a>) (value: 'a) : Effect<unit, 'E, 'R> =
        TxRef.atomically (TxRef.update self.Ref (fun chunk -> insertSorted self.Order chunk value))

    /// Insert all elements from a sequence. (TxPriorityQueue.offerAll)
    let offerAll (self: TxPriorityQueue<'a>) (values: seq<'a>) : Effect<unit, 'E, 'R> =
        TxRef.atomically (
            TxRef.update self.Ref (fun chunk -> sorted self.Order (Seq.append (Chunk.toArray chunk) values))
        )

    /// Take the smallest element, retrying while empty. (TxPriorityQueue.take)
    let take (self: TxPriorityQueue<'a>) : Effect<'a, 'E, 'R> =
        TxRef.atomically (
            stm {
                let! chunk = TxRef.get self.Ref

                match Chunk.head chunk with
                | None -> return! TxRef.retry
                | Some head ->
                    do! TxRef.set self.Ref (Chunk.drop 1 chunk)
                    return head
            }
        )

    /// Take all elements, in priority order. (TxPriorityQueue.takeAll)
    let takeAll (self: TxPriorityQueue<'a>) : Effect<'a list, 'E, 'R> =
        TxRef.atomically (TxRef.modify self.Ref (fun chunk -> (chunk, Chunk.empty)))
        |> Effect.map (Chunk.toArray >> List.ofArray)

    /// Try to take the smallest element, `None` when empty. (TxPriorityQueue.takeOption)
    let takeOption (self: TxPriorityQueue<'a>) : Effect<'a option, 'E, 'R> =
        TxRef.atomically (
            TxRef.modify self.Ref (fun chunk ->
                match Chunk.head chunk with
                | None -> (None, chunk)
                | Some head -> (Some head, Chunk.drop 1 chunk))
        )

    /// Take up to `n` elements, in priority order. (TxPriorityQueue.takeUpTo)
    let takeUpTo (self: TxPriorityQueue<'a>) (n: int) : Effect<'a list, 'E, 'R> =
        TxRef.atomically (TxRef.modify self.Ref (fun chunk -> (Chunk.take n chunk, Chunk.drop n chunk)))
        |> Effect.map (Chunk.toArray >> List.ofArray)

    /// Remove elements matching the predicate. (TxPriorityQueue.removeIf)
    let removeIf (self: TxPriorityQueue<'a>) (predicate: 'a -> bool) : Effect<unit, 'E, 'R> =
        TxRef.atomically (TxRef.update self.Ref (Chunk.filter (fun a -> not (predicate a))))

    /// Keep only elements matching the predicate. (TxPriorityQueue.retainIf)
    let retainIf (self: TxPriorityQueue<'a>) (predicate: 'a -> bool) : Effect<unit, 'E, 'R> =
        TxRef.atomically (TxRef.update self.Ref (Chunk.filter predicate))

    // --- converting ---

    /// All elements in priority order, without removing them. (TxPriorityQueue.toArray)
    let toList (self: TxPriorityQueue<'a>) : Effect<'a list, 'E, 'R> =
        TxRef.atomically (TxRef.get self.Ref) |> Effect.map (Chunk.toArray >> List.ofArray)
