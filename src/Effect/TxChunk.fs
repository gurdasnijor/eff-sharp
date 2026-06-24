namespace Effect

/// Port of repos/effect-smol/packages/effect/src/TxChunk.ts — a `Chunk` held in
/// transactional state. A `TxChunk<'a>` keeps its current `Chunk<'a>` in a
/// `TxRef`, so reads/updates commit atomically with other transactional ops.
///
/// As upstream, every operation is just a `TxRef.get`/`modify`/`set` on the
/// backing ref, so it inherits the STM semantics (optimistic validation, retry).
/// Operations return `Stm<_>` (compose with `stm { }` / run with
/// `TxRef.atomically`); `make`/`fromIterable` return `Effect` like upstream.
type TxChunk<'a> = internal { Ref: TxRef<Chunk<'a>> }

[<RequireQualifiedAccess>]
module TxChunk =

    // --- constructors ---

    /// Create a `TxChunk` from an initial `Chunk`. (TxChunk.make)
    let make (initial: Chunk<'a>) : Effect<TxChunk<'a>, 'E, 'R> =
        TxRef.make initial |> Effect.map (fun ref -> { Ref = ref })

    /// Create a `TxChunk` from any sequence. (TxChunk.fromIterable)
    let fromIterable (values: seq<'a>) : Effect<TxChunk<'a>, 'E, 'R> =
        make { Values = Seq.toArray values }

    // --- transactional operations ---

    /// Read the current chunk. (TxChunk.get)
    let get (self: TxChunk<'a>) : Stm<Chunk<'a>> = TxRef.get self.Ref

    /// Replace the current chunk. (TxChunk.set)
    let set (self: TxChunk<'a>) (chunk: Chunk<'a>) : Stm<unit> = TxRef.set self.Ref chunk

    /// Replace the chunk via `f`, returning a separate result. (TxChunk.modify)
    let modify (self: TxChunk<'a>) (f: Chunk<'a> -> 'r * Chunk<'a>) : Stm<'r> = TxRef.modify self.Ref f

    /// Transform the chunk in place. (TxChunk.update)
    let update (self: TxChunk<'a>) (f: Chunk<'a> -> Chunk<'a>) : Stm<unit> = TxRef.update self.Ref f

    /// Current size. (TxChunk.size)
    let size (self: TxChunk<'a>) : Stm<int> = stm { let! c = get self in return Chunk.size c }

    /// Whether the chunk is empty. (TxChunk.isEmpty)
    let isEmpty (self: TxChunk<'a>) : Stm<bool> = stm { let! c = get self in return Chunk.isEmpty c }

    /// Append a value. (TxChunk.append)
    let append (self: TxChunk<'a>) (value: 'a) : Stm<unit> = update self (Chunk.append value)

    /// Prepend a value. (TxChunk.prepend)
    let prepend (self: TxChunk<'a>) (value: 'a) : Stm<unit> = update self (Chunk.prepend value)
