namespace Effect

open System.Collections.Generic

/// Port of repos/effect-smol/packages/effect/src/MutableList.ts — a mutable FIFO
/// list that collects ordered values at the back and drains them from the front.
///
/// The structure mirrors upstream: a singly-linked chain of *buckets*, each a
/// growable array with a consumed `Offset`, so taking from the front is cheap
/// and bulk appends can be linked in as whole chunks. `Head`/`Tail`/`Length` are
/// exposed as upstream does.
///
/// Lowerings / omissions:
///   - The `Empty` sentinel symbol returned by `take` is lowered to F#'s native
///     `'A option` — `take` returns `None` for an empty list, `Some x` otherwise.
///   - The per-bucket `mutable` flag and the GC-nulling of consumed slots
///     (`array[i] = undefined`) are JS memory optimizations; dropped. Bucket
///     arrays are always owned copies, so `append` can always extend the tail.
///   - The zero-copy fast path in `takeN`, and the separate `takeNVoid` /
///     `appendAllUnsafe` `mutable` parameter, are optimization-only; folded away.
///   - `remove` compares with F#'s structural `<>` (decoupling rule) rather than
///     JS strict `!==`; identical for the primitives it is used with.
///
/// The list type and storage node are nested under the module (so they read as
/// `MutableList.MutableList<'A>` / `MutableList.Bucket<'A>`, mirroring upstream),
/// keeping the generic `Bucket` name out of the `Effect` namespace.
[<RequireQualifiedAccess>]
module MutableList =

    /// Internal storage node. Most code should use the list operations instead.
    type Bucket<'A> =
        { Items: ResizeArray<'A>
          mutable Offset: int
          mutable Next: Bucket<'A> option }

    /// A mutable FIFO list. `Head`/`Tail` are `None` when empty.
    type MutableList<'A> =
        { mutable Head: Bucket<'A> option
          mutable Tail: Bucket<'A> option
          mutable Length: int }

    let private mkBucket (items: ResizeArray<'A>) : Bucket<'A> =
        { Items = items
          Offset = 0
          Next = None }

    // --- constructors ---

    /// Creates an empty mutable list. (MutableList.make)
    let make<'A> () : MutableList<'A> =
        { Head = None; Tail = None; Length = 0 }

    // --- mutations ---

    /// Removes every element, resetting the list to empty. (MutableList.clear)
    let clear (self: MutableList<'A>) : unit =
        self.Head <- None
        self.Tail <- None
        self.Length <- 0

    /// Appends one element to the back. (MutableList.append)
    let append (self: MutableList<'A>) (message: 'A) : unit =
        let tail =
            match self.Tail with
            | Some t -> t
            | None ->
                let b = mkBucket (ResizeArray())
                self.Head <- Some b
                self.Tail <- Some b
                b

        tail.Items.Add message
        self.Length <- self.Length + 1

    /// Prepends one element to the front. (MutableList.prepend)
    let prepend (self: MutableList<'A>) (message: 'A) : unit =
        let b =
            { Items = ResizeArray([ message ])
              Offset = 0
              Next = self.Head }

        self.Head <- Some b
        self.Length <- self.Length + 1

    /// Appends all elements of an array to the back, returning the count added.
    /// (MutableList.appendAllUnsafe)
    let appendAllUnsafe (self: MutableList<'A>) (messages: 'A[]) : int =
        if messages.Length = 0 then
            0
        else
            let chunk = mkBucket (ResizeArray(messages))

            match self.Head with
            | Some _ ->
                (Option.get self.Tail).Next <- Some chunk
                self.Tail <- Some chunk
            | None ->
                self.Head <- Some chunk
                self.Tail <- Some chunk

            self.Length <- self.Length + messages.Length
            messages.Length

    /// Appends all elements of an iterable to the back, returning the count
    /// added. (MutableList.appendAll)
    let appendAll (self: MutableList<'A>) (messages: seq<'A>) : int =
        appendAllUnsafe self (Seq.toArray messages)

    /// Prepends all elements of an array to the front. (MutableList.prependAllUnsafe)
    let prependAllUnsafe (self: MutableList<'A>) (messages: 'A[]) : unit =
        if messages.Length <> 0 then
            let chunk =
                { Items = ResizeArray(messages)
                  Offset = 0
                  Next = self.Head }

            self.Head <- Some chunk
            self.Length <- self.Length + messages.Length

    /// Prepends all elements of an iterable to the front. (MutableList.prependAll)
    let prependAll (self: MutableList<'A>) (messages: seq<'A>) : unit =
        prependAllUnsafe self (Seq.toArray messages)

    // --- elements ---

    /// Takes up to `n` elements from the front, removing them. (MutableList.takeN)
    let takeN (self: MutableList<'A>) (n: int) : 'A[] =
        if n <= 0 || self.Head.IsNone then
            [||]
        else
            let n = min n self.Length
            let out = Array.zeroCreate<'A> n
            let mutable index = 0
            let mutable chunk = self.Head
            let mutable returned = false

            while not returned && chunk.IsSome do
                let c = chunk.Value

                while not returned && c.Offset < c.Items.Count do
                    out.[index] <- c.Items.[c.Offset]
                    index <- index + 1
                    c.Offset <- c.Offset + 1

                    if index = n then
                        self.Head <- Some c
                        self.Length <- self.Length - n

                        if self.Length = 0 then
                            clear self

                        returned <- true

                if not returned then
                    chunk <- c.Next

            if not returned then
                clear self

            out

    /// Removes up to `n` elements from the front without returning them.
    /// (MutableList.takeNVoid)
    let takeNVoid (self: MutableList<'A>) (n: int) : unit = takeN self n |> ignore

    /// Takes all elements from the front, emptying the list. (MutableList.takeAll)
    let takeAll (self: MutableList<'A>) : 'A[] = takeN self self.Length

    /// Takes a single element from the front, or `None` if empty. (MutableList.take)
    let take (self: MutableList<'A>) : 'A option =
        match self.Head with
        | None -> None
        | Some h ->
            let message = h.Items.[h.Offset]
            h.Offset <- h.Offset + 1
            self.Length <- self.Length - 1

            if h.Offset = h.Items.Count then
                match h.Next with
                | Some next -> self.Head <- Some next
                | None -> clear self

            Some message

    /// Copies up to `n` front elements into a new array without mutating.
    /// (MutableList.toArrayN)
    let toArrayN (self: MutableList<'A>) (n: int) : 'A[] =
        let length = min n self.Length
        let out = Array.zeroCreate<'A> length
        let mutable index = 0
        let mutable bucket = self.Head

        while bucket.IsSome && index < length do
            let b = bucket.Value
            let mutable i = b.Offset

            while i < b.Items.Count && index < length do
                out.[index] <- b.Items.[i]
                index <- index + 1
                i <- i + 1

            bucket <- b.Next

        out

    /// Copies all current elements into a new array without mutating.
    /// (MutableList.toArray)
    let toArray (self: MutableList<'A>) : 'A[] = toArrayN self self.Length

    /// Keeps only elements satisfying `f value index` (index is bucket-local, as
    /// upstream), rebuilding the list in place. (MutableList.filter)
    let filter (self: MutableList<'A>) (f: 'A -> int -> bool) : unit =
        let array = ResizeArray<'A>()
        let mutable chunk = self.Head

        while chunk.IsSome do
            let c = chunk.Value

            for i in c.Offset .. c.Items.Count - 1 do
                if f c.Items.[i] i then
                    array.Add c.Items.[i]

            chunk <- c.Next

        let b = mkBucket array
        self.Head <- Some b
        self.Tail <- Some b
        self.Length <- array.Count

    /// Removes all elements structurally equal to `value`, in place.
    /// (MutableList.remove)
    let remove (self: MutableList<'A>) (value: 'A) : unit = filter self (fun v _ -> v <> value)
