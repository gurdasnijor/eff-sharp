namespace Effect

/// Port of repos/effect-smol/packages/effect/src/MutableRef.ts — a synchronous
/// mutable reference cell holding one current value.
///
/// Unlike `Ref` (effectful, fiber-safe), a `MutableRef` is plain in-place state:
/// read/write `.Current` directly, or use these helpers for read-modify-write.
///
/// `compareAndSet` uses F#'s built-in structural equality (`=`) rather than the
/// `Equal` module (decoupling rule). Omissions (JS-runtime-only): the
/// `toJSON`/inspection hooks and the `pipe`/dual data-last currying — every
/// function takes `self` first.
type MutableRef<'T> = { mutable Current: 'T }

[<RequireQualifiedAccess>]
module MutableRef =

    /// Upstream brand constant (kept as a literal for parity).
    [<Literal>]
    let TypeId = "~effect/MutableRef"

    // --- constructor ---

    /// Creates a reference holding `value`. (MutableRef.make)
    let make (value: 'T) : MutableRef<'T> = { Current = value }

    // --- general ---

    /// Reads the current value. (MutableRef.get)
    let get (self: MutableRef<'T>) : 'T = self.Current

    /// Sets a new value, returning the same reference. (MutableRef.set)
    let set (self: MutableRef<'T>) (value: 'T) : MutableRef<'T> =
        self.Current <- value
        self

    /// Sets a new value, returning that value. (MutableRef.setAndGet)
    let setAndGet (self: MutableRef<'T>) (value: 'T) : 'T =
        self.Current <- value
        self.Current

    /// Sets a new value, returning the previous one. (MutableRef.getAndSet)
    let getAndSet (self: MutableRef<'T>) (value: 'T) : 'T =
        let previous = self.Current
        self.Current <- value
        previous

    /// Applies `f` to the current value, returning the same reference.
    /// (MutableRef.update)
    let update (self: MutableRef<'T>) (f: 'T -> 'T) : MutableRef<'T> = set self (f self.Current)

    /// Applies `f` to the current value, returning the new value.
    /// (MutableRef.updateAndGet)
    let updateAndGet (self: MutableRef<'T>) (f: 'T -> 'T) : 'T = setAndGet self (f self.Current)

    /// Applies `f` to the current value, returning the previous one.
    /// (MutableRef.getAndUpdate)
    let getAndUpdate (self: MutableRef<'T>) (f: 'T -> 'T) : 'T = getAndSet self (f self.Current)

    /// Sets `newValue` only if the current value structurally equals `oldValue`;
    /// returns whether the swap happened. (MutableRef.compareAndSet)
    let compareAndSet (self: MutableRef<'T>) (oldValue: 'T) (newValue: 'T) : bool =
        if oldValue = self.Current then
            self.Current <- newValue
            true
        else
            false

    // --- numeric ---

    /// Increments by 1, returning the same reference. (MutableRef.increment)
    let increment (self: MutableRef<int>) : MutableRef<int> = update self (fun n -> n + 1)

    /// Decrements by 1, returning the same reference. (MutableRef.decrement)
    let decrement (self: MutableRef<int>) : MutableRef<int> = update self (fun n -> n - 1)

    /// Increments by 1, returning the new value. (MutableRef.incrementAndGet)
    let incrementAndGet (self: MutableRef<int>) : int = updateAndGet self (fun n -> n + 1)

    /// Decrements by 1, returning the new value. (MutableRef.decrementAndGet)
    let decrementAndGet (self: MutableRef<int>) : int = updateAndGet self (fun n -> n - 1)

    /// Increments by 1, returning the previous value. (MutableRef.getAndIncrement)
    let getAndIncrement (self: MutableRef<int>) : int = getAndUpdate self (fun n -> n + 1)

    /// Decrements by 1, returning the previous value. (MutableRef.getAndDecrement)
    let getAndDecrement (self: MutableRef<int>) : int = getAndUpdate self (fun n -> n - 1)

    // --- boolean ---

    /// Flips a boolean reference, returning the same reference. (MutableRef.toggle)
    let toggle (self: MutableRef<bool>) : MutableRef<bool> = update self not
