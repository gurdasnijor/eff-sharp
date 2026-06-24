namespace Effect

/// Port of repos/effect-smol/packages/effect/src/Ref.ts — fiber-safe mutable
/// state for `Effect` programs.
///
/// A `Ref<'A>` holds one value and exposes reads, writes and atomic
/// transformations as effects. Each operation is a single `Effect.sync` thunk,
/// so the whole read-modify-write happens in one synchronous step (atomic with
/// respect to the cooperative async core).
///
/// Backing (per the w2-ref decoupling note): the cell is a merged
/// `MutableRef` (an atomic-ish `{ mutable Current }`) rather than a new
/// primitive. Operations never fail and need no environment, so they stay
/// generic in `'E`/`'R`.
///
/// Omissions vs upstream (noted per CONVENTIONS):
///   - HKT/variance plumbing (`Ref.Variance`, `Invariant`) — F# has no HKTs.
///   - JS-runtime machinery: `RefProto`, `PipeInspectableProto`, `toJSON`, the
///     `pipe`/`dual` data-last currying. Every function takes `self` first,
///     mirroring `MutableRef`.
///   - `Option` is F#'s native `option`; the `_tag === "Some"` checks lower into
///     `match`.
type Ref<'A> = internal { Cell: MutableRef<'A> }

[<RequireQualifiedAccess>]
module Ref =

    /// Upstream brand constant (kept as a literal for parity).
    [<Literal>]
    let TypeId = "~effect/Ref"

    // --- constructors ---

    /// Creates a `Ref` holding `value`, outside the `Effect` context.
    /// (Ref.makeUnsafe)
    let makeUnsafe (value: 'A) : Ref<'A> = { Cell = MutableRef.make value }

    /// Creates a `Ref` holding `value`, wrapped in an `Effect`. (Ref.make)
    let make (value: 'A) : Effect<Ref<'A>, 'E, 'R> = Effect.sync (fun () -> makeUnsafe value)

    // --- getters ---

    /// Reads the current value, outside the `Effect` context. (Ref.getUnsafe)
    let getUnsafe (self: Ref<'A>) : 'A = self.Cell.Current

    /// Reads the current value. (Ref.get)
    let get (self: Ref<'A>) : Effect<'A, 'E, 'R> = Effect.sync (fun () -> self.Cell.Current)

    // --- setters / mutations ---

    /// Replaces the current value. (Ref.set)
    let set (self: Ref<'A>) (value: 'A) : Effect<unit, 'E, 'R> =
        Effect.sync (fun () -> self.Cell.Current <- value)

    /// Replaces the value, returning the previous one. (Ref.getAndSet)
    let getAndSet (self: Ref<'A>) (value: 'A) : Effect<'A, 'E, 'R> =
        Effect.sync (fun () ->
            let current = self.Cell.Current
            self.Cell.Current <- value
            current)

    /// Updates the value with `f`, returning the previous one. (Ref.getAndUpdate)
    let getAndUpdate (self: Ref<'A>) (f: 'A -> 'A) : Effect<'A, 'E, 'R> =
        Effect.sync (fun () ->
            let current = self.Cell.Current
            self.Cell.Current <- f current
            current)

    /// Conditionally updates the value, returning the previous one. `Some` stores
    /// the new value; `None` leaves it unchanged. (Ref.getAndUpdateSome)
    let getAndUpdateSome (self: Ref<'A>) (pf: 'A -> 'A option) : Effect<'A, 'E, 'R> =
        Effect.sync (fun () ->
            let current = self.Cell.Current

            match pf current with
            | Some next -> self.Cell.Current <- next
            | None -> ()

            current)

    /// Replaces the value, returning the new one. (Ref.setAndGet)
    let setAndGet (self: Ref<'A>) (value: 'A) : Effect<'A, 'E, 'R> =
        Effect.sync (fun () ->
            self.Cell.Current <- value
            value)

    /// Computes `[result, newValue]` from the current value, stores `newValue`
    /// and returns `result`. (Ref.modify)
    let modify (self: Ref<'A>) (f: 'A -> 'B * 'A) : Effect<'B, 'E, 'R> =
        Effect.sync (fun () ->
            let (b, a) = f self.Cell.Current
            self.Cell.Current <- a
            b)

    /// Computes `[result, option]`; `Some` stores the new value, `None` leaves it
    /// unchanged. Always returns `result`. (Ref.modifySome)
    let modifySome (self: Ref<'A>) (pf: 'A -> 'B * 'A option) : Effect<'B, 'E, 'R> =
        modify self (fun value ->
            let (b, option) = pf value

            let next =
                match option with
                | None -> value
                | Some v -> v

            (b, next))

    /// Updates the value with `f`. (Ref.update)
    let update (self: Ref<'A>) (f: 'A -> 'A) : Effect<unit, 'E, 'R> =
        Effect.sync (fun () -> self.Cell.Current <- f self.Cell.Current)

    /// Updates the value with `f`, returning the new value. (Ref.updateAndGet)
    let updateAndGet (self: Ref<'A>) (f: 'A -> 'A) : Effect<'A, 'E, 'R> =
        Effect.sync (fun () ->
            self.Cell.Current <- f self.Cell.Current
            self.Cell.Current)

    /// Conditionally updates the value. `Some` stores the new value; `None`
    /// leaves it unchanged. (Ref.updateSome)
    let updateSome (self: Ref<'A>) (pf: 'A -> 'A option) : Effect<unit, 'E, 'R> =
        Effect.sync (fun () ->
            match pf self.Cell.Current with
            | Some next -> self.Cell.Current <- next
            | None -> ())

    /// Conditionally updates the value, returning the resulting current value.
    /// (Ref.updateSomeAndGet)
    let updateSomeAndGet (self: Ref<'A>) (pf: 'A -> 'A option) : Effect<'A, 'E, 'R> =
        Effect.sync (fun () ->
            (match pf self.Cell.Current with
             | Some next -> self.Cell.Current <- next
             | None -> ())

            self.Cell.Current)
