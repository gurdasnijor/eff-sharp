namespace Effect

/// Port of repos/effect-smol/packages/effect/src/Redacted.ts.
///
/// `Redacted<'A>` wraps a sensitive value so that its string, JSON, and
/// inspection output is a redacted placeholder, while trusted code can still
/// recover the original with `Redacted.value`.
///
/// Decoupling (per brief): equality/hashing use **F#'s built-in structural
/// equality** on the wrapped value rather than the `Equal`/`Hash` modules. The
/// type overrides `Equals`/`GetHashCode` so the native `=` operator and hashing
/// compare by the protected value without exposing it at the call site.
///
/// Omissions vs upstream (noted per CONVENTIONS §3):
///   - `isRedacted (u: unknown)` — a runtime guard over arbitrary values; F#'s
///     static types make it unnecessary, so it is dropped.
///   - The upstream value is held in an external `WeakMap` registry to keep it
///     off the instance; here it is a private mutable field, which is equally
///     hidden from F# inspection and supports `wipeUnsafe`.

open System

/// A wrapper for a sensitive value whose textual output is redacted.
[<Sealed>]
type Redacted<'A> internal (value: 'A, label: string option) =
    let mutable stored = Some value

    /// The optional label shown in the redacted placeholder.
    member _.Label : string option = label

    member internal _.Stored = stored

    /// Remove the stored value; returns true if a value was present.
    member internal _.Wipe() : bool =
        match stored with
        | Some _ ->
            stored <- None
            true
        | None -> false

    /// `<redacted>` or `<redacted:label>`.
    override _.ToString() : string =
        match label with
        | Some l -> sprintf "<redacted:%s>" l
        | None -> "<redacted>"

    /// Structural equality on the protected value (never exposes it).
    override this.Equals(o: obj) : bool =
        match o with
        | :? Redacted<'A> as other ->
            match this.Stored, other.Stored with
            | Some a, Some b -> Unchecked.equals a b
            | None, None -> true
            | _ -> false
        | _ -> false

    /// Hash derived from the protected value.
    override _.GetHashCode() : int =
        match stored with
        | Some a -> Unchecked.hash a
        | None -> 0

[<RequireQualifiedAccess>]
module Redacted =

    /// Upstream brand constant (kept for parity).
    [<Literal>]
    let TypeId = "~effect/data/Redacted"

    /// Wrap a sensitive value. (Redacted.make)
    let make (value: 'A) : Redacted<'A> = Redacted(value, None)

    /// Wrap a sensitive value with a label shown in redacted output.
    /// (Redacted.make with `{ label }`)
    let makeWith (label: string) (value: 'A) : Redacted<'A> = Redacted(value, Some label)

    /// Recover the original value. Throws if the wrapper has been wiped.
    /// (Redacted.value)
    let value (self: Redacted<'A>) : 'A =
        match self.Stored with
        | Some v -> v
        | None ->
            match self.Label with
            | Some l -> failwithf "Unable to get redacted value with label: \"%s\"" l
            | None -> failwith "Unable to get redacted value"

    /// Delete the stored value, making future `value` calls fail. Returns true
    /// if a value was present to remove. (Redacted.wipeUnsafe)
    let wipeUnsafe (self: Redacted<'A>) : bool = self.Wipe()

    /// JSON serialization is the redacted placeholder. (Redacted toJSON)
    let toJSON (self: Redacted<'A>) : string = self.ToString()

    /// An equivalence on `Redacted<'A>` derived from one on `'A`, comparing the
    /// protected values without exposing them at the call site.
    /// (Redacted.makeEquivalence)
    let makeEquivalence (isEquivalent: 'A -> 'A -> bool) : Redacted<'A> -> Redacted<'A> -> bool =
        fun x y -> isEquivalent (value x) (value y)
