namespace Effect

open Fable.Core

/// Port of repos/effect-smol/packages/effect/src/Redactable.ts.
///
/// `Redactable` is a protocol for values that present an alternative,
/// sanitized representation of themselves (e.g. masking secrets in logs).
/// Upstream the protocol is a single symbol-keyed method; in F# it lowers
/// directly onto an interface, `IRedactable`. The unknown-value guard remains
/// structural under Fable because interfaces do not exist at runtime in emitted
/// JavaScript (CONVENTIONS §2, §3).
///
/// Omissions / divergences (noted per CONVENTIONS §3, §4):
///   - The redaction method upstream receives the current fiber's
///     `Context.Context<never>` (empty if no fiber is active). To keep this leaf
///     decoupled from the runtime/Context machinery it takes no argument here;
///     implementations that need context can capture it themselves. The empty-
///     context plumbing (`emptyContext`, `currentFiberTypeId`) is dropped.
///   - `symbolRedactable` is kept as a `[<Literal>]` brand for parity only.
type IRedactable =
    /// Return the redacted representation of this value.
    abstract member Redact: unit -> obj

[<RequireQualifiedAccess>]
module Redactable =

    /// Upstream symbol brand, kept for parity. (`symbolRedactable`)
    [<Literal>]
    let symbolRedactable = "~effect/Redactable"

    [<Emit("!!($0 && typeof $0.Redact === 'function')")>]
    let private hasRedact (_u: obj) : bool = nativeOnly

    [<Emit("$0.Redact()")>]
    let private unsafeRedact (_u: obj) : obj = nativeOnly

    /// True when `u` implements the redaction protocol. Lowers the upstream
    /// `hasProperty(u, symbolRedactable)` guard onto a runtime protocol check.
    /// (`isRedactable`)
    let isRedactable (u: obj) : bool =
#if FABLE_COMPILER
        hasRedact u
#else
        match u with
        | :? IRedactable -> true
        | _ -> false
#endif

    /// Read the redacted representation from a known `IRedactable`.
    /// (`getRedacted`)
    let getRedacted (redactable: IRedactable) : obj = redactable.Redact()

    /// Redact `u` if it implements `IRedactable`, otherwise return it unchanged.
    /// Redaction is not recursive. (`redact`)
    let redact (u: obj) : obj =
#if FABLE_COMPILER
        if hasRedact u then unsafeRedact u else u
#else
        match u with
        | :? IRedactable as r -> r.Redact()
        | _ -> u
#endif
