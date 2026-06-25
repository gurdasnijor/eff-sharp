namespace Effect

open Fable.Core

/// Port of repos/effect-smol/packages/effect/src/PrimaryKey.ts.
///
/// `PrimaryKey` is a protocol for values that expose a stable, string-based
/// identifier (for equality, hashing, caching, persistence). Upstream the
/// protocol is a single symbol-keyed method; in F# it lowers onto an interface
/// and the `hasProperty` guard becomes a native type test (CONVENTIONS §2, §3).
///
/// The interface itself is `Effect.IPrimaryKey`, already introduced by
/// `HashRing.fs` (whose doc comment notes it "replaces upstream PrimaryKey").
/// This module binds the real `PrimaryKey` surface (`isPrimaryKey` / `value`) to
/// that same interface rather than defining a second one, unifying the protocol.
/// The member is a `PrimaryKey: string` property — a cleaner F# lowering of
/// upstream's zero-arg method. `symbol` is kept as a `[<Literal>]` for parity.
/// Unknown-value detection is structural under Fable because interfaces do not
/// exist at runtime in emitted JavaScript.
[<RequireQualifiedAccess>]
module PrimaryKey =

    /// Upstream protocol key, kept for parity. (`symbol`)
    [<Literal>]
    let symbol = "~effect/interfaces/PrimaryKey"

    [<Emit("!!($0 && typeof $0.PrimaryKey === 'string')")>]
    let private hasPrimaryKey (_u: obj) : bool = nativeOnly

    /// True when `u` implements the `PrimaryKey` protocol. Lowers the upstream
    /// structural `hasProperty(u, symbol)` guard onto a runtime protocol check.
    /// (`isPrimaryKey`)
    let isPrimaryKey (u: obj) : bool =
#if FABLE_COMPILER
        hasPrimaryKey u
#else
        match u with
        | :? IPrimaryKey -> true
        | _ -> false
#endif

    /// Extract the stable string identifier from a `PrimaryKey`. (`value`)
    let value (self: IPrimaryKey) : string = self.PrimaryKey
