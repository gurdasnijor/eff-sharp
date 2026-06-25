namespace Effect

/// Interfaces for describing changes to a value as patches.
///
/// Port of repos/effect-smol/packages/effect/src/Differ.ts. A `Differ<'T,'Patch>`
/// provides an empty patch, computes the patch between two values, combines
/// patches in order, and applies a patch to a value to produce an updated value.
///
/// Porting notes (see CONVENTIONS.md):
///   * This smol version of `Differ.ts` exposes only the model interface (no
///     constructors/combinators), so the port is the record type plus a `make`
///     smart constructor for ergonomics. No upstream test file exists.
///   * The TS `interface` is modelled as an F# record of (curried) functions.
type Differ<'T, 'Patch> =
    {
        /// The empty patch (applying it is a no-op).
        empty: 'Patch
        /// Computes the patch turning `oldValue` into `newValue`.
        diff: 'T -> 'T -> 'Patch
        /// Combines two patches in order (`first` then `second`).
        combine: 'Patch -> 'Patch -> 'Patch
        /// Applies a patch to `oldValue`, producing the updated value.
        patch: 'T -> 'Patch -> 'T
    }

[<RequireQualifiedAccess>]
module Differ =

    /// Creates a `Differ` from its component functions.
    let make
        (empty: 'Patch)
        (diff: 'T -> 'T -> 'Patch)
        (combine: 'Patch -> 'Patch -> 'Patch)
        (patch: 'T -> 'Patch -> 'T)
        : Differ<'T, 'Patch> =
        { empty = empty
          diff = diff
          combine = combine
          patch = patch }
