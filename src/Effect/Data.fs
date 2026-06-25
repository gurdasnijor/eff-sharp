namespace Effect

/// Port of repos/effect-smol/packages/effect/src/Data.ts.
///
/// effect's `Data` module exists to give plain JS objects what native F# types
/// already have for free: **structural equality, hashing, comparison, and
/// exhaustive pattern matching**. So the class-based constructors lower directly
/// onto idiomatic F# (CONVENTIONS §1, §2, §5):
///
///   Data.Class<A>          -> an F# record           (structural equality built in)
///   Data.TaggedClass(tag)  -> a single-case DU, or a record with a tag field
///   Data.TaggedEnum        -> a discriminated union
///   $is(tag) / $match      -> native `match ... with` (see `isTag` / native match)
///   Data.Error             -> see `Data.Error`
///   Data.TaggedError(tag)  -> see `Data.TaggedError`; in the value-based Effect
///                             world a tagged error is usually just a DU case in `'E`.
///
/// Skipped per CONVENTIONS §4 (no HKTs): the TypeScript type-level machinery
/// `TaggedEnum.Kind / Args / Value / WithGenerics / Constructor / GenericMatchers`.
/// The `taggedEnum()` Proxy-based constructor factory is also dropped — F# DUs
/// provide constructors directly.

open Microsoft.FSharp.Reflection

[<RequireQualifiedAccess>]
module Data =

    /// Base class for yieldable/ad-hoc errors (Data.Error). The F# analogue of
    /// effect's `Error` is a CLR exception carrying readonly fields; instances
    /// are `System.Exception`s and flow through the platform's error machinery.
    /// (Defined inside the module so it does not shadow `Result`'s `Error` case
    /// in the `Effect` namespace.)
    type Error(message: string) =
        inherit System.Exception(message)
        new() = Error("")

    /// Base class for tagged errors with a `_tag` discriminator
    /// (Data.TaggedError), enabling tag-based recovery analogous to
    /// `Effect.catchTag`.
    type TaggedError(tag: string, message: string) =
        inherit Error(message)
        new(tag: string) = TaggedError(tag, "")
        /// The discriminator tag (effect's `_tag`).
        member _.Tag: string = tag

    /// Structural equality — delegates to F#'s built-in structural equality,
    /// which is precisely what effect's `Data` adds to plain JS objects.
    /// (Equivalent to `Equal.equals` for `Data` values.)
    let equals (a: 'a) (b: 'a) : bool = a = b

    /// The discriminator tag of a value, mirroring effect's `_tag`. For a
    /// discriminated-union value it is the active case name; for any other type
    /// it is the runtime type name.
    let tagOf (value: 'a) : string =
#if FABLE_COMPILER
        // Fable erases generic type info (`typeof<'a>`) and FSharpType reflection,
        // so the precise union-case discriminator isn't available; fall back to the
        // runtime value's constructor name.
        (box value).GetType().Name
#else
        let t = typeof<'a>

        if FSharpType.IsUnion(t, true) then
            let case, _ = FSharpValue.GetUnionFields(value, t, true)
            case.Name
        else
            (box value).GetType().Name
#endif

    /// Type-guard by tag, mirroring `taggedEnum().$is(tag)`. Prefer a native
    /// `match` when destructuring; this is for the boolean check.
    let isTag (tag: string) (value: 'a) : bool = tagOf value = tag
