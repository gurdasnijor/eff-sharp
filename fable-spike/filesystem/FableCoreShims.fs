#if FABLE_COMPILER
namespace Fable.Core

open System

// Probe-only stand-ins for `Fable.Core.EmitAttribute`/`ImportAttribute`, identical
// to the block declared in `src/Effect/Terminal.fs` (which precedes `FileSystem.fs`
// in the real assembly's compile order and supplies these there). This probe does
// NOT compile Terminal.fs, so it re-declares the two attributes here so
// `FileSystem.fs`'s `[<Fable.Core.Emit>]`/`[<Fable.Core.Import>]` resolve. Fable
// resolves emit/import by the attribute's full name, so these local declarations
// are honored, and the real `src/Effect` tree is untouched. (severity: probe-only)
type internal EmitAttribute(macro: string) =
    inherit Attribute()

type internal ImportAttribute(selector: string, path: string) =
    inherit Attribute()
#endif
