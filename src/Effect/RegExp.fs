namespace Effect

open System.Text.RegularExpressions

/// Port of repos/effect-smol/packages/effect/src/RegExp.ts.
///
/// Upstream re-exports the JS `RegExp` constructor, a guard, and an `escape`
/// helper from the Effect namespace. In F# the native regular-expression type is
/// `System.Text.RegularExpressions.Regex`, so the port re-exposes it and lowers
/// the `isRegExp` guard onto a native type test (CONVENTIONS §3, §5).
///
/// Divergence: JS `RegExp.prototype.source` for `/a/` is `"a"`; .NET
/// `Regex.ToString()` likewise returns the original pattern, so the `escape`
/// character class matches upstream's exactly.
[<RequireQualifiedAccess>]
module RegExp =

    /// Construct a regular expression. (`new RegExp.RegExp(pattern)`)
    let make (pattern: string) : Regex = Regex(pattern)

    /// Construct a regular expression with options. (`new RegExp.RegExp(pattern, flags)`)
    let makeWith (options: RegexOptions) (pattern: string) : Regex = Regex(pattern, options)

    /// True when `input` is a regular expression. (`isRegExp`)
    let isRegExp (input: obj) : bool =
        match input with
        | :? Regex -> true
        | _ -> false

    /// Escape special characters so `string` is matched literally inside a
    /// pattern. Mirrors upstream's `/[/\\^$*+?.()|[\]{}]/g` replacement.
    /// (`escape`)
    let escape (string: string) : string =
        Regex.Replace(string, @"[/\\^$*+?.()|[\]{}]", @"\$&")
