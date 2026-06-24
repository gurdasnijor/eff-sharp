namespace Effect

/// Port of repos/effect-smol/packages/effect/src/JsonPointer.ts.
///
/// RFC 6901 reference-token escaping. JSON Pointer uses `/` to separate path
/// tokens, so a token's literal `~` and `/` must be encoded. F#'s
/// `String.Replace` already replaces every occurrence, so these are direct
/// one-line ports (no regex needed).
[<RequireQualifiedAccess>]
module JsonPointer =

    /// Escape a single reference token: `~` -> `~0`, then `/` -> `~1`.
    /// Order matters: `~` is replaced first so the `~1` produced for `/` is not
    /// re-escaped. (JsonPointer.escapeToken)
    let escapeToken (token: string) : string =
        token.Replace("~", "~0").Replace("/", "~1")

    /// Decode a single reference token: `~1` -> `/`, then `~0` -> `~`.
    /// Order matters: `~1` is decoded before `~0` so `~01` decodes to `~1`, not
    /// `/`. (JsonPointer.unescapeToken)
    let unescapeToken (token: string) : string =
        token.Replace("~1", "/").Replace("~0", "~")
