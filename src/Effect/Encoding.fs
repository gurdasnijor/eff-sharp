namespace Effect

/// Port of repos/effect-smol/packages/effect/src/Encoding.ts.
///
/// Base64 (RFC 4648), URL-safe Base64, and hexadecimal encoding/decoding.
/// Encode functions return a `string`; decode functions return an F# `Result`
/// so invalid input is reported as an `EncodingError` rather than thrown.
///
/// Idiomatic port (CONVENTIONS §5): rather than re-implementing the bit-twiddling
/// codecs and lookup tables, this delegates to .NET's `System.Convert` and
/// `System.Text.Encoding.UTF8`, which provide the same RFC 4648 behaviour. The
/// surrounding validation (length checks, padding placement, the URL-safe
/// alphabet) mirrors upstream so error reporting matches.
///
/// Omissions (noted per CONVENTIONS §3):
///   - `isEncodingError (u: unknown)` — a runtime guard over arbitrary values;
///     dropped as redundant under F#'s static typing.

open System

/// Error returned when an encoding or decoding operation cannot process its
/// input. Records the phase (`Decode`/`Encode`), reporting module, original
/// input, and a human-readable message. (Encoding.EncodingError)
type EncodingError =
    { Kind: string
      Module: string
      Input: obj
      Message: string }

[<RequireQualifiedAccess>]
module Encoding =

    /// Upstream brand constant (kept for parity).
    [<Literal>]
    let EncodingErrorTypeId = "~effect/encoding/EncodingError"

    let private utf8Bytes (s: string) : byte[] = System.Text.Encoding.UTF8.GetBytes s
    let private utf8String (b: byte[]) : string = System.Text.Encoding.UTF8.GetString b

    let private decodeError (module_: string) (input: obj) (message: string) : EncodingError =
        { Kind = "Decode"
          Module = module_
          Input = input
          Message = message }

    let private stripCrlf (s: string) : string = s.Replace("\n", "").Replace("\r", "")

    // ----------------------------------------------------------------------
    // Base64
    // ----------------------------------------------------------------------

    /// Encode bytes as standard padded Base64. (Encoding.encodeBase64)
    let encodeBase64 (bytes: byte[]) : string = Convert.ToBase64String bytes

    /// Encode a UTF-8 string as standard padded Base64.
    let encodeBase64String (s: string) : string = encodeBase64 (utf8Bytes s)

    /// Decode a standard Base64 string into bytes. (Encoding.decodeBase64)
    let decodeBase64 (str: string) : Result<byte[], EncodingError> =
        let stripped = stripCrlf str
        let length = stripped.Length
        if length % 4 <> 0 then
            Error(decodeError "Base64" stripped (sprintf "Length must be a multiple of 4, but is %d" length))
        else
            let index = stripped.IndexOf('=')
            if index <> -1
               && (index < length - 2
                   || (index = length - 2 && stripped.[length - 1] <> '=')) then
                Error(decodeError "Base64" stripped "Found a '=' character, but it is not at the end")
            else
                try
                    Ok(Convert.FromBase64String stripped)
                with _ ->
                    Error(decodeError "Base64" stripped "Invalid input")

    /// Decode a standard Base64 string into UTF-8 text. (Encoding.decodeBase64String)
    let decodeBase64String (str: string) : Result<string, EncodingError> =
        decodeBase64 str |> Result.map utf8String

    // ----------------------------------------------------------------------
    // Base64Url
    // ----------------------------------------------------------------------

    /// Encode bytes as URL-safe, unpadded Base64. (Encoding.encodeBase64Url)
    let encodeBase64Url (bytes: byte[]) : string =
        (Convert.ToBase64String bytes)
            .Replace("=", "")
            .Replace("+", "-")
            .Replace("/", "_")

    /// Encode a UTF-8 string as URL-safe, unpadded Base64.
    let encodeBase64UrlString (s: string) : string = encodeBase64Url (utf8Bytes s)

    let private isBase64Url (s: string) : bool =
        s
        |> Seq.forall (fun c ->
            (c >= 'A' && c <= 'Z')
            || (c >= 'a' && c <= 'z')
            || (c >= '0' && c <= '9')
            || c = '-'
            || c = '_'
            || c = '=')

    /// Decode a URL-safe Base64 string (padded or unpadded) into bytes.
    /// (Encoding.decodeBase64Url)
    let decodeBase64Url (str: string) : Result<byte[], EncodingError> =
        let stripped = stripCrlf str
        let length = stripped.Length
        if length % 4 = 1 then
            Error(decodeError "Base64Url" stripped (sprintf "Length should be a multiple of 4, but is %d" length))
        elif not (isBase64Url stripped) then
            Error(decodeError "Base64Url" stripped "Invalid input")
        else
            let padded =
                match length % 4 with
                | 2 -> stripped + "=="
                | 3 -> stripped + "="
                | _ -> stripped
            let sanitized = padded.Replace("-", "+").Replace("_", "/")
            decodeBase64 sanitized

    /// Decode a URL-safe Base64 string into UTF-8 text. (Encoding.decodeBase64UrlString)
    let decodeBase64UrlString (str: string) : Result<string, EncodingError> =
        decodeBase64Url str |> Result.map utf8String

    // ----------------------------------------------------------------------
    // Hex
    // ----------------------------------------------------------------------

    /// Encode bytes as lowercase hexadecimal. (Encoding.encodeHex)
    let encodeHex (bytes: byte[]) : string =
        (Convert.ToHexString bytes).ToLowerInvariant()

    /// Encode a UTF-8 string as lowercase hexadecimal.
    let encodeHexString (s: string) : string = encodeHex (utf8Bytes s)

    /// Decode a hexadecimal string into bytes. (Encoding.decodeHex)
    let decodeHex (str: string) : Result<byte[], EncodingError> =
        let bytes = utf8Bytes str
        if bytes.Length % 2 <> 0 then
            Error(decodeError "Hex" str (sprintf "Length must be a multiple of 2, but is %d" bytes.Length))
        else
            try
                Ok(Convert.FromHexString str)
            with _ ->
                Error(decodeError "Hex" str "Invalid input")

    /// Decode a hexadecimal string into UTF-8 text. (Encoding.decodeHexString)
    let decodeHexString (str: string) : Result<string, EncodingError> =
        decodeHex str |> Result.map utf8String
