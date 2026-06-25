namespace Effect

/// Trait: `Hash` — small numeric fingerprints used to bucket values.
///
/// Port of repos/effect-smol/packages/effect/src/Hash.ts. Ports the pure,
/// deterministic numeric surface — `optimize`, `combine`, `number`, `string`,
/// `array` — plus a generic `hash` dispatcher. The arithmetic mirrors the
/// upstream 32-bit semantics: JavaScript's bitwise operators coerce to signed
/// 32-bit integers, which F#'s `int` arithmetic (wrapping mod 2^32) reproduces.
///
/// Porting notes (see CONVENTIONS.md):
///   * The `Hash` symbol-method interface, `isHash`, `random` (reference hashing
///     via `Math.random`), the `WeakMap` result cache, circular-reference
///     tracking, and `byReference` marking are JS-runtime machinery and are
///     dropped. For composite values without a primitive case, `hash` reuses
///     F#'s built-in structural hash (`LanguagePrimitives.GenericHash`), kept in
///     range with `optimize`.
///   * Effect's `Map`/`Set` ordering-independent hashing is covered by the
///     generic structural fallback rather than the bespoke XOR folds.
[<RequireQualifiedAccess>]
module Hash =

    /// Upstream symbol id (kept for parity).
    [<Literal>]
    let symbol = "~effect/interfaces/Hash"

    /// Improves the bit distribution of a raw hash value.
    let optimize (n: int) : int =
        (n &&& 0xbfffffff) ||| (int (uint32 n >>> 1) &&& 0x40000000)

    /// Combines two hash values: `(self * 53) ^ b`.
    let combine (b: int) (self: int) : int = (self * 53) ^^^ b

    /// djb2-style string hash.
    let string (str: string) : int =
        let mutable h = 5381
        let mutable i = str.Length

        while i > 0 do
            i <- i - 1
            h <- (h * 33) ^^^ int str.[i]

        optimize h

    // JS `ToInt32`: truncate toward zero, then take the value mod 2^32 as a
    // signed 32-bit integer. Used by `number` to mirror `n | 0` and `n ^ m`.
    let private toInt32 (x: float) : int =
        if System.Double.IsNaN x || System.Double.IsInfinity x then
            0
        else
            let t = System.Math.Truncate x
            let m = t % 4294967296.0
            let m = if m < 0.0 then m + 4294967296.0 else m
            let m = if m >= 2147483648.0 then m - 4294967296.0 else m
            int m

    /// Numeric hash, with distinct values for `NaN`, `Infinity`, `-Infinity`.
    let number (n: float) : int =
        if System.Double.IsNaN n then
            string "NaN"
        elif n = System.Double.PositiveInfinity then
            string "Infinity"
        elif n = System.Double.NegativeInfinity then
            string "-Infinity"
        else
            let mutable h = toInt32 n

            if float h <> n then
                h <- h ^^^ toInt32 (n * 4294967295.0)

            let mutable nn = n

            while nn > 4294967295.0 do
                nn <- nn / 4294967295.0
                h <- h ^^^ toInt32 nn

            optimize h

    /// Hashes an iterable by XOR-folding element hashes from the seed `6151`.
    /// Because the fold uses XOR, reordered inputs can collide.
    let array (xs: seq<int>) : int =
        let mutable h = 6151

        for el in xs do
            h <- h ^^^ el

        optimize h

    /// Generic hash dispatcher. Primitives use the numeric/string hashes above;
    /// every other value falls back to F#'s built-in structural hash, kept in
    /// range with `optimize`.
    let hash (self: obj) : int =
        match self with
        | null -> string "null"
        | :? float as f -> number f
        | :? int as i -> number (float i)
        | :? int64 as i -> string (i.ToString())
        | :? bigint as i -> string (i.ToString())
        | :? string as s -> string s
        | :? bool as b -> string (if b then "true" else "false")
        | :? System.DateTime as d -> string (d.ToString("o"))
        | other -> optimize (LanguagePrimitives.GenericHash other)
