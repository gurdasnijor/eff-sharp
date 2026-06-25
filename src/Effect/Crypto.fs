namespace Effect

/// `Crypto` — platform-agnostic cryptographic operations as a `Context` service.
///
/// Port of repos/effect-smol/packages/effect/src/Crypto.ts. The service is built
/// from two primitives — `randomBytes` (secure bytes) and `digest` (a message
/// digest) — and `make` derives secure random numbers/booleans/ranges/shuffles
/// and UUIDv4/UUIDv7 from them. UUIDv7 reads the wall clock through the `Clock`
/// service for its timestamp.
///
/// Decoupling / omissions (per CONVENTIONS):
///   * All accessor effects unify on error `PlatformError` (the ones that never
///     fail simply never produce it) and environment `Context` (so UUIDv7 can
///     reach `Clock`). Upstream's never-failing helpers are wider; this is a
///     faithful narrowing for a single record of heterogeneous effects.
///   * `validateSize` only checks `size >= 0`: in F# `size: int` is already a
///     safe integer, so the `Number.isSafeInteger` half of the upstream guard is
///     unnecessary.
///   * `DigestAlgorithm` is a DU with a `.Name` (the upstream string).
///   * The `TypeId` brand is dropped (the F# type is the guard).
/// Digest algorithms supported by the `Crypto` service. (`DigestAlgorithm`)
type DigestAlgorithm =
    | SHA1
    | SHA256
    | SHA384
    | SHA512

    /// The upstream string name (e.g. `"SHA-256"`).
    member this.Name =
        match this with
        | SHA1 -> "SHA-1"
        | SHA256 -> "SHA-256"
        | SHA384 -> "SHA-384"
        | SHA512 -> "SHA-512"

/// Platform-agnostic cryptographic operations. (`Crypto`)
type Crypto =
    { NextIntUnsafe: unit -> float
      NextDoubleUnsafe: unit -> float
      RandomBytes: int -> Effect<byte[], PlatformError, Context>
      Digest: DigestAlgorithm -> byte[] -> Effect<byte[], PlatformError, Context>
      Random: Effect<float, PlatformError, Context>
      RandomBoolean: Effect<bool, PlatformError, Context>
      RandomInt: Effect<float, PlatformError, Context>
      RandomBetween: float -> float -> Effect<float, PlatformError, Context>
      RandomIntBetween: float -> float -> bool -> Effect<float, PlatformError, Context>
      RandomShuffle: obj -> Effect<obj, PlatformError, Context>
      RandomUUIDv4: Effect<string, PlatformError, Context>
      RandomUUIDv7: Effect<string, PlatformError, Context> }

[<RequireQualifiedAccess>]
module Crypto =

    /// The `Tag` under which the `Crypto` service is stored. (Crypto.Crypto)
    let tag: Tag<Crypto> = Tag.make<Crypto> "effect/Crypto"

    /// Read the active `Crypto` service. (Crypto.Crypto)
    let service<'E> : Effect<Crypto, 'E, Context> = Effect.service tag

    let private maxSafe = 9_007_199_254_740_991.0
    let private minSafe = -9_007_199_254_740_991.0
    let private maxUUIDv7Timestamp = (1L <<< 48) - 1L

    let private validateSize (method: string) (size: int) : Effect<int, PlatformError, Context> =
        if size >= 0 then
            Effect.succeed size
        else
            Effect.fail (
                PlatformError.badArgument
                    { Module = "Crypto"
                      Method = method
                      Description = Some "size must be a non-negative safe integer"
                      Cause = None }
            )

    let private hex (b: byte) : string = b.ToString("x2")

    let private formatUUID (bytes: byte[]) : string =
        let seg (start: int) (stop: int) =
            bytes.[start .. stop - 1] |> Array.map hex |> String.concat ""

        [ seg 0 4; seg 4 6; seg 6 8; seg 8 10; seg 10 16 ] |> String.concat "-"

    let private formatUUIDv4 (bytes: byte[]) : string =
        bytes.[6] <- (bytes.[6] &&& 0x0fuy) ||| 0x40uy
        bytes.[8] <- (bytes.[8] &&& 0x3fuy) ||| 0x80uy
        formatUUID bytes

    let private formatUUIDv7 (timestampMillis: int64) (bytes: byte[]) : string =
        let timestamp = min (max 0L timestampMillis) maxUUIDv7Timestamp
        bytes.[0] <- byte (timestamp >>> 40)
        bytes.[1] <- byte ((timestamp >>> 32) &&& 0xffL)
        bytes.[2] <- byte ((timestamp >>> 24) &&& 0xffL)
        bytes.[3] <- byte ((timestamp >>> 16) &&& 0xffL)
        bytes.[4] <- byte ((timestamp >>> 8) &&& 0xffL)
        bytes.[5] <- byte (timestamp &&& 0xffL)
        bytes.[6] <- (bytes.[6] &&& 0x0fuy) ||| 0x70uy
        bytes.[8] <- (bytes.[8] &&& 0x3fuy) ||| 0x80uy
        formatUUID bytes

    /// Build a `Crypto` service from secure `randomBytes` and `digest`
    /// primitives, deriving the remaining helpers. (Crypto.make)
    let make
        (randomBytesUnsafe: int -> byte[])
        (digest: DigestAlgorithm -> byte[] -> Effect<byte[], PlatformError, Context>)
        : Crypto =
        let nextDoubleUnsafe () =
            let bytes = randomBytesUnsafe 7

            let value =
                float (bytes.[0] &&& 0x1fuy) * 2.0 ** 48.0
                + float bytes.[1] * 2.0 ** 40.0
                + float bytes.[2] * 2.0 ** 32.0
                + float bytes.[3] * 2.0 ** 24.0
                + float bytes.[4] * 2.0 ** 16.0
                + float bytes.[5] * 2.0 ** 8.0
                + float bytes.[6]

            value / 2.0 ** 53.0

        let nextIntUnsafe () =
            floor (nextDoubleUnsafe () * (maxSafe - minSafe + 1.0)) + minSafe

        { NextDoubleUnsafe = nextDoubleUnsafe
          NextIntUnsafe = nextIntUnsafe
          RandomBytes = fun size -> validateSize "randomBytes" size |> Effect.map randomBytesUnsafe
          Digest = digest
          Random = Effect.sync nextDoubleUnsafe
          RandomBoolean = Effect.sync (fun () -> nextDoubleUnsafe () > 0.5)
          RandomInt = Effect.sync nextIntUnsafe
          RandomBetween = fun min max -> Effect.sync (fun () -> nextDoubleUnsafe () * (max - min) + min)
          RandomIntBetween =
            fun min max halfOpen ->
                let extra = if halfOpen then 0.0 else 1.0

                Effect.sync (fun () ->
                    let minInt = ceil min
                    let maxInt = floor max
                    floor (nextDoubleUnsafe () * (maxInt - minInt + extra)) + minInt)
          RandomShuffle =
            fun (elements: obj) ->
                Effect.sync (fun () ->
                    let buffer =
                        (elements :?> System.Collections.IEnumerable) |> Seq.cast<obj> |> Seq.toArray

                    for i in buffer.Length - 1 .. -1 .. 1 do
                        let index = min i (int (floor (nextDoubleUnsafe () * float (i + 1))))
                        let value = buffer.[i]
                        buffer.[i] <- buffer.[index]
                        buffer.[index] <- value

                    box buffer)
          RandomUUIDv4 = Effect.sync (fun () -> formatUUIDv4 (randomBytesUnsafe 16))
          RandomUUIDv7 =
            Clock.currentTimeMillis<PlatformError>
            |> Effect.map (fun millis -> formatUUIDv7 millis (randomBytesUnsafe 16)) }
