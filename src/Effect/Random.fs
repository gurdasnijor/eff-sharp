namespace Effect

open System.Text

/// `Random` — pseudo-random generation as a `Context` service.
///
/// Port of repos/effect-smol/packages/effect/src/Random.ts. The active `Random`
/// service exposes two unsafe primitives — `NextIntUnsafe` (a safe integer) and
/// `NextDoubleUnsafe` (a `[0,1)` double) — and the module derives booleans,
/// bounded numbers, shuffles, choice and seeded runs from them. Because it is a
/// service, tests can swap in a deterministic or mock generator.
///
/// Decoupling / omissions (per CONVENTIONS):
///   * The active service is a `Context.Reference` with `defaultRandom` as its
///     default, matching Effect v4's replacement for FiberRef-backed services.
///   * Upstream ships a bit-exact ISAAC CSPRNG for `withSeed`. The tests only
///     assert *determinism* (same seed → same sequence) and *distinctness*
///     (different seeds → different sequences), never exact ISAAC outputs (those
///     come from mock services). We therefore seed `System.Random`, which Fable
///     maps to a JS-compatible implementation, hashing string seeds with FNV-1a
///     over their UTF-8 bytes so distinct strings yield distinct seeds. Noted as
///     a deliberate substitution.
///   * `number` is `float` throughout (JS numbers); ints are integer-valued
///     floats, matching upstream.
///   * `Cause.NoSuchElementError` is not in the ported `Cause`; a local
///     `NoSuchElementError` exception stands in for `choice`'s empty failure.
/// The random-number service. Mirrors upstream `{ nextIntUnsafe, nextDoubleUnsafe }`.
type Random =
    { NextIntUnsafe: unit -> float
      NextDoubleUnsafe: unit -> float }

/// Raised by `Random.choice` on an empty collection. (`Cause.NoSuchElementError`)
type NoSuchElementError(message: string) =
    inherit System.Exception(message)

/// A deterministic seed for `Random.withSeed` (`string | number` upstream).
type Seed =
    | SeedString of string
    | SeedInt of int

[<RequireQualifiedAccess>]
module Random =

    // JS `Number.MAX_SAFE_INTEGER` / `MIN_SAFE_INTEGER`.
    let private maxSafe = 9_007_199_254_740_991.0
    let private minSafe = -9_007_199_254_740_991.0

    /// Build a `Random` from a `[0,1)` double source, deriving `NextIntUnsafe`
    /// exactly as upstream (`floor(d*(MAX-MIN+1)) + MIN`).
    let ofDouble (nextDouble: unit -> float) : Random =
        { NextDoubleUnsafe = nextDouble
          NextIntUnsafe = fun () -> floor (nextDouble () * (maxSafe - minSafe + 1.0)) + minSafe }

    /// The default, non-deterministic generator.
    let private sharedRandom = System.Random()
    let defaultRandom: Random = ofDouble (fun () -> sharedRandom.NextDouble())

    /// The defaulted `Context.Reference` under which the active random service is stored.
    let reference: Reference<Random> = Reference.make "effect/Random" defaultRandom

    /// Compatibility tag for Layer/Context APIs. Values provided here override `reference`.
    let tag: Tag<Random> = Reference.toTag reference

    /// Stable 32-bit FNV-1a hash of a string's UTF-8 bytes, used to derive a
    /// numeric seed from a string seed.
    let private fnv1a (s: string) : int =
        let mutable h = 2166136261u

        for b in Encoding.UTF8.GetBytes s do
            h <- (h ^^^ uint32 b) * 16777619u

        int h

    /// A deterministic generator for `seed` (same seed → same sequence).
    let seeded (seed: Seed) : Random =
        let intSeed =
            match seed with
            | SeedInt i -> i
            | SeedString s -> fnv1a s

        let rng = System.Random(intSeed)
        ofDouble (fun () -> rng.NextDouble())

    /// Run `f` against the active `Random` service (falling back to the default
    /// when none is provided). (Random.randomWith)
    let randomWith (f: Random -> 'A) : Effect<'A, 'E, Context> =
        Effect.serviceReference reference |> Effect.map f

    /// A random double in `[0, 1)`. (Random.next)
    let next<'E> : Effect<float, 'E, Context> =
        randomWith (fun r -> r.NextDoubleUnsafe())

    /// A random boolean (`nextDouble > 0.5`). (Random.nextBoolean)
    let nextBoolean<'E> : Effect<bool, 'E, Context> =
        randomWith (fun r -> r.NextDoubleUnsafe() > 0.5)

    /// A random safe integer. (Random.nextInt)
    let nextInt<'E> : Effect<float, 'E, Context> =
        randomWith (fun r -> r.NextIntUnsafe())

    /// A random double in `[min, max)`. (Random.nextBetween)
    let nextBetween (min: float) (max: float) : Effect<float, 'E, Context> =
        randomWith (fun r -> r.NextDoubleUnsafe() * (max - min) + min)

    /// A random integer in `[ceil min, floor max]` (or half-open). (Random.nextIntBetween)
    let nextIntBetween (min: float) (max: float) (halfOpen: bool) : Effect<float, 'E, Context> =
        let extra = if halfOpen then 0.0 else 1.0

        randomWith (fun r ->
            let minInt = ceil min
            let maxInt = floor max
            floor (r.NextDoubleUnsafe() * (maxInt - minInt + extra)) + minInt)

    /// A shuffled copy of `elements` (Fisher–Yates). (Random.shuffle)
    let shuffle (elements: seq<'A>) : Effect<'A[], 'E, Context> =
        randomWith (fun r ->
            let buffer = Seq.toArray elements

            for i in buffer.Length - 1 .. -1 .. 1 do
                let index = min i (int (floor (r.NextDoubleUnsafe() * float (i + 1))))
                let value = buffer.[i]
                buffer.[i] <- buffer.[index]
                buffer.[index] <- value

            buffer)

    /// A uniformly random element, failing on an empty collection. (Random.choice)
    let choice (elements: seq<'A>) : Effect<'A, NoSuchElementError, Context> =
        let buffer = Seq.toArray elements

        if buffer.Length = 0 then
            Effect.fail (NoSuchElementError "Cannot select a random element from an empty array")
        else
            randomWith (fun r ->
                buffer.[min (buffer.Length - 1) (int (floor (r.NextDoubleUnsafe() * float buffer.Length)))])

    /// Run `self` with a deterministic generator for `seed`. (Random.withSeed)
    let withSeed (seed: Seed) (self: Effect<'A, 'E, Context>) : Effect<'A, 'E, 'R> =
        Effect.provideServiceReference reference (seeded seed) self
