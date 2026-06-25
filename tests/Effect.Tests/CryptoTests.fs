module Effect.Tests.CryptoTests

open System.Threading.Tasks
open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/Crypto.test.ts. The mock
// `testCrypto` reproduces upstream's deterministic randomBytes/digest so derived
// values are exact. UUIDv7 reads a fixed test `Clock`.

let private testCrypto: Crypto =
    Crypto.make
        (fun size ->
            if size = 7 then
                [| 0x18uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy |]
            else
                Array.init size byte)
        (fun algorithm data -> Effect.succeed [| byte data.Length; byte algorithm.Name.Length |])

/// A `Clock` whose wall-clock read returns a fixed value.
let private fixedClock (millis: int64) : Clock =
    { CurrentTimeMillisUnsafe = fun () -> millis
      SleepUnsafe = fun _ -> Task.CompletedTask }

let private ctx = Context.make Crypto.tag testCrypto

let private run (context: Context) (eff: Effect<'A, PlatformError, Context>) : 'A =
    match Effect.runSync context eff with
    | Success a -> a
    | Failure c -> failwithf "unexpected failure: %s" (Cause.render c)

[<Fact>]
let ``supports string literal digest algorithms`` () = Assert.Equal("SHA-256", SHA256.Name)

[<Fact>]
let ``randomBytes delegates to the service`` () =
    let bytes =
        run
            ctx
            (effect {
                let! c = Crypto.service
                return! c.RandomBytes 4
            })

    Assert.Equal<byte[]>([| 0uy; 1uy; 2uy; 3uy |], bytes)

[<Fact>]
let ``random generators delegate to the service`` () =
    let result =
        run
            ctx
            (effect {
                let! c = Crypto.service
                let! random = c.Random
                let! randomInt = c.RandomInt
                let! randomBoolean = c.RandomBoolean
                let! randomBetween = c.RandomBetween 10.0 20.0
                let! randomBetweenDecimal = c.RandomBetween 10.5 20.5
                let! randomIntBetween = c.RandomIntBetween 1.0 6.0 false
                let! shuffled = c.RandomShuffle(box [ 1; 2; 3 ])
                return random, randomInt, randomBoolean, randomBetween, randomBetweenDecimal, randomIntBetween, shuffled
            })

    let random, randomInt, randomBoolean, randomBetween, randomBetweenDecimal, randomIntBetween, shuffled =
        result

    Assert.Equal(0.75, random)
    Assert.Equal(4503599627370497.0, randomInt)
    Assert.True(randomBoolean)
    Assert.Equal(17.5, randomBetween)
    Assert.Equal(18.0, randomBetweenDecimal)
    Assert.Equal(5.0, randomIntBetween)
    Assert.Equal<int list>([ 1; 2; 3 ], (shuffled :?> obj[]) |> Array.map unbox<int> |> List.ofArray)

[<Fact>]
let ``randomIntBetween excludes the upper bound in half-open ranges`` () =
    let allOnes =
        Crypto.make (fun size -> Array.create size 0xffuy) (fun _ data -> Effect.succeed data)

    let value =
        run
            (Context.make Crypto.tag allOnes)
            (effect {
                let! c = Crypto.service
                return! c.RandomIntBetween 1.0 6.0 true
            })

    Assert.Equal(5.0, value)

[<Fact>]
let ``randomUUIDv4 formats UUID bytes from randomBytes`` () =
    let uuid =
        run
            ctx
            (effect {
                let! c = Crypto.service
                return! c.RandomUUIDv4
            })

    Assert.Equal("00010203-0405-4607-8809-0a0b0c0d0e0f", uuid)

[<Fact>]
let ``randomUUIDv7 formats UUID bytes with the Clock timestamp`` () =
    let context = ctx |> Context.add Clock.tag (fixedClock 0x0123456789abL)

    let uuid =
        run
            context
            (effect {
                let! c = Crypto.service
                return! c.RandomUUIDv7
            })

    Assert.Equal("01234567-89ab-7607-8809-0a0b0c0d0e0f", uuid)

[<Fact>]
let ``digest delegates to the service`` () =
    let digest =
        run
            ctx
            (effect {
                let! c = Crypto.service
                return! c.Digest SHA256 [| 1uy; 2uy; 3uy |]
            })

    Assert.Equal<byte[]>([| 3uy; 7uy |], digest)

[<Fact>]
let ``randomBytes validates a negative size`` () =
    match
        Effect.runSync
            ctx
            (effect {
                let! c = Crypto.service
                return! c.RandomBytes -1
            })
    with
    | Failure cause ->
        match Cause.failures cause with
        | (e: PlatformError) :: _ -> Assert.Equal("PlatformError", e.Tag)
        | [] -> Assert.Fail "expected a typed PlatformError"
    | Success _ -> Assert.Fail "expected failure"
