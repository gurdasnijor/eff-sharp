module CryptoSpec

open Effect
open Effect.Vitest

let private testCrypto: Crypto =
    Crypto.make
        (fun size ->
            if size = 7 then
                [| 0x18uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy |]
            else
                Array.init size byte)
        (fun algorithm data -> Effect.succeed [| byte data.Length; byte algorithm.Name.Length |])

let private fixedClock (millis: int64) : Clock =
    { CurrentTimeMillisUnsafe = fun () -> millis
      SleepUnsafe = fun _ -> async { return () } }

let private ctx = Context.make Crypto.tag testCrypto

let private run (context: Context) (eff: Effect<'A, PlatformError, Context>) : 'A =
    match Effect.runSync context eff with
    | Success a -> a
    | Failure cause -> failwithf "unexpected failure: %s" (Cause.render cause)

describe "Crypto" (fun () ->
    test "supports string literal digest algorithms" (fun () -> toBe SHA256.Name "SHA-256")

    test "randomBytes delegates to the service" (fun () ->
        let bytes =
            run
                ctx
                (effect {
                    let! c = Crypto.service
                    return! c.RandomBytes 4
                })

        toBe (bytes = [| 0uy; 1uy; 2uy; 3uy |]) true)

    test "random generators delegate to the service" (fun () ->
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

        let shuffledList = (shuffled :?> obj[]) |> Array.map unbox<int> |> List.ofArray

        toBe random 0.75
        toBe randomInt 4503599627370497.0
        toBe randomBoolean true
        toBe randomBetween 17.5
        toBe randomBetweenDecimal 18.0
        toBe randomIntBetween 5.0
        toBe (shuffledList = [ 1; 2; 3 ]) true)

    test "randomIntBetween excludes the upper bound in half-open ranges" (fun () ->
        let allOnes =
            Crypto.make (fun size -> Array.create size 0xffuy) (fun _ data -> Effect.succeed data)

        let value =
            run
                (Context.make Crypto.tag allOnes)
                (effect {
                    let! c = Crypto.service
                    return! c.RandomIntBetween 1.0 6.0 true
                })

        toBe value 5.0)

    test "randomUUIDv4 formats UUID bytes from randomBytes" (fun () ->
        let uuid =
            run
                ctx
                (effect {
                    let! c = Crypto.service
                    return! c.RandomUUIDv4
                })

        toBe uuid "00010203-0405-4607-8809-0a0b0c0d0e0f")

    test "randomUUIDv7 formats UUID bytes with the Clock timestamp" (fun () ->
        let context = ctx |> Context.add Clock.tag (fixedClock 0x0123456789abL)

        let uuid =
            run
                context
                (effect {
                    let! c = Crypto.service
                    return! c.RandomUUIDv7
                })

        toBe uuid "01234567-89ab-7607-8809-0a0b0c0d0e0f")

    test "digest delegates to the service" (fun () ->
        let digest =
            run
                ctx
                (effect {
                    let! c = Crypto.service
                    return! c.Digest SHA256 [| 1uy; 2uy; 3uy |]
                })

        toBe (digest = [| 3uy; 7uy |]) true)

    test "randomBytes validates a negative size" (fun () ->
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
            | (e: PlatformError) :: _ -> toBe e.Tag "PlatformError"
            | [] -> failwith "expected a typed PlatformError"
        | Success _ -> failwith "expected failure"))
