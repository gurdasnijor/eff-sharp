module ConcurrencySpec

// Parallel combinators on the Effect core. Mirrors the intent of upstream
// Effect.forEach/all with a `concurrency` option (Effect.test.ts concurrency
// cases); v1 covers fan-out, bounded degree, parallel zip, and the `and!`
// applicative sugar.

open Effect
open Effect.Vitest

describe "Effect — parallel combinators" (fun () ->
    itEffect "forEachPar collects in input order" (fun () ->
        Effect.forEachPar [ 1; 2; 3; 4; 5 ] (fun x -> Effect.succeed (x * 10))
        |> Effect.map (fun ys -> toEqual ys [ 10; 20; 30; 40; 50 ]))

    itEffect "forEachParN bounds concurrency and preserves order" (fun () ->
        Effect.forEachParN 2 [ 1; 2; 3; 4; 5 ] (fun x -> Effect.succeed (x + 1))
        |> Effect.map (fun ys -> toEqual ys [ 2; 3; 4; 5; 6 ]))

    itEffect "collectAllPar runs all and collects" (fun () ->
        Effect.collectAllPar [ Effect.succeed 1; Effect.succeed 2; Effect.succeed 3 ]
        |> Effect.map (fun ys -> toEqual ys [ 1; 2; 3 ]))

    itEffect "zipPar pairs two concurrent successes" (fun () ->
        Effect.zipPar (Effect.succeed "a") (Effect.succeed 1)
        |> Effect.map (fun pair -> toEqual pair ("a", 1)))

    itEffect "zipWithPar combines two concurrent successes" (fun () ->
        Effect.succeed 2
        |> Effect.zipWithPar (+) (Effect.succeed 3)
        |> Effect.map (fun n -> toBe n 5))

    itEffect "and! runs sources concurrently then binds the pair" (fun () ->
        effect {
            let! a = Effect.succeed 2
            and! b = Effect.succeed 3
            return toBe (a + b) 5
        })

    itEffect "forEachPar surfaces a failure as a typed failure" (fun () ->
        Effect.forEachPar [ 1; 2; 3 ] (fun x ->
            if x = 2 then Effect.fail "boom" else Effect.succeed x)
        |> Effect.exit
        |> Effect.map (fun ex ->
            match ex with
            | Failure _ -> toBe true true
            | Success _ -> toBe true false))

    itEffect "forEachParN short-circuits to the failure" (fun () ->
        Effect.forEachParN 2 [ 1; 2; 3; 4 ] (fun x ->
            if x = 3 then Effect.fail "stop" else Effect.succeed x)
        |> Effect.flip
        |> Effect.map (fun err -> toBe err "stop")))
