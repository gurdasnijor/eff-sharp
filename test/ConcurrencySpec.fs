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

    itEffect "forEachPar overlaps work" (fun () ->
        let active = ref 0
        let maxActive = ref 0

        let work x =
            effect {
                do!
                    Effect.sync (fun () ->
                        active.Value <- active.Value + 1
                        maxActive.Value <- max maxActive.Value active.Value)

                do! Effect.sleep 20
                do! Effect.sync (fun () -> active.Value <- active.Value - 1)
                return x
            }

        Effect.forEachPar [ 1; 2; 3; 4 ] work
        |> Effect.map (fun ys ->
            toEqual ys [ 1; 2; 3; 4 ]
            toBe (maxActive.Value > 1) true))

    itEffect "forEachParN bounds concurrency and preserves order" (fun () ->
        let active = ref 0
        let maxActive = ref 0

        let work x =
            effect {
                do!
                    Effect.sync (fun () ->
                        active.Value <- active.Value + 1
                        maxActive.Value <- max maxActive.Value active.Value)

                do! Effect.sleep 20
                do! Effect.sync (fun () -> active.Value <- active.Value - 1)
                return x + 1
            }

        Effect.forEachParN 2 [ 1; 2; 3; 4; 5 ] work
        |> Effect.map (fun ys ->
            toEqual ys [ 2; 3; 4; 5; 6 ]
            toBe maxActive.Value 2))

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
        |> Effect.map (fun err -> toBe err "stop"))

    itEffect "forEachPar propagates parent interruption to children" (fun () ->
        let released = ref 0

        let child _ =
            Effect.sleep 250
            |> Effect.ensuring (Effect.sync (fun () -> released.Value <- released.Value + 1))

        effect {
            let! fiber = Effect.fork (Effect.forEachPar [ 1; 2; 3 ] child |> Effect.map ignore)
            do! Effect.sleep 30
            do! Fiber.interrupt fiber
            let! exit = Fiber.await fiber
            toBe (Exit.isFailure exit) true
            toBe released.Value 3
        })

    itEffect "zipPar observes parent interruption" (fun () ->
        effect {
            let! fiber =
                Effect.fork (
                    Effect.zipPar (Effect.sleep 250) (Effect.sleep 250)
                    |> Effect.map ignore
                )

            do! Effect.sleep 30
            do! Fiber.interrupt fiber
            let! exit = Fiber.await fiber
            toBe (Exit.isFailure exit) true
        }))
