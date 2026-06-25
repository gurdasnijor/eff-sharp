module PubSubSpec

open Effect
open Effect.Vitest

let private same actual expected = toBe (actual = expected) true

let private hasInterrupt (exit: Exit<'A, 'E>) : bool =
    match exit with
    | Failure cause ->
        cause.Reasons
        |> List.exists (function
            | Reason.Interrupt _ -> true
            | _ -> false)
    | Success _ -> false

let private publishAllCase capacity =
    itEffect (sprintf "publishAll - capacity %d" capacity) (fun () ->
        effect {
            let! pubsub = PubSub.bounded capacity

            return!
                Scope.scoped (fun scope ->
                    effect {
                        let! sub1 = PubSub.subscribe scope pubsub
                        let! sub2 = PubSub.subscribe scope pubsub
                        let! _ = PubSub.publishAll pubsub [ 1; 2 ]
                        let! taken1 = PubSub.takeAll sub1
                        let! taken2 = PubSub.takeAll sub2
                        same taken1 [ 1; 2 ]
                        same taken2 [ 1; 2 ]
                    })
        })

let private concurrentOneToOne name makePubSub publish =
    itEffect name (fun () ->
        let values = [ 0..63 ]

        effect {
            let! pubsub = makePubSub

            return!
                Scope.scoped (fun scope ->
                    effect {
                        let! sub = PubSub.subscribe scope pubsub
                        let! subscriber = Effect.fork (Effect.forEach values (fun _ -> PubSub.take sub))
                        let! _ = Effect.fork (publish pubsub values)
                        let! exit = Fiber.await subscriber
                        same exit (Exit.succeed values)
                    })
        })

describe "PubSub" (fun () ->
    publishAllCase 2
    publishAllCase 3
    publishAllCase 4

    itEffect "sequential one publisher one subscriber" (fun () ->
        let values = [ 0..9 ]

        effect {
            let! latch = Latch.make false
            let! pubsub = PubSub.bounded 10

            return!
                Scope.scoped (fun scope ->
                    effect {
                        let! sub = PubSub.subscribe scope pubsub
                        let! fiber =
                            Effect.fork (
                                Latch.await latch
                                |> Effect.flatMap (fun () -> Effect.forEach values (fun _ -> PubSub.take sub))
                            )

                        let! _ = PubSub.publishAll pubsub values
                        let! _ = Latch.``open`` latch
                        let! exit = Fiber.await fiber
                        same exit (Exit.succeed values)
                    })
        })

    itEffect "sequential one publisher two subscribers" (fun () ->
        let values = [ 0..9 ]
        let taker latch sub = Latch.await latch |> Effect.flatMap (fun () -> Effect.forEach values (fun _ -> PubSub.take sub))

        effect {
            let! latch = Latch.make false
            let! pubsub = PubSub.bounded 10

            return!
                Scope.scoped (fun scope ->
                    effect {
                        let! sub1 = PubSub.subscribe scope pubsub
                        let! sub2 = PubSub.subscribe scope pubsub
                        let! f1 = Effect.fork (taker latch sub1)
                        let! f2 = Effect.fork (taker latch sub2)
                        let! _ = PubSub.publishAll pubsub values
                        let! _ = Latch.``open`` latch
                        let! r1 = Fiber.await f1
                        let! r2 = Fiber.await f2
                        same r1 (Exit.succeed values)
                        same r2 (Exit.succeed values)
                    })
        })

    concurrentOneToOne "backpressured concurrent - one to one" (PubSub.bounded 64) (fun pubsub values ->
        PubSub.publishAll pubsub values)

    concurrentOneToOne "dropping concurrent - one to one" (PubSub.dropping 64) (fun pubsub values ->
        Effect.forEach values (PubSub.publish pubsub) |> Effect.map (fun _ -> true))

    concurrentOneToOne "sliding concurrent - one to one" (PubSub.sliding 64) (fun pubsub values ->
        Effect.forEach values (PubSub.publish pubsub) |> Effect.map (fun _ -> true))

    concurrentOneToOne "unbounded concurrent - one to one" (PubSub.unbounded ()) (fun pubsub values ->
        PubSub.publishAll pubsub values)

    itEffect "null values" (fun () ->
        let messages: string list = [ "1"; null ]

        effect {
            let! pubsub = PubSub.unbounded ()

            return!
                Scope.scoped (fun scope ->
                    effect {
                        let! sub = PubSub.subscribe scope pubsub
                        let! _ = PubSub.publishAll pubsub messages
                        let! taken = PubSub.takeAll sub
                        same taken messages
                    })
        })

    itEffect "publish does not increase size while no subscribers" (fun () ->
        effect {
            let! pubsub = PubSub.dropping 2
            let! _ = PubSub.publish pubsub 1
            let! _ = PubSub.publish pubsub 2
            toBe (PubSub.sizeUnsafe pubsub) 0
        })

    itEffect "shutdown interrupts suspended take" (fun () ->
        effect {
            let! pubsub = PubSub.unbounded ()

            return!
                Scope.scoped (fun scope ->
                    effect {
                        let! sub = PubSub.subscribe scope pubsub
                        let! fiber = Effect.fork (PubSub.take sub)
                        do! Effect.sleep 50
                        do! PubSub.shutdown pubsub
                        let! exit = Fiber.await fiber
                        toBe (hasInterrupt exit) true
                    })
        })

    itEffect "shutdown interrupts suspended takeAll" (fun () ->
        effect {
            let! pubsub = PubSub.unbounded ()

            return!
                Scope.scoped (fun scope ->
                    effect {
                        let! sub = PubSub.subscribe scope pubsub
                        let! fiber = Effect.fork (PubSub.takeAll sub)
                        do! Effect.sleep 50
                        do! PubSub.shutdown pubsub
                        let! exit = Fiber.await fiber
                        toBe (hasInterrupt exit) true
                    })
        })

    itEffect "publish returns false after shutdown" (fun () ->
        effect {
            let! pubsub = PubSub.unbounded ()
            do! PubSub.shutdown pubsub
            let! result = PubSub.publish pubsub 1
            toBe result false
        })

    itEffect "publishAll returns false after shutdown" (fun () ->
        effect {
            let! pubsub = PubSub.unbounded ()
            do! PubSub.shutdown pubsub
            let! result = PubSub.publishAll pubsub [ 1; 2; 3 ]
            toBe result false
        }))
