module SubscriptionRefSpec

open Effect
open Effect.Vitest

let private same actual expected = toBe (actual = expected) true

describe "SubscriptionRef" (fun () ->
    itEffect "get returns the initial value and set updates it" (fun () ->
        effect {
            let! ref = SubscriptionRef.make 0
            let! before = SubscriptionRef.get ref
            do! SubscriptionRef.set ref 42
            let! after = SubscriptionRef.get ref
            toBe before 0
            toBe after 42
        })

    itEffect "update getAndSet and modify" (fun () ->
        effect {
            let! ref = SubscriptionRef.make 10
            do! SubscriptionRef.update ref (fun n -> n + 1)
            let! updated = SubscriptionRef.get ref
            let! previous = SubscriptionRef.getAndSet ref 20
            let! modified = SubscriptionRef.modify ref (fun n -> (sprintf "was %d" n, n * 2))
            let! current = SubscriptionRef.get ref
            toBe updated 11
            toBe previous 11
            toBe modified "was 20"
            toBe current 40
        })

    itEffect "changes streams the current value and subsequent updates" (fun () ->
        effect {
            let! ref = SubscriptionRef.make 0
            let! ready = Deferred.make ()

            let collector =
                SubscriptionRef.changes ref
                |> Stream.tap (fun _ -> Deferred.succeed ready () |> Effect.map ignore)
                |> Stream.take 3
                |> Stream.runCollect

            let! fiber = Effect.fork collector
            do! Deferred.await ready
            do! SubscriptionRef.set ref 1
            do! SubscriptionRef.set ref 2
            let! result = Fiber.join fiber
            same result [ 0; 1; 2 ]
        })

    itEffect "a late subscriber sees the value current at subscription time" (fun () ->
        effect {
            let! ref = SubscriptionRef.make 0
            do! SubscriptionRef.set ref 1
            do! SubscriptionRef.set ref 2

            let! ready = Deferred.make ()

            let collector =
                SubscriptionRef.changes ref
                |> Stream.tap (fun _ -> Deferred.succeed ready () |> Effect.map ignore)
                |> Stream.take 2
                |> Stream.runCollect

            let! fiber = Effect.fork collector
            do! Deferred.await ready
            do! SubscriptionRef.set ref 3
            let! result = Fiber.join fiber
            same result [ 2; 3 ]
        }))
