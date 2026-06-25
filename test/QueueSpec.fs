module QueueSpec

open Effect
open Effect.Vitest

let private same actual expected = toBe (actual = expected) true

let private causeIsDone (cause: Cause<obj>) : bool =
    cause.Reasons
    |> List.exists (function
        | Reason.Fail e -> Queue.isDone e
        | _ -> false)

let private causeHasInterrupt (cause: Cause<obj>) : bool =
    cause.Reasons
    |> List.exists (function
        | Reason.Interrupt _ -> true
        | _ -> false)

let private firstError (cause: Cause<obj>) : obj option =
    cause.Reasons
    |> List.tryPick (function
        | Reason.Fail e -> Some e
        | _ -> None)

describe "Queue" (fun () ->
    itEffect "offer then take" (fun () ->
        effect {
            let! queue = Queue.bounded 10
            let! offered = Queue.offer queue 42
            let! value = Queue.take queue
            toBe offered true
            toBe value 42
        })

    itEffect "poll returns Option for non-blocking take" (fun () ->
        effect {
            let! queue = Queue.bounded 10
            let! empty = Queue.poll queue
            let! _ = Queue.offer queue 42
            let! some = Queue.poll queue
            let! empty2 = Queue.poll queue
            same empty None
            same some (Some 42)
            same empty2 None
        })

    itEffect "peek views item without removing it" (fun () ->
        effect {
            let! queue = Queue.bounded 10
            let! _ = Queue.offer queue 42
            let! peek1 = Queue.peek queue
            let! size1 = Queue.size queue
            let! peek2 = Queue.peek queue
            let! taken = Queue.take queue
            let! size2 = Queue.size queue
            toBe peek1 42
            toBe size1 1
            toBe peek2 42
            toBe taken 42
            toBe size2 0
        })

    itEffect "offer dropping" (fun () ->
        effect {
            let! queue = Queue.make 2 Dropping
            let! remaining = Queue.offerAll queue [ 1; 2; 3; 4 ]
            let! offered = Queue.offer queue 5
            let! taken = Queue.takeAll queue
            same remaining [ 3; 4 ]
            toBe offered false
            same taken [ 1; 2 ]
        })

    itEffect "offer sliding" (fun () ->
        effect {
            let! queue = Queue.make 2 Sliding
            let! remaining = Queue.offerAll queue [ 1; 2; 3; 4 ]
            let! offered = Queue.offer queue 5
            let! taken = Queue.takeAll queue
            same remaining []
            toBe offered true
            same taken [ 4; 5 ]
        })

    itEffect "takeN and size" (fun () ->
        effect {
            let! queue = Queue.unbounded ()
            let! size0 = Queue.size queue
            let! _ = Queue.offerAll queue [ 1; 2; 3; 4 ]
            let! size4 = Queue.size queue
            let! first = Queue.takeN queue 2
            let! second = Queue.takeN queue 2
            toBe size0 0
            toBe size4 4
            same first [ 1; 2 ]
            same second [ 3; 4 ]
        })

    itEffect "end completes takes after draining" (fun () ->
        effect {
            let! queue = Queue.bounded 2
            let! _ = Queue.offer queue 1
            let! _ = Queue.offer queue 2
            let! _ = Queue.endQueue queue
            let! first = Queue.take queue
            let! second = Queue.take queue
            let! exit = Effect.exit (Queue.take queue)
            toBe first 1
            toBe second 2

            match exit with
            | Failure cause -> toBe (causeIsDone cause) true
            | Success value -> failwithf "expected Done failure, got %d" value
        })

    itEffect "end then await succeeds and offer is rejected" (fun () ->
        effect {
            let! queue = Queue.bounded 2
            let! ended = Queue.endQueue queue
            do! Queue.await queue
            let! offered = Queue.offer queue 10
            toBe ended true
            toBe offered false
        })

    itEffect "collect does not duplicate messages" (fun () ->
        effect {
            let! queue = Queue.unbounded ()
            let! _ = Queue.offer queue 0
            let! _ = Queue.endQueue queue
            let! result = Queue.collect queue
            same result [ 0 ]
        })

    itEffect "fail drains buffered values before failing takers" (fun () ->
        effect {
            let! queue = Queue.unbounded ()
            let! _ = Queue.offerAll queue [ 1; 2; 3; 4; 5 ]
            let! _ = Queue.fail queue (box "boom")
            let! drained = Queue.takeAll queue
            let! exit = Effect.exit (Queue.takeAll queue)
            same drained [ 1; 2; 3; 4; 5 ]

            match exit with
            | Failure cause ->
                toBe (causeIsDone cause) false
                match firstError cause with
                | Some error -> toBe (string error) "boom"
                | None -> failwith "expected boxed failure"
            | Success values -> failwithf "expected failure, got %A" values
        })

    itEffect "interrupt allows draining then fails" (fun () ->
        effect {
            let! queue = Queue.bounded 10
            let! _ = Queue.offerAll queue [ 1; 2; 3; 4; 5 ]
            let! interrupted = Queue.interrupt queue
            let! offered = Queue.offer queue 6
            let! drained = Queue.takeN queue 5
            let! exit = Effect.exit (Queue.take queue)
            toBe interrupted true
            toBe offered false
            same drained [ 1; 2; 3; 4; 5 ]

            match exit with
            | Failure cause -> toBe (causeHasInterrupt cause) true
            | Success value -> failwithf "expected interrupt failure, got %d" value
        })

    itEffect "shutdown drops buffered values and fails" (fun () ->
        effect {
            let! queue = Queue.bounded 10
            let! _ = Queue.offerAll queue [ 1; 2; 3 ]
            let! shutdown = Queue.shutdown queue
            let! offered = Queue.offer queue 10
            let! exit = Effect.exit (Queue.takeAll queue)
            toBe shutdown true
            toBe offered false

            match exit with
            | Failure cause -> toBe (causeHasInterrupt cause) true
            | Success values -> failwithf "expected interrupt failure, got %A" values
        })

    itEffect "bounded offerAll waits until capacity is released" (fun () ->
        effect {
            let! queue = Queue.bounded 2
            let! fiber = Effect.fork (Queue.offerAll queue [ 1; 2; 3; 4 ])
            let! first = Queue.takeAll queue
            let! second = Queue.takeAll queue
            let! exit = Fiber.await fiber
            same first [ 1; 2 ]
            same second [ 3; 4 ]
            same exit (Exit.succeed [])
        })

    itEffect "bounded 0 capacity rendezvous" (fun () ->
        effect {
            let! queue = Queue.bounded 0
            let! _ = Effect.fork (Queue.offer queue 1)
            let! first = Queue.take queue
            let! takeFiber = Effect.fork (Queue.take queue)
            let! _ = Queue.offer queue 2
            let! second = Fiber.join takeFiber
            toBe first 1
            toBe second 2
        })

    itEffect "await waits for items then completes" (fun () ->
        effect {
            let! queue = Queue.unbounded ()
            let! awaitFiber = Effect.fork (Queue.await queue)
            let! _ = Queue.offer queue 1
            let! _ = Queue.endQueue queue
            let! pending = Fiber.poll awaitFiber
            let! items = Queue.takeAll queue
            let! exit = Fiber.await awaitFiber
            toBe (Option.isNone pending) true
            same items [ 1 ]
            same exit (Exit.succeed ())
        })

    itEffect "concurrent producers and consumer" (fun () ->
        effect {
            let! queue = Queue.bounded 4
            let! _ = Effect.fork (Queue.offerAll queue [ 1; 2; 3; 4; 5 ])
            let! _ = Effect.fork (Queue.offerAll queue [ 6; 7; 8; 9; 10 ])
            let! taken = Queue.takeN queue 10
            toBe (List.sum taken) 55
        })

    itEffect "toStream emits buffered items then ends when interrupted" (fun () ->
        effect {
            let! queue = Queue.unbounded ()
            let! _ = Queue.offer queue 1
            let! _ = Queue.offer queue 2
            let! _ = Queue.offer queue 3
            let! _ = Queue.interrupt queue
            let! items = Stream.runCollect (Queue.toStream queue)
            same items [ 1; 2; 3 ]
        })

    itEffect "offerUnsafe accepts into an unbounded queue and is takeable" (fun () ->
        effect {
            let! queue = Queue.unbounded ()
            let accepted = Queue.offerUnsafe queue 42
            let! taken = Queue.take queue
            toBe accepted true
            toBe taken 42
        })

    itEffect "offerUnsafe rejects for full dropping and suspend queues" (fun () ->
        effect {
            let! dropping = Queue.make 1 Dropping
            let d1 = Queue.offerUnsafe dropping 1
            let d2 = Queue.offerUnsafe dropping 2
            let! suspend = Queue.bounded 1
            let s1 = Queue.offerUnsafe suspend 1
            let s2 = Queue.offerUnsafe suspend 2
            toBe d1 true
            toBe d2 false
            toBe s1 true
            toBe s2 false
        })

    itEffect "offerUnsafe evicts oldest in a sliding queue" (fun () ->
        effect {
            let! queue = Queue.make 2 Sliding
            let _ = Queue.offerUnsafe queue 1
            let _ = Queue.offerUnsafe queue 2
            let accepted = Queue.offerUnsafe queue 3
            let! taken = Queue.takeAll queue
            toBe accepted true
            same taken [ 2; 3 ]
        })

    itEffect "offerUnsafe rejects once completed and feeds toStream" (fun () ->
        effect {
            let! completed = Queue.unbounded ()
            let! _ = Queue.endQueue completed
            toBe (Queue.offerUnsafe completed 1) false

            let! queue = Queue.unbounded ()
            let _ = Queue.offerUnsafe queue 1
            let _ = Queue.offerUnsafe queue 2
            let _ = Queue.offerUnsafe queue 3
            let! _ = Queue.endQueue queue
            let! items = Stream.runCollect (Queue.toStream queue)
            same items [ 1; 2; 3 ]
        }))
