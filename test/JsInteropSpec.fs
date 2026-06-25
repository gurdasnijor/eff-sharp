module JsInteropSpec

open Effect
open Effect.Vitest
open Fable.Core

[<Emit("new ReadableStream({ start: function(controller) { $0.forEach(function(value) { controller.enqueue(value); }); controller.close(); } })")>]
let private readableStreamOfArray (_values: int array) : obj = jsNative

let private same actual expected = toBe (actual = expected) true

describe "JS Promise / stream interop" (fun () ->
    itEffect "Effect.promiseJSWith awaits a native Promise" (fun () ->
        effect {
            let! value =
                Effect.promiseJSWith (fun () ->
                    Promise.lift 40
                    |> Promise.map (fun n -> n + 2))

            toBe value 42
        })

    itEffect "Effect.tryPromiseJS maps Promise rejection into the typed error channel" (fun () ->
        effect {
            let! exit =
                Effect.exit (
                    Effect.tryPromiseJS
                        (fun () -> Promise.reject<int> (System.Exception "boom"))
                        (fun ex -> "mapped: " + ex.Message)
                )

            match exit with
            | Success _ -> failwith "expected Promise rejection"
            | Failure cause -> same (Cause.failures cause) [ "mapped: boom" ]
        })

    itEffect "Stream.fromAsyncIterableJS consumes a Fable.Promise AsyncIterable" (fun () ->
        effect {
            let mutable next = 0

            let iterable =
                AsyncIterable.create (fun () ->
                    next <- next + 1

                    if next <= 3 then
                        Promise.lift (Some next)
                    else
                        Promise.lift None)

            let stream: Stream<int, string, Context> =
                Stream.fromAsyncIterableJS iterable (fun ex -> ex.Message)

            let! values = Stream.runCollect stream
            same values [ 1; 2; 3 ]
        })

    itEffect "Stream.fromAsyncIterableJS propagates effectful emit failures" (fun () ->
        effect {
            let mutable next = 0

            let iterable =
                AsyncIterable.create (fun () ->
                    next <- next + 1

                    if next <= 3 then
                        Promise.lift (Some next)
                    else
                        Promise.lift None)

            let stream: Stream<int, string, Context> =
                Stream.fromAsyncIterableJS iterable (fun ex -> ex.Message)
                |> Stream.mapEffect (fun value ->
                    if value = 2 then
                        Effect.fail "bad emit"
                    else
                        Effect.succeed value)

            let! exit = Effect.exit (Stream.runCollect stream)

            match exit with
            | Success _ -> failwith "expected failure"
            | Failure cause -> same (Cause.failures cause) [ "bad emit" ]
        })

    itEffect "Stream.fromReadableStreamJS consumes a WHATWG ReadableStream" (fun () ->
        effect {
            let stream: Stream<int, string, Context> =
                Stream.fromReadableStreamJS (readableStreamOfArray [| 4; 5; 6 |]) (fun ex -> ex.Message)

            let! values = Stream.runCollect stream
            same values [ 4; 5; 6 ]
        })

    itEffect "Stream.unwrap runs the stream produced by an Effect" (fun () ->
        effect {
            let delayed: Effect<Stream<int, string, Context>, string, Context> =
                Effect.succeed (Stream.make [ 7; 8; 9 ])

            let! values = Stream.unwrap delayed |> Stream.runCollect
            same values [ 7; 8; 9 ]
        }))
