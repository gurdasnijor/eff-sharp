namespace Effect

#if FABLE_COMPILER
open Fable.Core
#endif

/// Slice: wave-5. `Stream` — a pull-of-many effectful values (port of Stream.ts).
///
/// Representation: a **sink/push fold** — a stream is "run me with a per-element
/// callback". `Stream<'A,'E,'R> = (('A -> Effect<unit,'E,'R>) -> Effect<unit,'E,'R>)`.
/// This is stack-safe (it rides the `Effect`/`Async` core), makes `yield`/`map`/
/// `filter`/`flatMap` trivial, and gives a *clean onramp to the `stream { }`
/// computation expression* from docs/computation-expressions.md — the headline
/// ergonomic win over Effect's combinator-only authoring.
///
/// Departure from upstream: Effect's `Stream` is a chunked, pull-based `Channel`
/// with backpressure/concurrency. This v1 is element-at-a-time and single-consumer;
/// chunking, `Sink`/`Channel`, and concurrent operators are deferred. Early
/// termination (`take`) uses an internal stop signal caught at the run boundary.
type Stream<'A, 'E, 'R> =
    internal
        { Run: ('A -> Effect<unit, 'E, 'R>) -> Effect<unit, 'E, 'R> }

[<RequireQualifiedAccess>]
module Stream =

    exception private StreamStop

#if FABLE_COMPILER
    [<Emit("!!$0.done")>]
    let private iteratorResultDone (_result: obj) : bool = jsNative

    [<Emit("$0.value")>]
    let private iteratorResultValue (_result: obj) : 'A = jsNative

    [<Emit("$0.getReader()")>]
    let private readableGetReader (_readable: obj) : obj = jsNative

    [<Emit("$0.read()")>]
    let private readableReaderRead (_reader: obj) : JS.Promise<obj> = jsNative

    [<Emit("if ($0.releaseLock) { $0.releaseLock(); }")>]
    let private readableReleaseLock (_reader: obj) : unit = jsNative
#endif

    // --- constructors ---

    let empty<'A, 'E, 'R> : Stream<'A, 'E, 'R> = { Run = fun _ -> Effect.succeed () }

    /// A one-element stream. (Stream.succeed)
    let succeed (value: 'A) : Stream<'A, 'E, 'R> = { Run = fun emit -> emit value }

    /// A stream of the given elements. (Stream.fromIterable)
    let fromIterable (items: seq<'A>) : Stream<'A, 'E, 'R> =
        { Run = fun emit -> Effect.forEach items emit |> Effect.map ignore }

    /// Elements of a list. (Stream.make)
    let make (items: 'A list) : Stream<'A, 'E, 'R> = fromIterable items

    /// `lo..hi` inclusive. (Stream.range)
    let range (lo: int) (hi: int) : Stream<int, 'E, 'R> = fromIterable (seq { lo..hi })

    /// A stream from an effect's success. (Stream.fromEffect)
    let fromEffect (eff: Effect<'A, 'E, 'R>) : Stream<'A, 'E, 'R> =
        { Run = fun emit -> eff |> Effect.flatMap emit }

    /// Repeatedly run `pull`, emitting each `Some` value and ending on the first
    /// `None`. If `pull` fails, the stream ends with that error. (Stream.repeatEffectOption)
    ///
    /// The public constructor for bridging an asynchronous *pull* source — a Node
    /// `Readable` read, a .NET `Stream.ReadAsync` loop — into a `Stream`, without
    /// reaching the internal `Stream` representation. Platform packages
    /// (`Effect.Platform.Node`) build their `Readable → Stream` adapters on it.
    /// Stack-safe: the loop threads through `Effect.flatMap` (the Async trampoline).
    let repeatEffectOption (pull: Effect<'A option, 'E, 'R>) : Stream<'A, 'E, 'R> =
        { Run =
            fun emit ->
                let rec loop () =
                    pull
                    |> Effect.flatMap (function
                        | Some a -> emit a |> Effect.flatMap loop
                        | None -> Effect.succeed ())

                loop () }

    /// Lazily unwrap an effect that produces a stream. (Stream.unwrap)
    let unwrap (effect: Effect<Stream<'A, 'E, 'R>, 'E, 'R>) : Stream<'A, 'E, 'R> =
        { Run = fun emit -> effect |> Effect.flatMap (fun stream -> stream.Run emit) }

#if FABLE_COMPILER
    /// Bridge a JavaScript `AsyncIterable` into eff-sharp `Stream`.
    let fromAsyncIterableJS (iterable: JS.AsyncIterable<'A>) (onError: exn -> 'E) : Stream<'A, 'E, 'R> =
        { Run =
            fun emit ->
                Effect(fun fib env ->
                    async {
                        let mutable failure: Cause<'E> option = None
                        let mutable chain: JS.Promise<unit> = Promise.lift ()

                        try
                            do!
                                AsyncIterable.iter
                                    (fun _ value ->
                                        chain <-
                                            chain
                                            |> Promise.bind (fun () ->
                                                match failure with
                                                | Some _ -> Promise.lift ()
                                                | None ->
                                                    let (Effect run) = emit value

                                                    async {
                                                        let! exit = run fib env

                                                        match exit with
                                                        | Success() -> ()
                                                        | Failure cause -> failure <- Some cause
                                                    }
                                                    |> Async.StartAsPromise))
                                    iterable
                                |> Async.AwaitPromise

                            do! Async.AwaitPromise chain

                            match failure with
                            | Some cause -> return Failure cause
                            | None -> return Success()
                        with ex ->
                            return Exit.fail (onError ex)
                    }) }

    /// Bridge a WHATWG `ReadableStream` into eff-sharp `Stream`.
    let fromReadableStreamJS (readable: obj) (onError: exn -> 'E) : Stream<'A, 'E, 'R> =
        { Run =
            fun emit ->
                Effect(fun fib env ->
                    async {
                        let reader = readableGetReader readable

                        try
                            let pull =
                                Effect.tryPromiseJS
                                    (fun () -> readableReaderRead reader)
                                    onError
                                |> Effect.map (fun result ->
                                    if iteratorResultDone result then
                                        None
                                    else
                                        Some(iteratorResultValue result))

                            let stream = repeatEffectOption pull
                            let (Effect run) = stream.Run emit
                            return! run fib env
                        finally
                            readableReleaseLock reader
                    }) }
#else
    /// JavaScript AsyncIterable interop is only executable under Fable/JS.
    let fromAsyncIterableJS (_iterable: Fable.Core.JS.AsyncIterable<'A>) (_onError: exn -> 'E) : Stream<'A, 'E, 'R> =
        fromEffect (Effect.sync (fun () -> failwith "Stream.fromAsyncIterableJS is only available on the Fable/JS target"))

    /// JavaScript ReadableStream interop is only executable under Fable/JS.
    let fromReadableStreamJS (_readable: obj) (_onError: exn -> 'E) : Stream<'A, 'E, 'R> =
        fromEffect (Effect.sync (fun () -> failwith "Stream.fromReadableStreamJS is only available on the Fable/JS target"))
#endif

    // --- combinators ---

    let map (f: 'A -> 'B) (self: Stream<'A, 'E, 'R>) : Stream<'B, 'E, 'R> =
        { Run = fun emit -> self.Run(fun a -> emit (f a)) }

    /// Map each element through an effect. (Stream.mapEffect)
    let mapEffect (f: 'A -> Effect<'B, 'E, 'R>) (self: Stream<'A, 'E, 'R>) : Stream<'B, 'E, 'R> =
        { Run = fun emit -> self.Run(fun a -> f a |> Effect.flatMap emit) }

    let filter (p: 'A -> bool) (self: Stream<'A, 'E, 'R>) : Stream<'A, 'E, 'R> =
        { Run = fun emit -> self.Run(fun a -> if p a then emit a else Effect.succeed ()) }

    let flatMap (f: 'A -> Stream<'B, 'E, 'R>) (self: Stream<'A, 'E, 'R>) : Stream<'B, 'E, 'R> =
        { Run = fun emit -> self.Run(fun a -> (f a).Run emit) }

    /// Recover from a stream failure by switching to another stream.
    let catchAll (handler: 'E -> Stream<'A, 'E, 'R>) (self: Stream<'A, 'E, 'R>) : Stream<'A, 'E, 'R> =
        { Run = fun emit -> self.Run emit |> Effect.catchAll (fun error -> (handler error).Run emit) }

    /// `a` then `b`. (Stream.concat)
    let concat (a: Stream<'A, 'E, 'R>) (b: Stream<'A, 'E, 'R>) : Stream<'A, 'E, 'R> =
        { Run = fun emit -> a.Run emit |> Effect.flatMap (fun () -> b.Run emit) }

    /// Run an effect per element, passing it through. (Stream.tap)
    let tap (f: 'A -> Effect<unit, 'E, 'R>) (self: Stream<'A, 'E, 'R>) : Stream<'A, 'E, 'R> =
        { Run = fun emit -> self.Run(fun a -> f a |> Effect.flatMap (fun () -> emit a)) }

    /// Take at most `n` elements, then stop the producer. (Stream.take)
    let take (n: int) (self: Stream<'A, 'E, 'R>) : Stream<'A, 'E, 'R> =
        { Run =
            fun emit ->
                let mutable count = 0

                let wrapped a =
                    if count >= n then
                        Effect.sync (fun () -> raise StreamStop)
                    else
                        count <- count + 1

                        emit a
                        |> Effect.flatMap (fun () ->
                            if count >= n then
                                Effect.sync (fun () -> raise StreamStop)
                            else
                                Effect.succeed ())

                self.Run wrapped
                |> Effect.catchDefect (fun ex ->
                    match ex with
                    | :? StreamStop -> Effect.succeed ()
                    | _ -> Effect.sync (fun () -> raise ex)) }

    // --- runners ---

    /// Run the stream, invoking `f` per element. (Stream.runForEach)
    let runForEach (f: 'A -> Effect<unit, 'E, 'R>) (self: Stream<'A, 'E, 'R>) : Effect<unit, 'E, 'R> = self.Run f

    /// Run to completion, discarding elements. (Stream.runDrain)
    let runDrain (self: Stream<'A, 'E, 'R>) : Effect<unit, 'E, 'R> = self.Run(fun _ -> Effect.succeed ())

    /// Run, collecting all elements into a list. (Stream.runCollect)
    let runCollect (self: Stream<'A, 'E, 'R>) : Effect<'A list, 'E, 'R> =
        let buffer = System.Collections.Generic.List<'A>()

        self.Run(fun a -> Effect.sync (fun () -> buffer.Add a))
        |> Effect.map (fun () -> List.ofSeq buffer)

    /// Run the stream into a `Sink`, producing the sink's output. (Stream.run)
    ///
    /// The sink's `emit` may short-circuit by raising the internal `SinkDone`
    /// signal (e.g. `Sink.head`, `Sink.reduceWhile` stopping); it is caught here
    /// at the run boundary so the producer halts early, then `extract` runs.
    let run (sink: Sink<'A, 'B, 'E, 'R>) (self: Stream<'A, 'E, 'R>) : Effect<'B, 'E, 'R> =
        let emit, extract = sink.Build()

        self.Run emit
        |> Effect.catchDefect (fun ex ->
            match ex with
            | :? SinkDone -> Effect.succeed ()
            | _ -> Effect.sync (fun () -> raise ex))
        |> Effect.flatMap (fun () -> extract ())

/// The `stream { yield ... }` computation expression (docs/computation-expressions.md).
type StreamBuilder() =
    member _.Yield(value: 'A) : Stream<'A, 'E, 'R> = Stream.succeed value
    member _.YieldFrom(s: Stream<'A, 'E, 'R>) : Stream<'A, 'E, 'R> = s
    member _.Zero() : Stream<'A, 'E, 'R> = Stream.empty
    member _.Combine(a: Stream<'A, 'E, 'R>, b: Stream<'A, 'E, 'R>) : Stream<'A, 'E, 'R> = Stream.concat a b
    member _.Delay(f: unit -> Stream<'A, 'E, 'R>) : Stream<'A, 'E, 'R> = { Run = fun emit -> (f ()).Run emit }

    /// `for x in xs do yield ...` — flatMap over a collection.
    member _.For(items: seq<'T>, f: 'T -> Stream<'A, 'E, 'R>) : Stream<'A, 'E, 'R> =
        { Run = fun emit -> Effect.forEach items (fun x -> (f x).Run emit) |> Effect.map ignore }

[<AutoOpen>]
module StreamBuilderModule =
    /// `stream { yield 1; yield! other; for x in xs do yield x }`
    let stream = StreamBuilder()
