namespace Effect

/// Slice: wave-6. `Sink` — a stream consumer (port of Sink.ts), pragmatic v1.
///
/// Representation: a **fold builder** over the merged push-fold `Stream`
/// (src/Effect/Stream.fs). `Build ()` allocates a fresh consumer per run as a
/// pair `(emit, extract)`: `emit` folds one input element (mutating run-local
/// state), `extract` produces the final output. `Stream.run sink stream` drives
/// the stream's emitter with `emit`, then calls `extract`. Because a `Sink`
/// references only `Effect` (never `Stream`), it compiles *before* `Stream`,
/// which lets `Stream.run` keep upstream's exact spelling.
///
/// Short-circuit (`reduceWhile` stopping, `head`, `succeed`) raises the internal
/// `SinkDone` signal from inside `emit`; `Stream.run` catches it at the run
/// boundary (the same defect-as-stop trick `Stream.take` already uses) and then
/// extracts. This means the producer stops early, not just the fold.
///
/// Departures from upstream (Sink.ts, ~2200 lines): leftovers (`L`), chunked
/// `*Array` variants, `Channel` interop (`fromChannel`/`toChannel`), env/error
/// combinators (`provideService`, `catchCause`, `ensuring`, ...), and the
/// `transduce` machinery are deferred. Ported is the consuming core:
/// reduce/reduceWhile/fold, collect/sum/count/drain, forEach, head/last,
/// succeed, and the map/mapInput shape transformers.
type Sink<'In, 'Out, 'E, 'R> =
    internal { Build: unit -> (('In -> Effect<unit, 'E, 'R>) * (unit -> Effect<'Out, 'E, 'R>)) }

/// Internal stop signal for short-circuiting sinks. Raised inside `emit` and
/// caught by `Stream.run`. Mirrors `Stream`'s own private `StreamStop`.
exception internal SinkDone

[<RequireQualifiedAccess>]
module Sink =

    // --- folds ---

    /// Fold inputs while `cont` holds on the running state, then stop. When
    /// `cont state` is `false` the current element is *not* folded (it would be
    /// upstream leftover) and the sink completes with `state`. (Sink.reduceWhile)
    let reduceWhile (initial: 'S) (cont: 'S -> bool) (f: 'S -> 'In -> 'S) : Sink<'In, 'S, 'E, 'R> =
        { Build =
            fun () ->
                let mutable state = initial

                (fun a -> Effect.sync (fun () -> if cont state then state <- f state a else raise SinkDone)),
                (fun () -> Effect.sync (fun () -> state)) }

    /// Fold every input into a single value, starting from `initial`. (Sink.reduce)
    let reduce (initial: 'S) (f: 'S -> 'In -> 'S) : Sink<'In, 'S, 'E, 'R> =
        reduceWhile initial (fun _ -> true) f

    /// Alias of `reduce`: fold every input from `initial`. (Sink.fold, total form)
    let fold (initial: 'S) (f: 'S -> 'In -> 'S) : Sink<'In, 'S, 'E, 'R> = reduce initial f

    // --- collectors ---

    /// Collect every input element into a list. (Sink.collect)
    let collect<'In, 'E, 'R> () : Sink<'In, 'In list, 'E, 'R> =
        { Build =
            fun () ->
                let buffer = System.Collections.Generic.List<'In>()
                (fun a -> Effect.sync (fun () -> buffer.Add a)), (fun () -> Effect.sync (fun () -> List.ofSeq buffer)) }

    /// Sum all integer inputs. (Sink.sum — specialised to `int`; F# lacks a
    /// numeric typeclass, so a generic sum would need an explicit `Combiner`.)
    let sum<'E, 'R> : Sink<int, int, 'E, 'R> = reduce 0 (+)

    /// Count the inputs. (Sink.count)
    let count<'In, 'E, 'R> : Sink<'In, int, 'E, 'R> = reduce 0 (fun n _ -> n + 1)

    /// Consume and discard every input, producing `()`. (Sink.drain)
    let drain<'In, 'E, 'R> : Sink<'In, unit, 'E, 'R> =
        { Build = fun () -> (fun _ -> Effect.succeed ()), (fun () -> Effect.succeed ()) }

    // --- effectful / positional ---

    /// Run `f` on each input for its effect, producing `()`. (Sink.forEach)
    let forEach (f: 'In -> Effect<unit, 'E, 'R>) : Sink<'In, unit, 'E, 'R> =
        { Build = fun () -> f, (fun () -> Effect.succeed ()) }

    /// Take the first input (short-circuits the producer), or `None` if empty. (Sink.head)
    let head<'In, 'E, 'R> () : Sink<'In, 'In option, 'E, 'R> =
        { Build =
            fun () ->
                let mutable result: 'In option = None

                (fun a ->
                    Effect.sync (fun () ->
                        match result with
                        | None ->
                            result <- Some a
                            raise SinkDone
                        | Some _ -> ())),
                (fun () -> Effect.sync (fun () -> result)) }

    /// Take the last input, or `None` if empty. (Sink.last)
    let last<'In, 'E, 'R> () : Sink<'In, 'In option, 'E, 'R> =
        { Build =
            fun () ->
                let mutable result: 'In option = None
                (fun a -> Effect.sync (fun () -> result <- Some a)), (fun () -> Effect.sync (fun () -> result)) }

    /// A sink that ignores its input and immediately succeeds with `a`
    /// (short-circuiting the producer). (Sink.succeed)
    let succeed (a: 'Out) : Sink<'In, 'Out, 'E, 'R> =
        { Build = fun () -> (fun _ -> Effect.sync (fun () -> raise SinkDone)), (fun () -> Effect.succeed a) }

    // --- transformers ---

    /// Transform a sink's output. (Sink.map)
    let map (f: 'A -> 'B) (self: Sink<'In, 'A, 'E, 'R>) : Sink<'In, 'B, 'E, 'R> =
        { Build =
            fun () ->
                let emit, extract = self.Build()
                emit, (fun () -> extract () |> Effect.map f) }

    /// Adapt a sink to a different input type (contravariant). (Sink.mapInput)
    let mapInput (f: 'In2 -> 'In) (self: Sink<'In, 'Out, 'E, 'R>) : Sink<'In2, 'Out, 'E, 'R> =
        { Build =
            fun () ->
                let emit, extract = self.Build()
                (fun b -> emit (f b)), extract }
