namespace Effect

/// Slice: wave-6. `Channel` — MINIMAL output-writer core (port of Channel.ts).
///
/// ⚠️ DEFERRAL: upstream `Channel.ts` is ~8600 lines — a pull-based, chunked
/// primitive that reads typed *input*, writes *output*, terminates with a *done*
/// value, supports concurrency/backpressure, and underpins both `Stream` and
/// `Sink`. A faithful port is far beyond one pass and is **deferred**.
///
/// What is ported here is the slice that `Stream`/`Sink` interop actually needs
/// in this push-fold port: the **output half** of a channel — a writer of
/// elements with no input and a `unit` done value. In this encoding a `Channel`
/// is literally shape-identical to a `Stream` (`emit -> Effect<unit>`), so the
/// bridges `fromStream`/`toStream` are zero-cost and total. Input consumption,
/// the `Done`/`Leftover` type parameters, chunking, and combinators like
/// `pipeTo`/`mergeAll`/`concatMap` are NOT ported — use `Stream`/`Sink`.
type Channel<'Out, 'E, 'R> =
    internal { Run: ('Out -> Effect<unit, 'E, 'R>) -> Effect<unit, 'E, 'R> }

[<RequireQualifiedAccess>]
module Channel =

    /// A channel that writes nothing. (Channel.empty)
    let empty<'Out, 'E, 'R> : Channel<'Out, 'E, 'R> = { Run = fun _ -> Effect.succeed () }

    /// Write a single output element. (Channel.write)
    let write (value: 'Out) : Channel<'Out, 'E, 'R> = { Run = fun emit -> emit value }

    /// Write each element of a collection. (Channel.writeAll)
    let writeAll (values: seq<'Out>) : Channel<'Out, 'E, 'R> =
        { Run = fun emit -> Effect.forEach values emit |> Effect.map ignore }

    /// Transform every written element. (Channel.map)
    let map (f: 'Out -> 'Out2) (self: Channel<'Out, 'E, 'R>) : Channel<'Out2, 'E, 'R> =
        { Run = fun emit -> self.Run(fun o -> emit (f o)) }

    /// Sequence two channels: all of `a`'s output, then all of `b`'s. (Channel.concat-ish)
    let concat (a: Channel<'Out, 'E, 'R>) (b: Channel<'Out, 'E, 'R>) : Channel<'Out, 'E, 'R> =
        { Run = fun emit -> a.Run emit |> Effect.flatMap (fun () -> b.Run emit) }

    // --- Stream interop (the reason this minimal core exists) ---

    /// View a `Stream` as a `Channel` of its outputs (zero-cost). (Channel.fromStream-ish)
    let fromStream (self: Stream<'Out, 'E, 'R>) : Channel<'Out, 'E, 'R> = { Run = self.Run }

    /// View an output `Channel` as a `Stream` (zero-cost). (Channel.toStream-ish)
    let toStream (self: Channel<'Out, 'E, 'R>) : Stream<'Out, 'E, 'R> = { Run = self.Run }

    // --- runners ---

    /// Run the channel, discarding output. (Channel.runDrain)
    let runDrain (self: Channel<'Out, 'E, 'R>) : Effect<unit, 'E, 'R> = self.Run(fun _ -> Effect.succeed ())

    /// Run the channel, collecting output into a list. (Channel.runCollect)
    let runCollect (self: Channel<'Out, 'E, 'R>) : Effect<'Out list, 'E, 'R> =
        let buffer = System.Collections.Generic.List<'Out>()

        self.Run(fun o -> Effect.sync (fun () -> buffer.Add o))
        |> Effect.map (fun () -> List.ofSeq buffer)
