namespace Effect

type SqlStreamEmitter<'A, 'E> =
    { Single: 'A -> unit
      Array: 'A list -> unit
      Fail: 'E -> unit
      End: unit -> unit }

type SqlStreamControls =
    { OnPause: unit -> unit
      OnResume: unit -> unit }

[<RequireQualifiedAccess>]
module SqlStream =

    let private start (eff: Effect<'A, 'E, Context>) =
        Async.StartImmediate(
            async {
                let! _ = Effect.runExit Context.empty eff
                return ()
            }
        )

    /// Adapt a callback-style SQL row producer into a stream. The producer can
    /// push single rows or batches, fail the stream, or end it.
    let asyncPauseResume
        (register: SqlStreamEmitter<'A, 'E> -> Effect<SqlStreamControls, 'E, Context>)
        (bufferSize: int)
        : Stream<'A, 'E, Context> =
        { Run =
            fun emit ->
                let _ = bufferSize

                Queue.unbounded ()
                |> Effect.flatMap (fun queue ->
                    let offerAll values =
                        match values with
                        | [] -> ()
                        | _ ->
                            values |> List.iter (fun value -> Queue.offerUnsafe queue value |> ignore)

                    let emitter =
                        { Single = fun item -> offerAll [ item ]
                          Array = offerAll
                          Fail = fun error -> Queue.fail queue (box error) |> Effect.map ignore |> start
                          End = fun () -> Queue.endQueue queue |> Effect.map ignore |> start }

                    register emitter
                    |> Effect.flatMap (fun _ ->

                        let rec consume () =
                            Queue.take queue
                            |> Effect.exit
                            |> Effect.flatMap (function
                                | Success value -> emit value |> Effect.flatMap consume
                                | Failure cause when Cause.failures cause |> List.exists Queue.isDone -> Effect.succeed ()
                                | Failure cause -> Effect.failCause (Cause.map unbox<'E> cause))

                        consume ())) }

    let asyncPauseResumeDefault register =
        asyncPauseResume register 128
