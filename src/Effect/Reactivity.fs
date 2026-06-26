namespace Effect

type ReactivityKeys = obj list

type Reactivity =
    abstract InvalidateUnsafe: ReactivityKeys -> unit
    abstract RegisterUnsafe: ReactivityKeys -> (unit -> unit) -> (unit -> unit)
    abstract Invalidate<'E> : ReactivityKeys -> Effect<unit, 'E, Context>
    abstract Mutation<'A, 'E> : ReactivityKeys -> Effect<'A, 'E, Context> -> Effect<'A, 'E, Context>
    abstract Query<'A, 'E> : ReactivityKeys -> Effect<'A, 'E, Context> -> Effect<Queue<'A>, 'E, Context>
    abstract Stream<'A, 'E> : ReactivityKeys -> Effect<'A, 'E, Context> -> Stream<'A, 'E, Context>

[<RequireQualifiedAccess>]
module Reactivity =

    let tag: Tag<Reactivity> = Tag.make<Reactivity> "effect/reactivity/Reactivity"

    let private keyHash (key: obj) : string =
        if obj.ReferenceEquals(key, null) then
            "null"
        else
            string key

    let makeUnsafe () : Reactivity =
        let handlers = System.Collections.Generic.Dictionary<string, System.Collections.Generic.HashSet<unit -> unit>>()

        let invalidateUnsafe keys =
            for key in keys do
                match handlers.TryGetValue(keyHash key) with
                | true, set ->
                    for handler in set |> Seq.toArray do
                        handler ()
                | false, _ -> ()

        let registerUnsafe keys handler =
            let resolved = keys |> List.map keyHash

            for key in resolved do
                let set =
                    match handlers.TryGetValue key with
                    | true, existing -> existing
                    | false, _ ->
                        let created = System.Collections.Generic.HashSet<unit -> unit>()
                        handlers.[key] <- created
                        created

                set.Add handler |> ignore

            fun () ->
                for key in resolved do
                    match handlers.TryGetValue key with
                    | true, set ->
                        set.Remove handler |> ignore

                        if set.Count = 0 then
                            handlers.Remove key |> ignore
                    | false, _ -> ()

        let invalidate keys : Effect<unit, 'E, Context> = Effect.sync (fun () -> invalidateUnsafe keys)

        let mutation keys effect = effect |> Effect.tap (fun _ -> invalidate keys)

        let startIntoQueue ctx (queue: Queue<'A>) (eff: Effect<'A, 'E, Context>) =
            Async.StartImmediate(
                async {
                    let! exit = Effect.runExit ctx eff

                    match exit with
                    | Success value -> Queue.offerUnsafe queue value |> ignore
                    | Failure cause ->
                        let! _ = Effect.runExit Context.empty (Queue.failCause queue (Cause.map box cause))
                        ()
                }
            )

        let query keys (effect: Effect<'A, 'E, Context>) : Effect<Queue<'A>, 'E, Context> =
            Effect.environment<Context, 'E>
            |> Effect.flatMap (fun ctx ->
                Queue.unbounded ()
                |> Effect.map (fun queue ->
                    let _cancel = registerUnsafe keys (fun () -> startIntoQueue ctx queue effect)
                    startIntoQueue ctx queue effect
                    queue))

        let stream keys (effect: Effect<'A, 'E, Context>) : Stream<'A, 'E, Context> =
            { Run =
                fun emit ->
                    Effect.environment<Context, 'E>
                    |> Effect.flatMap (fun ctx ->
                        Queue.unbounded ()
                        |> Effect.flatMap (fun queue ->
                            let active = ref true

                            let start () =
                                if active.Value then
                                    startIntoQueue ctx queue effect

                            let cancel = registerUnsafe keys start
                            start ()

                            let rec consume () =
                                Queue.take queue
                                |> Effect.mapError unbox<'E>
                                |> Effect.flatMap (fun value -> emit value |> Effect.flatMap consume)

                            consume ()
                            |> Effect.ensuring (
                                Effect.sync (fun () ->
                                    active.Value <- false
                                    cancel ())
                                |> Effect.flatMap (fun () -> Queue.shutdown queue |> Effect.map ignore)
                            ))) }

        { new Reactivity with
            member _.InvalidateUnsafe keys = invalidateUnsafe keys
            member _.RegisterUnsafe keys handler = registerUnsafe keys handler
            member _.Invalidate keys = invalidate keys
            member _.Mutation keys effect = mutation keys effect
            member _.Query keys effect = query keys effect
            member _.Stream keys effect = stream keys effect }

    let make<'E> : Effect<Reactivity, 'E, Context> = Effect.sync makeUnsafe

    let layer<'E, 'RIn> : Layer<'E, 'RIn> =
        Layer.sync tag makeUnsafe

    let service<'E> : Effect<Reactivity, 'E, Context> =
        Effect.environment<Context, 'E>
        |> Effect.map (fun ctx ->
            match Context.tryGet tag ctx with
            | Some r -> r
            | None -> makeUnsafe ())

    let invalidate keys =
        service |> Effect.flatMap (fun r -> r.Invalidate keys)

    let mutation keys effect =
        service |> Effect.flatMap (fun r -> r.Mutation keys effect)

    let query keys effect =
        service |> Effect.flatMap (fun r -> r.Query keys effect)

    let stream keys effect =
        Stream.unwrap (service |> Effect.map (fun r -> r.Stream keys effect))
