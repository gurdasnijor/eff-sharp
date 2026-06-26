namespace Effect

type ReactivityKeys = obj list

type Reactivity =
    abstract InvalidateUnsafe: ReactivityKeys -> unit
    abstract RegisterUnsafe: ReactivityKeys -> (unit -> unit) -> (unit -> unit)
    abstract Invalidate<'E> : ReactivityKeys -> Effect<unit, 'E, Context>
    abstract Mutation<'A, 'E> : ReactivityKeys -> Effect<'A, 'E, Context> -> Effect<'A, 'E, Context>
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

        let stream keys (effect: Effect<'A, 'E, Context>) : Stream<'A, 'E, Context> =
            { Run =
                fun emit ->
                    // v1: run once, and rerun for invalidations while the stream is active.
                    let active = ref true

                    let runOnce =
                        effect |> Effect.flatMap emit

                    let cancel =
                        registerUnsafe keys (fun () ->
                            if active.Value then
                                Effect.runSync Context.empty runOnce |> ignore)

                    runOnce
                    |> Effect.ensuring (
                        Effect.sync (fun () ->
                            active.Value <- false
                            cancel ())
                    ) }

        { new Reactivity with
            member _.InvalidateUnsafe keys = invalidateUnsafe keys
            member _.RegisterUnsafe keys handler = registerUnsafe keys handler
            member _.Invalidate keys = invalidate keys
            member _.Mutation keys effect = mutation keys effect
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

    let stream keys effect =
        Stream.unwrap (service |> Effect.map (fun r -> r.Stream keys effect))
