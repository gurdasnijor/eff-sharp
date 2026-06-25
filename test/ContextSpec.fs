module ContextSpec

open Effect
open Effect.Vitest

type private Logger = { Log: string -> string }
type private Counter = { Count: int }

let private loggerTag: Tag<Logger> = Tag.make<Logger> "test/Logger"
let private counterTag: Tag<Counter> = Tag.make<Counter> "test/Counter"

describe "Context" (fun () ->
    test "empty has no services" (fun () ->
        toBe (Context.contains loggerTag Context.empty) false
        toEqual (Context.tryGet loggerTag Context.empty) None)

    test "add then get round-trips the typed service" (fun () ->
        let logger = { Log = fun s -> "log:" + s }
        let ctx = Context.empty |> Context.add loggerTag logger
        toBe ((Context.get loggerTag ctx).Log "hi") "log:hi")

    test "contains reflects presence" (fun () ->
        let ctx = Context.make counterTag { Count = 1 }
        toBe (Context.contains counterTag ctx) true
        toBe (Context.contains loggerTag ctx) false)

    test "add replaces an existing service under the same tag" (fun () ->
        let ctx =
            Context.empty
            |> Context.add counterTag { Count = 1 }
            |> Context.add counterTag { Count = 2 }

        toBe ((Context.get counterTag ctx).Count) 2)

    test "unsafeGet raises when a service is absent" (fun () ->
        toThrow (fun () -> Context.unsafeGet loggerTag Context.empty |> ignore))

    test "merge keeps both services, right wins on conflict" (fun () ->
        let a =
            Context.empty
            |> Context.add counterTag { Count = 1 }
            |> Context.add loggerTag { Log = id }

        let b = Context.make counterTag { Count = 9 }
        let merged = Context.merge a b
        toBe ((Context.get counterTag merged).Count) 9
        toBe (Context.contains loggerTag merged) true)

    itEffectIn (Context.make counterTag { Count = 7 })
        "Effect.service reads a provided service"
        (fun () ->
            Effect.service counterTag
            |> Effect.map (fun c -> toBe (sprintf "count=%d" c.Count) "count=7"))

    test "Effect.service on a missing service becomes a defect" (fun () ->
        let program: Effect<int, string, Context> =
            Effect.service counterTag |> Effect.map (fun c -> c.Count)

        match Effect.runSync Context.empty program with
        | Failure cause -> toBe (List.isEmpty (Cause.defects cause)) false
        | Success v -> failwithf "expected a defect, got Success %d" v)

    test "provideService discharges the Context requirement" (fun () ->
        let program: Effect<int, string, Context> =
            Effect.service counterTag |> Effect.map (fun c -> c.Count + 1)

        let provided: Effect<int, string, unit> =
            program |> Effect.provideService counterTag { Count = 41 }

        toEqual (Effect.runSync () provided) (Exit.succeed 42))

    test "provideContext supplies several services at once" (fun () ->
        let program: Effect<string, string, Context> =
            effect {
                let! logger = Effect.service loggerTag
                let! counter = Effect.service counterTag
                return logger.Log(string counter.Count)
            }

        let ctx =
            Context.empty
            |> Context.add loggerTag { Log = fun s -> "n=" + s }
            |> Context.add counterTag { Count = 3 }

        let provided: Effect<string, string, unit> = program |> Effect.provideContext ctx
        toEqual (Effect.runSync () provided) (Exit.succeed "n=3")))
