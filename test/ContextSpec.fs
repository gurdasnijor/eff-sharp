module ContextSpec

open Effect
open Effect.Vitest

type private Logger = { Log: string -> string }
type private Counter = { Count: int }

let private loggerTag: Tag<Logger> = Tag.make<Logger> "test/Logger"
let private counterTag: Tag<Counter> = Tag.make<Counter> "test/Counter"
let private counterReference: Reference<Counter> = Reference.make "test/CounterReference" { Count = 10 }

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

    test "references read their default until overridden" (fun () ->
        toBe ((Context.getReference counterReference Context.empty).Count) 10
        toBe (Context.containsReference counterReference Context.empty) false

        let ctx = Context.makeReference counterReference { Count = 99 }
        toBe ((Context.getReference counterReference ctx).Count) 99
        toBe (Context.containsReference counterReference ctx) true)

    itEffectIn (Context.make counterTag { Count = 7 })
        "Effect.service reads a provided service"
        (fun () ->
            Effect.service counterTag
            |> Effect.map (fun c -> toBe (sprintf "count=%d" c.Count) "count=7"))

    itEffectIn Context.empty
        "Effect.serviceReference reads the default when absent"
        (fun () ->
            Effect.serviceReference counterReference
            |> Effect.map (fun c -> toBe c.Count 10))

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

    test "provideService preserves the ambient Context when present" (fun () ->
        let program: Effect<string, string, Context> =
            effect {
                let! logger = Effect.service loggerTag
                let! counter = Effect.service counterTag
                return logger.Log(string counter.Count)
            }

        let ctx = Context.make loggerTag { Log = fun s -> s + "!" }
        let provided = program |> Effect.provideService counterTag { Count = 5 }
        toEqual (Effect.runSync ctx provided) (Exit.succeed "5!"))

    test "provideServiceReference overrides a reference and preserves the ambient Context" (fun () ->
        let program: Effect<string, string, Context> =
            effect {
                let! logger = Effect.service loggerTag
                let! counter = Effect.serviceReference counterReference
                return logger.Log(string counter.Count)
            }

        let ctx = Context.make loggerTag { Log = fun s -> "count=" + s }
        let provided = program |> Effect.provideServiceReference counterReference { Count = 77 }
        toEqual (Effect.runSync ctx provided) (Exit.succeed "count=77"))

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
