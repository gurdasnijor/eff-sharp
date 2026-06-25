module Effect.Tests.ContextTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/Context.test.ts, adapted to
// the F# port's `Tag` + `Context` (typed immutable map) and `Effect.service` DI.

type Logger = { Log: string -> string }
type Counter = { Count: int }

let private loggerTag: Tag<Logger> = Tag.make<Logger> "test/Logger"
let private counterTag: Tag<Counter> = Tag.make<Counter> "test/Counter"

[<Fact>]
let ``empty has no services`` () =
    Assert.False(Context.contains loggerTag Context.empty)
    Assert.Equal<Logger option>(None, Context.tryGet loggerTag Context.empty)

[<Fact>]
let ``add then get round-trips the typed service`` () =
    let logger = { Log = fun s -> "log:" + s }
    let ctx = Context.empty |> Context.add loggerTag logger
    Assert.Equal("log:hi", (Context.get loggerTag ctx).Log "hi")

[<Fact>]
let ``contains reflects presence`` () =
    let ctx = Context.make counterTag { Count = 1 }
    Assert.True(Context.contains counterTag ctx)
    Assert.False(Context.contains loggerTag ctx)

[<Fact>]
let ``add replaces an existing service under the same tag`` () =
    let ctx =
        Context.empty
        |> Context.add counterTag { Count = 1 }
        |> Context.add counterTag { Count = 2 }

    Assert.Equal(2, (Context.get counterTag ctx).Count)

[<Fact>]
let ``unsafeGet raises when a service is absent`` () =
    Assert.ThrowsAny<System.Exception>(fun () -> Context.unsafeGet loggerTag Context.empty |> ignore)

[<Fact>]
let ``merge keeps both services, right wins on conflict`` () =
    let a =
        Context.empty
        |> Context.add counterTag { Count = 1 }
        |> Context.add loggerTag { Log = id }

    let b = Context.make counterTag { Count = 9 }
    let merged = Context.merge a b
    Assert.Equal(9, (Context.get counterTag merged).Count)
    Assert.True(Context.contains loggerTag merged)

// ----------------------------------------------------------------------------
// Effect.service / provideService / provideContext (DI wired into the core)
// ----------------------------------------------------------------------------

[<Fact>]
let ``Effect.service reads a provided service`` () =
    let program: Effect<string, string, Context> =
        Effect.service counterTag |> Effect.map (fun c -> sprintf "count=%d" c.Count)

    let exit = Effect.runSync (Context.make counterTag { Count = 7 }) program
    Assert.Equal<Exit<string, string>>(Exit.succeed "count=7", exit)

[<Fact>]
let ``Effect.service on a missing service becomes a defect`` () =
    let program: Effect<int, string, Context> =
        Effect.service counterTag |> Effect.map (fun c -> c.Count)

    match Effect.runSync Context.empty program with
    | Failure cause -> Assert.NotEmpty(Cause.defects cause)
    | Success v -> Assert.Fail(sprintf "expected a defect, got Success %d" v)

[<Fact>]
let ``provideService discharges the Context requirement`` () =
    let program: Effect<int, string, Context> =
        Effect.service counterTag |> Effect.map (fun c -> c.Count + 1)

    let provided: Effect<int, string, unit> =
        program |> Effect.provideService counterTag { Count = 41 }

    Assert.Equal<Exit<int, string>>(Exit.succeed 42, Effect.runSync () provided)

[<Fact>]
let ``provideContext supplies several services at once`` () =
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
    Assert.Equal<Exit<string, string>>(Exit.succeed "n=3", Effect.runSync () provided)
