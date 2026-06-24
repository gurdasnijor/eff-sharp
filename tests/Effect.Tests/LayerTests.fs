module Effect.Tests.LayerTests

open Xunit
open Effect

type DbConfig = { Url: string }
type CacheConfig = { Size: int }

[<Fact>]
let ``provide a merged layer and read both services`` () =
    let dbTag = Tag.make<DbConfig> "db"
    let cacheTag = Tag.make<CacheConfig> "cache"

    let layer =
        Layer.merge (Layer.succeed dbTag { Url = "pg://" }) (Layer.effect cacheTag (Effect.succeed { Size = 100 }))

    let program: Effect<string, string, Context> =
        effect {
            let! db = Effect.service dbTag
            let! cache = Effect.service cacheTag
            return sprintf "%s/%d" db.Url cache.Size
        }

    Assert.Equal<Exit<string, string>>(Exit.succeed "pg:///100", Effect.runSync () (Layer.provide layer program))

[<Fact>]
let ``mergeAll combines several layers`` () =
    let a = Tag.make<int> "a"
    let b = Tag.make<int> "b"
    let layer = Layer.mergeAll [ Layer.succeed a 1; Layer.succeed b 2 ]

    let program: Effect<int, string, Context> =
        effect {
            let! x = Effect.service a
            let! y = Effect.service b
            return x + y
        }

    Assert.Equal<Exit<int, string>>(Exit.succeed 3, Effect.runSync () (Layer.provide layer program))

[<Fact>]
let ``a scoped layer's finalizer runs when the providing scope closes`` () =
    let log = System.Collections.Generic.List<string>()
    let tag = Tag.make<int> "resource"

    let layer =
        Layer.scoped tag (fun scope ->
            effect {
                do! Scope.addFinalizer scope (Effect.sync (fun () -> log.Add "released"))
                return 42
            })

    let program: Effect<int, string, Context> = Effect.service tag
    let result = Effect.runSync () (Layer.provide layer program)
    Assert.Equal<Exit<int, string>>(Exit.succeed 42, result)
    Assert.Equal<string>("released", System.String.Join(",", log)) // released after the consuming effect
