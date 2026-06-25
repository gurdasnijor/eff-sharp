module LayerSpec

open Effect
open Effect.Vitest

type private DbConfig = { Url: string }
type private CacheConfig = { Size: int }

describe "Layer" (fun () ->
    test "provide a merged layer and read both services" (fun () ->
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

        toBe (Effect.runSync () (Layer.provide layer program) = Exit.succeed "pg:///100") true)

    test "mergeAll combines several layers" (fun () ->
        let a = Tag.make<int> "a"
        let b = Tag.make<int> "b"
        let layer = Layer.mergeAll [ Layer.succeed a 1; Layer.succeed b 2 ]

        let program: Effect<int, string, Context> =
            effect {
                let! x = Effect.service a
                let! y = Effect.service b
                return x + y
            }

        toBe (Effect.runSync () (Layer.provide layer program) = Exit.succeed 3) true)

    test "a scoped layer's finalizer runs when the providing scope closes" (fun () ->
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
        toBe (result = Exit.succeed 42) true
        toBe (System.String.Join(",", log)) "released"))
