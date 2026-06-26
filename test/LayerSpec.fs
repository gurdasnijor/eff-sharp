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

    test "provide preserves ambient context and lets the layer win on conflicts" (fun () ->
        let dbTag = Tag.make<DbConfig> "ambient-db"
        let cacheTag = Tag.make<CacheConfig> "ambient-cache"

        let layer = Layer.succeed cacheTag { Size = 100 }

        let program: Effect<string, string, Context> =
            effect {
                let! db = Effect.service dbTag
                let! cache = Effect.service cacheTag
                return sprintf "%s/%d" db.Url cache.Size
            }

        let ambient =
            Context.empty
            |> Context.add dbTag { Url = "pg://ambient" }
            |> Context.add cacheTag { Size = 1 }

        toEqual (Effect.runSync ambient (Layer.provide layer program)) (Exit.succeed "pg://ambient/100"))

    test "reference layers override defaults" (fun () ->
        let reference = Reference.make "layer/reference" 1
        let layer = Layer.succeedReference reference 42
        let program: Effect<int, string, Context> = Effect.serviceReference reference

        toEqual (Effect.runSync () (Layer.provide layer program)) (Exit.succeed 42))

    test "effectReference layers can depend on ambient context" (fun () ->
        let baseTag = Tag.make<int> "layer/base"
        let reference = Reference.make "layer/effect-reference" 0

        let layer =
            Layer.effectReference
                reference
                (Effect.service baseTag |> Effect.map (fun n -> n + 1))

        let program: Effect<int, string, Context> = Effect.serviceReference reference
        let ambient = Context.make baseTag 10

        toEqual (Effect.runSync ambient (Layer.provide layer program)) (Exit.succeed 11))

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
