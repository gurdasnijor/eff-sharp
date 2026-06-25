module FiberMapSpec

open Effect
open Effect.Vitest

let private same actual expected = toBe (actual = expected) true
let private voidExit: Exit<obj, obj> = Success(box ())

let private blocker (onStop: unit -> unit) : Effect<unit, string, 'R> =
    Effect.sleep 1_000_000 |> Effect.ensuring (Effect.sync onStop)

describe "FiberMap" (fun () ->
    itEffect "run at an existing key interrupts the previous fiber" (fun () ->
        let interruptions = ref 0

        effect {
            let! count, present, size =
                Scope.scoped (fun parent ->
                    effect {
                        let! map = FiberMap.make parent
                        let! _ = FiberMap.run map "a" (blocker (fun () -> interruptions.Value <- interruptions.Value + 1))
                        let! _ = FiberMap.run map "a" (blocker (fun () -> interruptions.Value <- interruptions.Value + 1))
                        let! present = FiberMap.contains map "a"
                        let! size = FiberMap.size map
                        return interruptions.Value, present, size
                    })

            toBe count 1
            toBe present true
            toBe size 1
        })

    itEffect "different keys coexist" (fun () ->
        effect {
            let! size =
                Scope.scoped (fun parent ->
                    effect {
                        let! map = FiberMap.make parent
                        let! _ = FiberMap.run map "a" (blocker ignore)
                        let! _ = FiberMap.run map "b" (blocker ignore)
                        return! FiberMap.size map
                    })

            toBe size 2
        })

    itEffect "remove interrupts and removes the keyed fiber" (fun () ->
        let interruptions = ref 0

        effect {
            let! present =
                Scope.scoped (fun parent ->
                    effect {
                        let! map = FiberMap.make parent
                        let! _ = FiberMap.run map "a" (blocker (fun () -> interruptions.Value <- interruptions.Value + 1))
                        do! FiberMap.remove map "a"
                        return! FiberMap.contains map "a"
                    })

            toBe interruptions.Value 1
            toBe present false
        })

    itEffect "scope close interrupts all members" (fun () ->
        let interruptions = ref 0

        effect {
            do!
                Scope.scoped (fun parent ->
                    effect {
                        let! map = FiberMap.make parent
                        let! _ = FiberMap.run map "a" (blocker (fun () -> interruptions.Value <- interruptions.Value + 1))
                        let! _ = FiberMap.run map "b" (blocker (fun () -> interruptions.Value <- interruptions.Value + 1))
                        return ()
                    })

            toBe interruptions.Value 2
        })

    itEffect "size drops to zero after the scope closes" (fun () ->
        effect {
            let scope = Scope.make<string, Context> ()
            let! map = FiberMap.make scope
            let! _ = FiberMap.run map "a" (blocker ignore)
            let! _ = FiberMap.run map "b" (blocker ignore)
            let! before = FiberMap.size map
            do! Scope.close scope voidExit
            let! after = FiberMap.size map
            toBe before 2
            toBe after 0
        })

    itEffect "join surfaces the first failure" (fun () ->
        effect {
            let! exit =
                Effect.exit (
                    Scope.scoped (fun parent ->
                        effect {
                            let! map = FiberMap.make parent
                            let! _ = FiberMap.run map "ok" (Effect.succeed ())
                            let! fiber = FiberMap.run map "bad" (Effect.fail "fail")
                            let! _ = Fiber.await fiber
                            return! FiberMap.join map
                        })
                )

            match exit with
            | Failure cause -> same (Cause.failures cause) [ "fail" ]
            | Success() -> failwith "expected failure"
        }))
