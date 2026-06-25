module FiberHandleSpec

open Effect
open Effect.Vitest

let private same actual expected = toBe (actual = expected) true

let private blocker (onStop: unit -> unit) : Effect<unit, string, 'R> =
    Effect.sleep 1_000_000 |> Effect.ensuring (Effect.sync onStop)

describe "FiberHandle" (fun () ->
    itEffect "run interrupts the previous fiber" (fun () ->
        let interruptions = ref 0

        effect {
            let! count, hasCurrent =
                Scope.scoped (fun parent ->
                    effect {
                        let! handle = FiberHandle.make parent
                        let! _ = FiberHandle.run handle (blocker (fun () -> interruptions.Value <- interruptions.Value + 1))
                        let! _ = FiberHandle.run handle (blocker (fun () -> interruptions.Value <- interruptions.Value + 1))
                        return interruptions.Value, (FiberHandle.getUnsafe handle).IsSome
                    })

            toBe count 1
            toBe hasCurrent true
        })

    itEffect "clear interrupts the current fiber and empties the handle" (fun () ->
        let interruptions = ref 0

        effect {
            let! hasCurrent =
                Scope.scoped (fun parent ->
                    effect {
                        let! handle = FiberHandle.make parent
                        let! _ = FiberHandle.run handle (blocker (fun () -> interruptions.Value <- interruptions.Value + 1))
                        do! FiberHandle.clear handle
                        return (FiberHandle.getUnsafe handle).IsSome
                    })

            toBe interruptions.Value 1
            toBe hasCurrent false
        })

    itEffect "scope close interrupts the current fiber" (fun () ->
        let interruptions = ref 0

        effect {
            do!
                Scope.scoped (fun parent ->
                    effect {
                        let! handle = FiberHandle.make parent
                        let! _ = FiberHandle.run handle (blocker (fun () -> interruptions.Value <- interruptions.Value + 1))
                        return ()
                    })

            toBe interruptions.Value 1
        })

    itEffect "onlyIfMissing keeps the current fiber and interrupts the rejected start" (fun () ->
        let interruptions = ref 0

        effect {
            let! insideCount, hasCurrent =
                Scope.scoped (fun parent ->
                    effect {
                        let! handle = FiberHandle.make parent
                        let! _ = FiberHandle.run handle (blocker (fun () -> interruptions.Value <- interruptions.Value + 1))
                        let! _ = FiberHandle.runWith handle true (blocker (fun () -> interruptions.Value <- interruptions.Value + 1))
                        return interruptions.Value, (FiberHandle.getUnsafe handle).IsSome
                    })

            toBe insideCount 1
            toBe hasCurrent true
            toBe interruptions.Value 2
        })

    itEffect "get returns None initially and Some after run" (fun () ->
        effect {
            let! before, after =
                Scope.scoped (fun parent ->
                    effect {
                        let! handle = FiberHandle.make parent
                        let! before = FiberHandle.get handle
                        let! _ = FiberHandle.run handle (blocker ignore)
                        let! after = FiberHandle.get handle
                        return before.IsSome, after.IsSome
                    })

            toBe before false
            toBe after true
        })

    itEffect "awaitEmpty waits for the current fiber to complete" (fun () ->
        effect {
            let! hasCurrent =
                Scope.scoped (fun parent ->
                    effect {
                        let! handle = FiberHandle.make parent
                        let! _ = FiberHandle.run handle (Effect.sleep 30)
                        do! FiberHandle.awaitEmpty handle
                        return (FiberHandle.getUnsafe handle).IsSome
                    })

            toBe hasCurrent false
        })

    itEffect "join surfaces the child failure" (fun () ->
        effect {
            let! exit =
                Effect.exit (
                    Scope.scoped (fun parent ->
                        effect {
                            let! handle = FiberHandle.make parent
                            let! fiber = FiberHandle.run handle (Effect.fail "fail")
                            let! _ = Fiber.await fiber
                            return! FiberHandle.join handle
                        })
                )

            match exit with
            | Failure cause -> same (Cause.failures cause) [ "fail" ]
            | Success() -> failwith "expected failure"
        }))
