module FiberSetSpec

open Effect
open Effect.Vitest

let private same actual expected = toBe (actual = expected) true
let private voidExit: Exit<obj, obj> = Success(box ())

let private blocker (onStop: unit -> unit) : Effect<unit, string, 'R> =
    Effect.sleep 1_000_000 |> Effect.ensuring (Effect.sync onStop)

describe "FiberSet" (fun () ->
    itEffect "scope close interrupts all members" (fun () ->
        let interruptions = ref 0

        effect {
            do!
                Scope.scoped (fun parent ->
                    effect {
                        let! set = FiberSet.make parent
                        let! _ = FiberSet.run set (blocker (fun () -> interruptions.Value <- interruptions.Value + 1))
                        let! _ = FiberSet.run set (blocker (fun () -> interruptions.Value <- interruptions.Value + 1))
                        let! _ = FiberSet.run set (blocker (fun () -> interruptions.Value <- interruptions.Value + 1))
                        return ()
                    })

            toBe interruptions.Value 3
        })

    itEffect "size reflects live fibers and drops to zero after close" (fun () ->
        effect {
            let scope = Scope.make<string, Context> ()
            let! set = FiberSet.make scope
            let! _ = FiberSet.run set (blocker ignore)
            let! _ = FiberSet.run set (blocker ignore)
            let! before = FiberSet.size set
            do! Scope.close scope voidExit
            let! after = FiberSet.size set
            toBe before 2
            toBe after 0
        })

    itEffect "clear interrupts all members and empties the set" (fun () ->
        let interruptions = ref 0

        effect {
            let! remaining =
                Scope.scoped (fun parent ->
                    effect {
                        let! set = FiberSet.make parent
                        let! _ = FiberSet.run set (blocker (fun () -> interruptions.Value <- interruptions.Value + 1))
                        let! _ = FiberSet.run set (blocker (fun () -> interruptions.Value <- interruptions.Value + 1))
                        do! FiberSet.clear set
                        return! FiberSet.size set
                    })

            toBe interruptions.Value 2
            toBe remaining 0
        })

    itEffect "join surfaces the first failure" (fun () ->
        effect {
            let! exit =
                Effect.exit (
                    Scope.scoped (fun parent ->
                        effect {
                            let! set = FiberSet.make parent
                            let! _ = FiberSet.run set (Effect.succeed ())
                            let! _ = FiberSet.run set (Effect.succeed ())
                            let! fiber = FiberSet.run set (Effect.fail "fail")
                            let! _ = Fiber.await fiber
                            return! FiberSet.join set
                        })
                )

            match exit with
            | Failure cause -> same (Cause.failures cause) [ "fail" ]
            | Success() -> failwith "expected failure"
        })

    itEffect "addUnsafe stores a forked fiber" (fun () ->
        effect {
            let! size =
                Scope.scoped (fun parent ->
                    effect {
                        let! set = FiberSet.make parent
                        let! fiber = Effect.fork (blocker ignore)
                        do! Effect.sync (fun () -> FiberSet.addUnsafe set fiber)
                        return! FiberSet.size set
                    })

            toBe size 1
        }))
