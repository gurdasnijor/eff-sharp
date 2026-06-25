module SynchronizedRefSpec

open Effect
open Effect.Vitest

let private current = "value"
let private update = "new value"
let private failure = "failure"

type private State =
    | Active
    | Changed
    | Closed

let private same actual expected = toBe (actual = expected) true

describe "SynchronizedRef" (fun () ->
    itEffect "get" (fun () ->
        effect {
            let! ref = SynchronizedRef.make current
            let! result = SynchronizedRef.get ref
            toBe result current
        })

    itEffect "getAndUpdateEffect - happy path" (fun () ->
        effect {
            let! ref = SynchronizedRef.make current
            let! previous = SynchronizedRef.getAndUpdateEffect ref (fun _ -> Effect.succeed update)
            let! result = SynchronizedRef.get ref
            toBe previous current
            toBe result update
        })

    itEffect "getAndUpdateEffect - with failure" (fun () ->
        effect {
            let! ref = SynchronizedRef.make current
            let! exit = Effect.exit (SynchronizedRef.getAndUpdateEffect ref (fun _ -> Effect.fail failure))
            same exit (Exit.fail failure)
        })

    itEffect "getAndUpdateSomeEffect - happy path" (fun () ->
        effect {
            let! ref = SynchronizedRef.make Active
            let! previous =
                SynchronizedRef.getAndUpdateSomeEffect ref (function
                    | Closed -> Effect.succeed (Some Changed)
                    | _ -> Effect.succeed None)

            let! result = SynchronizedRef.get ref
            same previous Active
            same result Active
        })

    itEffect "getAndUpdateSomeEffect - twice" (fun () ->
        effect {
            let! ref = SynchronizedRef.make Active
            let! first =
                SynchronizedRef.getAndUpdateSomeEffect ref (function
                    | Active -> Effect.succeed (Some Changed)
                    | _ -> Effect.succeed None)

            let! second =
                SynchronizedRef.getAndUpdateSomeEffect ref (function
                    | Closed -> Effect.succeed (Some Active)
                    | Changed -> Effect.succeed (Some Closed)
                    | _ -> Effect.succeed None)

            let! result = SynchronizedRef.get ref
            same first Active
            same second Changed
            same result Closed
        })

    itEffect "getAndUpdateSomeEffect - with failure" (fun () ->
        effect {
            let! ref = SynchronizedRef.make Active
            let! exit =
                Effect.exit (
                    SynchronizedRef.getAndUpdateSomeEffect ref (function
                        | Active -> Effect.fail failure
                        | _ -> Effect.succeed None)
                )

            same exit (Exit.fail failure)
        })

    itEffect "updateAndGetEffect stores and returns the new value" (fun () ->
        effect {
            let! ref = SynchronizedRef.make Active
            let! result = SynchronizedRef.updateAndGetEffect ref (fun _ -> Effect.succeed Closed)
            let! current = SynchronizedRef.get ref
            same result Closed
            same current Closed
        })

    itEffect "modifyEffect returns a result while storing the new value" (fun () ->
        effect {
            let! ref = SynchronizedRef.make current
            let! result = SynchronizedRef.modifyEffect ref (fun _ -> Effect.succeed ("hello", update))
            let! current = SynchronizedRef.get ref
            toBe result "hello"
            toBe current update
        })

    itEffect "updateSomeAndGetEffect returns current when no transition matches" (fun () ->
        effect {
            let! ref = SynchronizedRef.make Active
            let! result =
                SynchronizedRef.updateSomeAndGetEffect ref (function
                    | Closed -> Effect.succeed (Some Changed)
                    | _ -> Effect.succeed None)

            same result Active
        })

    itEffect "pure set update and modifySome" (fun () ->
        effect {
            let! ref = SynchronizedRef.make current
            do! SynchronizedRef.set ref update
            let! afterSet = SynchronizedRef.get ref
            toBe afterSet update

            let! state = SynchronizedRef.make Active
            do! SynchronizedRef.update state (fun _ -> Changed)
            let! afterUpdate = SynchronizedRef.get state
            same afterUpdate Changed

            let! modified =
                SynchronizedRef.modifySome state (function
                    | Changed -> ("closed", Some Closed)
                    | _ -> ("unchanged", None))

            let! afterModify = SynchronizedRef.get state
            toBe modified "closed"
            same afterModify Closed
        }))
