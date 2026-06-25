module TakeSpec

open Effect
open Effect.Vitest

describe "Take" (fun () ->
    test "toPull succeeds with emitted values" (fun () ->
        let take: Take<int, string, unit> = Take.chunk [ 1; 2; 3 ]
        toEqual (Effect.runSync () (Take.toPull take)) (Success [ 1; 2; 3 ])

        let single: Take<int, string, unit> = Take.single 7
        toEqual (Effect.runSync () (Take.toPull single)) (Success [ 7 ]))

    test "toPull of a successful exit becomes pull completion" (fun () ->
        let take: Take<int, string, string> = Take.endWith "stream ended"

        match Effect.runSync () (Take.toPull take) with
        | Failure cause ->
            toBe (Pull.isDoneCause cause) true

            match Pull.doneExitFromCause cause with
            | Success leftover -> toBe (unbox<string> leftover) "stream ended"
            | Failure _ -> failwith "expected done leftover"
        | Success _ -> failwith "a completion exit must become a done failure")

    test "toPull of a failure exit fails ordinarily" (fun () ->
        let take: Take<int, string, unit> = Take.failCause (Cause.fail "boom")

        match Effect.runSync () (Take.toPull take) with
        | Failure cause ->
            toBe (Pull.isDoneCause cause) false
            let firstError =
                cause.Reasons
                |> List.tryPick (function
                    | Reason.Fail e -> Some e
                    | _ -> None)

            toEqual firstError (Some(box "boom"))
        | Success _ -> failwith "a failure exit must fail the pull"))

