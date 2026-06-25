module PullSpec

open Effect
open Effect.Vitest

let private doneCause: Cause<obj> =
    { Reasons = [ Reason.Fail(box { Leftover = 42 }) ] }

let private errCause: Cause<obj> = { Reasons = [ Reason.Fail(box "boom") ] }
let private emptyCause: Cause<obj> = Cause.empty

let private run (pull: Pull.Pull<int, unit>) =
    Effect.runSync () pull

let private label (pull: Pull.Pull<int, unit>) : string =
    pull
    |> Pull.matchEffect
        (fun a -> (Effect.succeed (sprintf "ok:%d" a): Effect<string, obj, unit>))
        (fun _ -> Effect.succeed "fail")
        (fun l -> Effect.succeed (sprintf "done:%O" l))
    |> Effect.runSync ()
    |> function
        | Success s -> s
        | Failure _ -> "unexpected-failure"

describe "Pull" (fun () ->
    test "isDone detects Done markers" (fun () ->
        toBe (Pull.isDone (box { Leftover = 1 })) true
        toBe (Pull.isDone (box "x")) false
        toBe (Pull.isDone null) false)

    test "isDoneFailure and isDoneCause" (fun () ->
        toBe (Pull.isDoneFailure (Reason.Fail(box { Leftover = 0 }))) true
        toBe (Pull.isDoneFailure (Reason.Fail(box "x"))) false
        toBe (Pull.isDoneFailure (Reason.Die(box "d"))) false
        toBe (Pull.isDoneCause doneCause) true
        toBe (Pull.isDoneCause errCause) false
        toBe (Pull.isDoneCause emptyCause) false)

    test "filter helpers" (fun () ->
        match Pull.filterDone doneCause with
        | Ok marker -> toBe ((marker :?> Done<int>).Leftover) 42
        | Error _ -> failwith "expected Ok"

        toEqual (Pull.filterDone errCause) (Error errCause)
        toEqual (Pull.filterDone emptyCause) (Error emptyCause)

        match Pull.filterDoneLeftover doneCause with
        | Ok leftover -> toBe (unbox<int> leftover) 42
        | Error _ -> failwith "expected Ok"

        toEqual (Pull.filterNoDone errCause) (Ok errCause)
        toEqual (Pull.filterNoDone doneCause) (Error doneCause))

    test "doneExitFromCause maps done to success and others to failure" (fun () ->
        match Pull.doneExitFromCause doneCause with
        | Success v -> toBe (unbox<int> v) 42
        | Failure _ -> failwith "expected Success"

        match Pull.doneExitFromCause errCause with
        | Failure c -> toEqual c errCause
        | Success _ -> failwith "expected Failure")

    test "matchEffect routes success, failure, and done" (fun () ->
        toBe (label (Effect.succeed 5)) "ok:5"
        toBe (label (Effect.failCause errCause)) "fail"
        toBe (label (Pull.halt 99)) "done:99")

    test "halt signals completion with a leftover" (fun () ->
        match run (Pull.halt 7) with
        | Failure cause -> toBe (Pull.isDoneCause cause) true
        | Success _ -> failwith "expected a done failure")

    test "catchDone recovers completion but leaves ordinary failures" (fun () ->
        let recovered: Pull.Pull<int, unit> =
            Pull.halt 7 |> Pull.catchDone (fun l -> Effect.succeed (unbox<int> l + 1))

        toEqual (Effect.runSync () recovered) (Success 8)

        let notRecovered: Pull.Pull<int, unit> =
            Effect.failCause errCause |> Pull.catchDone (fun _ -> Effect.succeed 0)

        match Effect.runSync () notRecovered with
        | Failure cause -> toBe (Pull.isDoneCause cause) false
        | Success _ -> failwith "ordinary failure should not be recovered"))

