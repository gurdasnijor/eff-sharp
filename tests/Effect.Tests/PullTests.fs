module Effect.Tests.PullTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/src/Pull.ts (no upstream test
// file exists; these Facts assert the documented behavior). A Pull's error
// channel is `obj`; a `Done<'L>` marker in a `Fail` reason signals completion.

let private doneCause: Cause<obj> =
    { Reasons = [ Reason.Fail(box { Leftover = 42 }) ] }

let private errCause: Cause<obj> = { Reasons = [ Reason.Fail(box "boom") ] }
let private emptyCause: Cause<obj> = Cause.empty

[<Fact>]
let ``isDone detects Done markers`` () =
    Assert.True(Pull.isDone (box { Leftover = 1 }))
    Assert.False(Pull.isDone (box "x"))
    Assert.False(Pull.isDone null)

[<Fact>]
let ``isDoneFailure and isDoneCause`` () =
    Assert.True(Pull.isDoneFailure (Reason.Fail(box { Leftover = 0 })))
    Assert.False(Pull.isDoneFailure (Reason.Fail(box "x")))
    Assert.False(Pull.isDoneFailure (Reason.Die(box "d")))
    Assert.True(Pull.isDoneCause doneCause)
    Assert.False(Pull.isDoneCause errCause)
    Assert.False(Pull.isDoneCause emptyCause)

[<Fact>]
let ``filterDone extracts the Done marker or fails with the cause`` () =
    match Pull.filterDone doneCause with
    | Ok marker -> Assert.Equal(42, (marker :?> Done<int>).Leftover)
    | Error _ -> Assert.True(false, "expected Ok")

    Assert.Equal(Error errCause, Pull.filterDone errCause)
    Assert.Equal(Error emptyCause, Pull.filterDone emptyCause)

[<Fact>]
let ``filterDoneLeftover extracts the leftover value`` () =
    match Pull.filterDoneLeftover doneCause with
    | Ok leftover -> Assert.Equal(42, unbox<int> leftover)
    | Error _ -> Assert.True(false, "expected Ok")

    Assert.Equal(Error errCause, Pull.filterDoneLeftover errCause)

[<Fact>]
let ``filterNoDone keeps only non-done causes`` () =
    Assert.Equal(Ok errCause, Pull.filterNoDone errCause)
    Assert.Equal(Error doneCause, Pull.filterNoDone doneCause)

[<Fact>]
let ``doneExitFromCause maps done to success and others to failure`` () =
    match Pull.doneExitFromCause doneCause with
    | Success v -> Assert.Equal(42, unbox<int> v)
    | Failure _ -> Assert.True(false, "expected Success")

    match Pull.doneExitFromCause errCause with
    | Failure c -> Assert.Equal(errCause, c)
    | Success _ -> Assert.True(false, "expected Failure")

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

[<Fact>]
let ``matchEffect routes success, failure, and done`` () =
    Assert.Equal("ok:5", label (Effect.succeed 5))
    Assert.Equal("fail", label (Effect.failCause errCause))
    Assert.Equal("done:99", label (Pull.halt 99))

[<Fact>]
let ``halt signals completion with a leftover`` () =
    match Effect.runSync () (Pull.halt 7: Pull.Pull<int, unit>) with
    | Failure cause -> Assert.True(Pull.isDoneCause cause)
    | Success _ -> Assert.True(false, "expected a done failure")

[<Fact>]
let ``catchDone recovers completion but leaves ordinary failures`` () =
    let recovered: Pull.Pull<int, unit> =
        Pull.halt 7 |> Pull.catchDone (fun l -> Effect.succeed (unbox<int> l + 1))

    Assert.Equal(Success 8, Effect.runSync () recovered)

    let notRecovered: Pull.Pull<int, unit> =
        Effect.failCause errCause |> Pull.catchDone (fun _ -> Effect.succeed 0)

    match Effect.runSync () notRecovered with
    | Failure cause -> Assert.False(Pull.isDoneCause cause)
    | Success _ -> Assert.True(false, "ordinary failure should not be recovered")
