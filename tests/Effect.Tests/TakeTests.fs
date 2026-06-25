module Effect.Tests.TakeTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/src/Take.ts (no upstream test
// file exists; these Facts assert the documented behavior of `toPull`).

[<Fact>]
let ``toPull succeeds with an emitted batch`` () =
    let take: Take<int, string, unit> = Take.chunk [ 1; 2; 3 ]
    Assert.Equal(Success [ 1; 2; 3 ], Effect.runSync () (Take.toPull take))

[<Fact>]
let ``toPull of a single value`` () =
    let take: Take<int, string, unit> = Take.single 7
    Assert.Equal(Success [ 7 ], Effect.runSync () (Take.toPull take))

[<Fact>]
let ``toPull of a successful exit becomes pull completion`` () =
    let take: Take<int, string, string> = Take.endWith "stream ended"

    match Effect.runSync () (Take.toPull take) with
    | Failure cause ->
        Assert.True(Pull.isDoneCause cause)

        match Pull.doneExitFromCause cause with
        | Success leftover -> Assert.Equal("stream ended", unbox<string> leftover)
        | Failure _ -> Assert.True(false, "expected the done leftover")
    | Success _ -> Assert.True(false, "a completion exit must become a done failure")

[<Fact>]
let ``toPull of a failure exit fails ordinarily`` () =
    let take: Take<int, string, unit> = Take.failCause (Cause.fail "boom")

    match Effect.runSync () (Take.toPull take) with
    | Failure cause ->
        Assert.False(Pull.isDoneCause cause)
        // the original typed error survives (boxed into the obj error channel)
        let firstError =
            cause.Reasons
            |> List.tryPick (function
                | Reason.Fail e -> Some e
                | _ -> None)

        Assert.Equal(Some(box "boom"), firstError)
    | Success _ -> Assert.True(false, "a failure exit must fail the pull")
