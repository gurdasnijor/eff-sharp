module Effect.Tests.RequestTests

open Xunit
open Effect

// Adapted from repos/effect-smol/packages/effect/test/Request.test.ts.
//
// The upstream test drives requests end-to-end through `RequestResolver` /
// `Effect.request` / batching, none of which are ported yet. These tests cover
// the portable surface: the `Entry` data type and the completion combinators
// (`complete` / `succeed` / `fail` / `failCause` / `completeEffect`), which are
// what a resolver calls to finish a pending request.

// A concrete request implementing the marker interface.
type private GetNameById =
    { Id: int }

    interface Request<string, string, unit>

let private makeCapturingEntry (request: Request<'A, 'E, 'R>) =
    let captured = ref None

    let entry =
        Request.makeEntry request Context.empty false (fun exit -> captured.Value <- Some exit)

    entry, captured

let private run (eff: Effect<unit, 'E, unit>) = Effect.runSync () eff

[<Fact>]
let ``makeEntry carries its fields`` () =
    let request = { Id = 7 }
    let entry, _ = makeCapturingEntry request
    Assert.Equal<Request<string, string, unit>>(request :> Request<string, string, unit>, entry.Request)
    Assert.Equal(Context.empty, entry.Context)
    Assert.False(entry.Uninterruptible)

[<Fact>]
let ``succeed completes the entry with a success exit`` () =
    let entry, captured = makeCapturingEntry { Id = 1 }
    run (Request.succeed entry "alice") |> ignore
    Assert.Equal(Some(Exit.succeed "alice"), captured.Value)

[<Fact>]
let ``fail completes the entry with a typed failure`` () =
    let entry, captured = makeCapturingEntry { Id = 2 }
    run (Request.fail entry "Not Found") |> ignore
    Assert.Equal(Some(Exit.fail "Not Found"), captured.Value)

[<Fact>]
let ``failCause completes the entry with a cause`` () =
    let entry, captured = makeCapturingEntry { Id = 3 }
    let cause = Cause.die (box "boom")
    run (Request.failCause entry cause) |> ignore
    Assert.Equal(Some(Exit.failCause cause), captured.Value)

[<Fact>]
let ``complete completes the entry with a prebuilt exit`` () =
    let entry, captured = makeCapturingEntry { Id = 4 }
    run (Request.complete entry (Exit.succeed "ok")) |> ignore
    Assert.Equal(Some(Exit.succeed "ok"), captured.Value)

[<Fact>]
let ``completeEffect maps a succeeding effect to a success`` () =
    let entry, captured = makeCapturingEntry { Id = 5 }
    run (Request.completeEffect entry (Effect.succeed "bob")) |> ignore
    Assert.Equal(Some(Exit.succeed "bob"), captured.Value)

[<Fact>]
let ``completeEffect maps a failing effect to a typed failure`` () =
    let entry, captured = makeCapturingEntry { Id = 6 }
    run (Request.completeEffect entry (Effect.fail "Not Found")) |> ignore
    Assert.Equal(Some(Exit.fail "Not Found"), captured.Value)

[<Fact>]
let ``the completion effect itself succeeds`` () =
    let entry, _ = makeCapturingEntry { Id = 8 }
    Assert.True(Exit.isSuccess (run (Request.succeed entry "x")))
    Assert.True(Exit.isSuccess (run (Request.completeEffect entry (Effect.fail "ignored"))))
