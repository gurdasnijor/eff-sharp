module RequestSpec

open Effect
open Effect.Vitest

type private GetNameById =
    { Id: int }

    interface Request<string, string, unit>

let private makeCapturingEntry (request: Request<'A, 'E, 'R>) =
    let captured = ref None

    let entry =
        Request.makeEntry request Context.empty false (fun exit -> captured.Value <- Some exit)

    entry, captured

let private run (eff: Effect<unit, 'E, unit>) =
    Effect.runSync () eff

describe "Request" (fun () ->
    test "makeEntry carries its fields" (fun () ->
        let request = { Id = 7 }
        let entry, _ = makeCapturingEntry request
        toEqual entry.Request (request :> Request<string, string, unit>)
        toEqual entry.Context Context.empty
        toBe entry.Uninterruptible false)

    test "succeed completes the entry with a success exit" (fun () ->
        let entry, captured = makeCapturingEntry { Id = 1 }
        run (Request.succeed entry "alice") |> ignore
        toEqual captured.Value (Some(Exit.succeed "alice")))

    test "fail completes the entry with a typed failure" (fun () ->
        let entry, captured = makeCapturingEntry { Id = 2 }
        run (Request.fail entry "Not Found") |> ignore
        toEqual captured.Value (Some(Exit.fail "Not Found")))

    test "failCause completes the entry with a cause" (fun () ->
        let entry, captured = makeCapturingEntry { Id = 3 }
        let cause = Cause.die (box "boom")
        run (Request.failCause entry cause) |> ignore
        toEqual captured.Value (Some(Exit.failCause cause)))

    test "complete completes the entry with a prebuilt exit" (fun () ->
        let entry, captured = makeCapturingEntry { Id = 4 }
        run (Request.complete entry (Exit.succeed "ok")) |> ignore
        toEqual captured.Value (Some(Exit.succeed "ok")))

    test "completeEffect maps a succeeding effect to a success" (fun () ->
        let entry, captured = makeCapturingEntry { Id = 5 }
        run (Request.completeEffect entry (Effect.succeed "bob")) |> ignore
        toEqual captured.Value (Some(Exit.succeed "bob")))

    test "completeEffect maps a failing effect to a typed failure" (fun () ->
        let entry, captured = makeCapturingEntry { Id = 6 }
        run (Request.completeEffect entry (Effect.fail "Not Found")) |> ignore
        toEqual captured.Value (Some(Exit.fail "Not Found")))

    test "the completion effect itself succeeds" (fun () ->
        let entry, _ = makeCapturingEntry { Id = 8 }
        toBe (Exit.isSuccess (run (Request.succeed entry "x"))) true
        toBe (Exit.isSuccess (run (Request.completeEffect entry (Effect.fail "ignored")))) true))
