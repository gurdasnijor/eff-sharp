module Effect.Tests.RequestResolverTests

open Xunit
open Effect

// Adapted from repos/effect-smol/packages/effect/test/Request.test.ts (the
// resolver-driving cases). The upstream suite runs requests through
// `Effect.request` + the fiber-runtime batching window, which is not ported; these
// drive the native `RequestResolver.resolveAll`/`resolve` driver, which makes one
// entry per request, groups by batch key, chunks by `batchN`, and runs `runAll`.

// --- concrete requests (records implementing the marker interface) ---

type private GetSquare =
    { Value: int }
    interface Request<int, string, unit>

type private GetNameById =
    { Id: int }
    interface Request<string, string, unit>

let private square (e: Entry<int, string, unit>) =
    let v = (e.Request :?> GetSquare).Value
    v * v
let private nameId (e: Entry<string, string, unit>) = (e.Request :?> GetNameById).Id

let private names = Map [ for i in 1..26 -> i, string (char (96 + i)) ] // 1->"a" .. 26->"z"

let private run (eff: Effect<'a, 'e, unit>) : 'a =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure cause -> failwithf "unexpected failure: %s" (Cause.render cause)

// --- constructors ---

[<Fact>]
let ``fromFunction resolves each request with a pure function`` () =
    let resolver = RequestResolver.fromFunction square
    let requests: Request<int, string, unit> list = [ { Value = 2 }; { Value = 5 } ]
    Assert.Equal<Exit<int, string> list>([ Exit.succeed 4; Exit.succeed 25 ], run (RequestResolver.resolveAll resolver requests))

[<Fact>]
let ``fromFunctionBatched resolves a whole batch positionally`` () =
    let resolver =
        RequestResolver.fromFunctionBatched (fun entries -> entries |> List.map (fun e -> square e) |> List.toSeq)

    let requests: Request<int, string, unit> list = [ { Value = 1 }; { Value = 2 }; { Value = 3 } ]
    Assert.Equal<int list>([ 1; 4; 9 ], run (RequestResolver.resolveAll resolver requests) |> List.map (Exit.matchExit (fun _ -> 0) id))

[<Fact>]
let ``resolve folds a single request's exit into the effect`` () =
    let resolver = RequestResolver.fromFunction square
    Assert.Equal(49, run (RequestResolver.resolve resolver { Value = 7 }))

// --- make + manual runAll, counting invocations and batched entries ---

[<Fact>]
let ``make batches all requests into one runAll invocation`` () =
    let invocations = ref 0
    let seen = ref 0

    let resolver: RequestResolver<string, string, unit> =
        RequestResolver.make (fun entries ->
            Effect.sync (fun () ->
                incr invocations
                seen.Value <- seen.Value + List.length entries
                entries |> List.iter (fun e -> e.CompleteUnsafe(Exit.succeed (names.[nameId e])))))

    let requests: Request<string, string, unit> list = [ { Id = 1 }; { Id = 2 }; { Id = 3 } ]
    let results = run (RequestResolver.resolveAll resolver requests)
    Assert.Equal<Exit<string, string> list>([ Exit.succeed "a"; Exit.succeed "b"; Exit.succeed "c" ], results)
    Assert.Equal(1, invocations.Value)
    Assert.Equal(3, seen.Value)

[<Fact>]
let ``identical requests are batched but kept as individual entries`` () =
    // Upstream "preserves individual & identical requests": 2 entries, 1 invocation.
    let invocations = ref 0
    let seen = ref 0

    let resolver: RequestResolver<string, string, unit> =
        RequestResolver.make (fun entries ->
            Effect.sync (fun () ->
                incr invocations
                seen.Value <- seen.Value + List.length entries
                entries |> List.iter (fun e -> e.CompleteUnsafe(Exit.succeed (names.[nameId e])))))

    let requests: Request<string, string, unit> list = [ { Id = 1 }; { Id = 1 } ]
    let results = run (RequestResolver.resolveAll resolver requests)
    Assert.Equal<Exit<string, string> list>([ Exit.succeed "a"; Exit.succeed "a" ], results)
    Assert.Equal(1, invocations.Value)
    Assert.Equal(2, seen.Value)

// --- fromEffect (success + typed failure) ---

[<Fact>]
let ``fromEffect completes entries from an effectful function`` () =
    let resolver =
        RequestResolver.fromEffect (fun (e: Entry<string, string, unit>) ->
            match Map.tryFind (nameId e) names with
            | Some name -> Effect.succeed name
            | None -> Effect.fail "Not Found")

    let requests: Request<string, string, unit> list = [ { Id = 1 }; { Id = 99 } ]
    Assert.Equal<Exit<string, string> list>([ Exit.succeed "a"; Exit.fail "Not Found" ], run (RequestResolver.resolveAll resolver requests))

// --- batchN bounds batch size ---

[<Fact>]
let ``batchN splits a group into bounded batches`` () =
    let batchSizes = System.Collections.Generic.List<int>()

    let resolver: RequestResolver<int, string, unit> =
        RequestResolver.make (fun entries ->
            Effect.sync (fun () ->
                batchSizes.Add(List.length entries)
                entries |> List.iter (fun e -> e.CompleteUnsafe(Exit.succeed (square e)))))
        |> RequestResolver.batchN 2

    let requests: Request<int, string, unit> list = [ { Value = 1 }; { Value = 2 }; { Value = 3 }; { Value = 4 }; { Value = 5 } ]
    let results = run (RequestResolver.resolveAll resolver requests)
    Assert.Equal<int list>([ 1; 4; 9; 16; 25 ], results |> List.map (Exit.matchExit (fun _ -> 0) id))
    Assert.Equal<int list>([ 2; 2; 1 ], List.ofSeq batchSizes)

// --- grouped + batchN (the upstream "grouped requests + batchN" count) ---

[<Fact>]
let ``grouped plus batchN matches the upstream batch count`` () =
    let invocations = ref 0
    let seen = ref 0

    let resolver: RequestResolver<string, string, unit> =
        RequestResolver.make (fun entries ->
            Effect.sync (fun () ->
                incr invocations
                seen.Value <- seen.Value + List.length entries
                entries |> List.iter (fun e -> e.CompleteUnsafe(Exit.succeed (names.[nameId e])))))
        |> RequestResolver.batchN 5
        |> RequestResolver.grouped (fun e -> nameId e % 2)

    let requests: Request<string, string, unit> list = [ for i in 1..26 -> { Id = i } ]
    run (RequestResolver.resolveAll resolver requests) |> ignore
    // 2 groups (odd/even) of 13, each chunked into batches of 5 -> 3 batches each -> 6.
    Assert.Equal(6, invocations.Value)
    Assert.Equal(26, seen.Value)

// --- makeGrouped passes the key to the runner ---

[<Fact>]
let ``makeGrouped routes batches with their key`` () =
    let keysSeen = System.Collections.Generic.List<int>()

    let resolver: RequestResolver<string, string, unit> =
        RequestResolver.makeGrouped
            (fun e -> nameId e % 2)
            (fun entries key ->
                Effect.sync (fun () ->
                    keysSeen.Add key
                    entries |> List.iter (fun e -> e.CompleteUnsafe(Exit.succeed (names.[nameId e])))))

    let requests: Request<string, string, unit> list = [ { Id = 1 }; { Id = 2 }; { Id = 3 }; { Id = 4 } ]
    run (RequestResolver.resolveAll resolver requests) |> ignore
    Assert.Equal<int list>([ 1; 0 ], List.ofSeq keysSeen) // odds first (id 1), then evens (id 2)

// --- failure modes ---

[<Fact>]
let ``a runAll failure fails its batch's entries`` () =
    let resolver: RequestResolver<int, string, unit> =
        RequestResolver.make (fun _ -> Effect.fail "backend down")

    let requests: Request<int, string, unit> list = [ { Value = 1 }; { Value = 2 } ]
    Assert.Equal<Exit<int, string> list>([ Exit.fail "backend down"; Exit.fail "backend down" ], run (RequestResolver.resolveAll resolver requests))

[<Fact>]
let ``an entry the resolver leaves uncompleted dies with a defect`` () =
    let resolver: RequestResolver<int, string, unit> =
        RequestResolver.make (fun _ -> Effect.succeed ()) // completes nothing

    match run (RequestResolver.resolveAll resolver [ { Value = 1 } ]) with
    | [ Failure cause ] -> Assert.False(List.isEmpty (Cause.defects cause), "expected a defect")
    | other -> Assert.Fail(sprintf "expected one uncompleted defect, got %A" other)
