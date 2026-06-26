module RequestResolverSpec

open System.Collections.Generic
open Effect
open Effect.Vitest

type private GetSquare =
    { Value: int }

    interface Request<int, string, unit>

type private GetNameById =
    { Id: int }

    interface Request<string, string, unit>

type private Tenant =
    { Prefix: string }

type private GetTenantName =
    { TenantId: int }

    interface Request<string, string, Context>

let private square (e: Entry<int, string, unit>) =
    let v = (e.Request :?> GetSquare).Value
    v * v

let private nameId (e: Entry<string, string, unit>) =
    (e.Request :?> GetNameById).Id

let private names =
    Map [ for i in 1..26 -> i, string (char (96 + i)) ]

let private tenantTag = Tag.make<Tenant> "request/tenant"

let private run (eff: Effect<'A, 'E, unit>) : 'A =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure cause -> failwithf "unexpected failure: %s" (Cause.render cause)

let private runContext (ctx: Context) (eff: Effect<'A, 'E, Context>) : 'A =
    match Effect.runSync ctx eff with
    | Success a -> a
    | Failure cause -> failwithf "unexpected failure: %s" (Cause.render cause)

describe "RequestResolver" (fun () ->
    test "fromFunction resolves each request with a pure function" (fun () ->
        let resolver = RequestResolver.fromFunction square
        let requests: Request<int, string, unit> list = [ { Value = 2 }; { Value = 5 } ]

        toEqual
            (run (RequestResolver.resolveAll resolver requests))
            [ Exit.succeed 4
              Exit.succeed 25 ])

    test "fromFunctionBatched resolves a whole batch positionally" (fun () ->
        let resolver =
            RequestResolver.fromFunctionBatched (fun entries -> entries |> List.map square |> List.toSeq)

        let requests: Request<int, string, unit> list =
            [ { Value = 1 }; { Value = 2 }; { Value = 3 } ]

        let results =
            run (RequestResolver.resolveAll resolver requests)
            |> List.map (Exit.matchExit (fun _ -> 0) id)

        toEqual results [ 1; 4; 9 ])

    test "resolve folds a single request's exit into the effect" (fun () ->
        let resolver = RequestResolver.fromFunction square
        toBe (run (RequestResolver.resolve resolver { Value = 7 })) 49)

    test "make batches all requests into one runAll invocation" (fun () ->
        let invocations = ref 0
        let seen = ref 0

        let resolver: RequestResolver<string, string, unit> =
            RequestResolver.make (fun entries ->
                Effect.sync (fun () ->
                    invocations.Value <- invocations.Value + 1
                    seen.Value <- seen.Value + List.length entries

                    entries
                    |> List.iter (fun e -> e.CompleteUnsafe(Exit.succeed (names.[nameId e])))))

        let requests: Request<string, string, unit> list =
            [ { Id = 1 }; { Id = 2 }; { Id = 3 } ]

        let results = run (RequestResolver.resolveAll resolver requests)
        toEqual results [ Exit.succeed "a"; Exit.succeed "b"; Exit.succeed "c" ]
        toBe invocations.Value 1
        toBe seen.Value 3)

    test "identical requests are batched but kept as individual entries" (fun () ->
        let invocations = ref 0
        let seen = ref 0

        let resolver: RequestResolver<string, string, unit> =
            RequestResolver.make (fun entries ->
                Effect.sync (fun () ->
                    invocations.Value <- invocations.Value + 1
                    seen.Value <- seen.Value + List.length entries

                    entries
                    |> List.iter (fun e -> e.CompleteUnsafe(Exit.succeed (names.[nameId e])))))

        let requests: Request<string, string, unit> list = [ { Id = 1 }; { Id = 1 } ]
        let results = run (RequestResolver.resolveAll resolver requests)
        toEqual results [ Exit.succeed "a"; Exit.succeed "a" ]
        toBe invocations.Value 1
        toBe seen.Value 2)

    test "fromEffect completes entries from an effectful function" (fun () ->
        let resolver =
            RequestResolver.fromEffect (fun (e: Entry<string, string, unit>) ->
                match Map.tryFind (nameId e) names with
                | Some name -> Effect.succeed name
                | None -> Effect.fail "Not Found")

        let requests: Request<string, string, unit> list = [ { Id = 1 }; { Id = 99 } ]

        toEqual
            (run (RequestResolver.resolveAll resolver requests))
            [ Exit.succeed "a"
              Exit.fail "Not Found" ])

    test "entries capture the ambient Context" (fun () ->
        let resolver: RequestResolver<string, string, Context> =
            RequestResolver.fromFunction (fun e ->
                let tenant = Context.get tenantTag e.Context
                let req = e.Request :?> GetTenantName
                sprintf "%s-%d" tenant.Prefix req.TenantId)

        let ctx = Context.make tenantTag { Prefix = "tenant" }
        let request: Request<string, string, Context> = { TenantId = 7 }

        toEqual (runContext ctx (RequestResolver.resolve resolver request)) "tenant-7")

    test "batchN splits a group into bounded batches" (fun () ->
        let batchSizes = List<int>()

        let resolver: RequestResolver<int, string, unit> =
            RequestResolver.make (fun entries ->
                Effect.sync (fun () ->
                    batchSizes.Add(List.length entries)
                    entries |> List.iter (fun e -> e.CompleteUnsafe(Exit.succeed (square e)))))
            |> RequestResolver.batchN 2

        let requests: Request<int, string, unit> list =
            [ { Value = 1 }; { Value = 2 }; { Value = 3 }; { Value = 4 }; { Value = 5 } ]

        let results =
            run (RequestResolver.resolveAll resolver requests)
            |> List.map (Exit.matchExit (fun _ -> 0) id)

        toEqual results [ 1; 4; 9; 16; 25 ]
        toEqual (List.ofSeq batchSizes) [ 2; 2; 1 ])

    test "grouped plus batchN matches the upstream batch count" (fun () ->
        let invocations = ref 0
        let seen = ref 0

        let resolver: RequestResolver<string, string, unit> =
            RequestResolver.make (fun entries ->
                Effect.sync (fun () ->
                    invocations.Value <- invocations.Value + 1
                    seen.Value <- seen.Value + List.length entries

                    entries
                    |> List.iter (fun e -> e.CompleteUnsafe(Exit.succeed (names.[nameId e])))))
            |> RequestResolver.batchN 5
            |> RequestResolver.grouped (fun e -> nameId e % 2)

        let requests: Request<string, string, unit> list = [ for i in 1..26 -> { Id = i } ]
        run (RequestResolver.resolveAll resolver requests) |> ignore
        toBe invocations.Value 6
        toBe seen.Value 26)

    test "makeGrouped routes batches with their key" (fun () ->
        let keysSeen = List<int>()

        let resolver: RequestResolver<string, string, unit> =
            RequestResolver.makeGrouped (fun e -> nameId e % 2) (fun entries key ->
                Effect.sync (fun () ->
                    keysSeen.Add key

                    entries
                    |> List.iter (fun e -> e.CompleteUnsafe(Exit.succeed (names.[nameId e])))))

        let requests: Request<string, string, unit> list =
            [ { Id = 1 }; { Id = 2 }; { Id = 3 }; { Id = 4 } ]

        run (RequestResolver.resolveAll resolver requests) |> ignore
        toEqual (List.ofSeq keysSeen) [ 1; 0 ])

    test "a runAll failure fails its batch's entries" (fun () ->
        let resolver: RequestResolver<int, string, unit> =
            RequestResolver.make (fun _ -> Effect.fail "backend down")

        let requests: Request<int, string, unit> list = [ { Value = 1 }; { Value = 2 } ]

        toEqual
            (run (RequestResolver.resolveAll resolver requests))
            [ Exit.fail "backend down"
              Exit.fail "backend down" ])

    test "an entry the resolver leaves uncompleted dies with a defect" (fun () ->
        let resolver: RequestResolver<int, string, unit> =
            RequestResolver.make (fun _ -> Effect.succeed ())

        match run (RequestResolver.resolveAll resolver [ { Value = 1 } ]) with
        | [ Failure cause ] -> toBe (List.isEmpty (Cause.defects cause)) false
        | other -> failwithf "expected one uncompleted defect, got %A" other))
