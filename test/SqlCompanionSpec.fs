module SqlCompanionSpec

open Effect
open Effect.Vitest

type private UserReq = { Id: int }

type private UserRow = { Id: int; Name: string }

type private GroupReq = { Group: string }

type private GroupRow = { Group: string; Name: string }

let private reqSchema: Schema<UserReq> =
    Schema.object {
        let! id = Schema.field "id" Schema.int (fun req -> req.Id)
        return { Id = id }
    }

let private rowSchema: Schema<UserRow> =
    Schema.object {
        let! id = Schema.field "id" Schema.int (fun row -> row.Id)
        and! name = Schema.field "name" Schema.string (fun row -> row.Name)
        return { Id = id; Name = name }
    }

let private groupReqSchema: Schema<GroupReq> =
    Schema.object {
        let! group = Schema.field "group" Schema.string (fun req -> req.Group)
        return { Group = group }
    }

let private groupRowSchema: Schema<GroupRow> =
    Schema.object {
        let! group = Schema.field "group" Schema.string (fun row -> row.Group)
        and! name = Schema.field "name" Schema.string (fun row -> row.Name)
        return { Group = group; Name = name }
    }

let private row fields : Row = fields |> Map.ofList

let private run eff =
    match Effect.runSync Context.empty eff with
    | Success value -> value
    | Failure cause -> failwithf "unexpected failure: %s" (Cause.render cause)

describe "Effect SQL companion layer" (fun () ->
    test "SqlSchema.findAll encodes requests and decodes SQL rows" (fun () ->
        let seen = ref JNull

        let execute encoded =
            Effect.sync (fun () ->
                seen.Value <- encoded
                [ row [ "id", box 1; "name", box "Ada" ]
                  row [ "id", box 2; "name", box "Grace" ] ])

        let results = run (SqlSchema.findAll reqSchema rowSchema execute { Id = 1 })

        toEqual results [ { Id = 1; Name = "Ada" }; { Id = 2; Name = "Grace" } ]

        match seen.Value with
        | JObject fields ->
            match Map.tryFind "id" fields with
            | Some(JNumber id) -> toBe id 1.0
            | other -> failwithf "expected encoded id, got %A" other
        | other -> failwithf "expected encoded request object, got %A" other)

    test "SqlSchema.findOneOption returns None for empty results" (fun () ->
        let execute _ = Effect.succeed []
        let result = run (SqlSchema.findOneOption reqSchema rowSchema execute { Id = 1 })
        toBe result None)

    test "SqlResolver.ordered completes requests positionally" (fun () ->
        let batches = ref []

        let execute encoded =
            Effect.sync (fun () ->
                batches.Value <- encoded :: batches.Value

                encoded
                |> List.map (function
                    | JObject fields ->
                        match Map.find "id" fields with
                        | JNumber id -> row [ "id", box (int id); "name", box (sprintf "user-%d" (int id)) ]
                        | other -> failwithf "unexpected id json: %A" other
                    | other -> failwithf "unexpected request json: %A" other))

        let resolver = SqlResolver.ordered reqSchema rowSchema execute

        let program =
            RequestResolver.resolveAll
                resolver
                [ SqlResolver.makeRequest { Id = 1 } :> Request<UserRow, obj, Context>
                  SqlResolver.makeRequest { Id = 2 } :> Request<UserRow, obj, Context> ]

        let exits = run program
        toEqual exits [ Exit.succeed { Id = 1; Name = "user-1" }; Exit.succeed { Id = 2; Name = "user-2" } ]
        toBe (List.length batches.Value) 1)

    test "SqlResolver.ordered fails each request on result length mismatch" (fun () ->
        let execute _ = Effect.succeed [ row [ "id", box 1; "name", box "Ada" ] ]
        let resolver = SqlResolver.ordered reqSchema rowSchema execute

        let program =
            RequestResolver.resolveAll
                resolver
                [ SqlResolver.makeRequest { Id = 1 } :> Request<UserRow, obj, Context>
                  SqlResolver.makeRequest { Id = 2 } :> Request<UserRow, obj, Context> ]

        match run program with
        | [ Failure first; Failure second ] ->
            match Cause.failures first, Cause.failures second with
            | [ a ], [ b ] ->
                let mismatchA = unbox<ResultLengthMismatch> a
                let mismatchB = unbox<ResultLengthMismatch> b
                toBe mismatchA.Expected 2
                toBe mismatchA.Actual 1
                toBe mismatchB.Expected 2
                toBe mismatchB.Actual 1
            | other -> failwithf "unexpected mismatch failures: %A" other
        | other -> failwithf "expected two length mismatch failures, got %A" other)

    test "SqlResolver.grouped completes each request with its matching result group" (fun () ->
        let execute _ =
            Effect.succeed
                [ row [ "group", box "a"; "name", box "Ada" ]
                  row [ "group", box "a"; "name", box "Alonzo" ]
                  row [ "group", box "b"; "name", box "Grace" ] ]

        let resolver =
            SqlResolver.grouped groupReqSchema (fun req -> req.Group) groupRowSchema (fun result _ -> result.Group) execute

        let aRows = run (SqlResolver.request resolver { Group = "a" })
        let bRows = run (SqlResolver.request resolver { Group = "b" })

        toEqual (aRows |> List.map _.Name) [ "Ada"; "Alonzo" ]
        toEqual (bRows |> List.map _.Name) [ "Grace" ])

    test "SqlResolver.findById maps unordered result rows back to requests" (fun () ->
        let execute _ =
            Effect.succeed
                [ row [ "id", box 2; "name", box "Grace" ]
                  row [ "id", box 1; "name", box "Ada" ] ]

        let resolver =
            SqlResolver.findById Schema.int rowSchema (fun result _ -> result.Id) execute

        let first = run (SqlResolver.request resolver 1)
        let second = run (SqlResolver.request resolver 2)

        toBe first.Name "Ada"
        toBe second.Name "Grace")

    test "SqlStream.asyncPauseResume adapts callback emitters" (fun () ->
        let stream =
            SqlStream.asyncPauseResume
                (fun emit ->
                    Effect.sync (fun () ->
                        emit.Single 1
                        emit.Array [ 2; 3 ]
                        emit.End()

                        { OnPause = ignore
                          OnResume = ignore }))
                2

        let values = run (Stream.runCollect stream)
        toEqual values [ 1; 2; 3 ]))
