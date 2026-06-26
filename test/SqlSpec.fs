module SqlSpec

open Effect
open Effect.Vitest

let private run eff =
    match Effect.runSync Context.empty eff with
    | Success value -> value
    | Failure cause -> failwithf "unexpected failure: %s" (Cause.render cause)

let private runCtx ctx eff =
    match Effect.runSync ctx eff with
    | Success value -> value
    | Failure cause -> failwithf "unexpected failure: %s" (Cause.render cause)

let private row fields : Row = fields |> Map.ofList

let private testConnection (calls: System.Collections.Generic.List<string * obj list>) rows =
    let execute sql parameters transformRows =
        Effect.sync (fun () ->
            calls.Add(sql, parameters)

            match transformRows with
            | Some f -> f rows
            | None -> rows)

    SqlConnection.make
        execute
        (fun sql parameters ->
            Effect.sync (fun () ->
                calls.Add(sql, parameters)
                box rows))
        (fun sql parameters transformRows ->
            Stream.fromEffect (execute sql parameters transformRows) |> Stream.flatMap Stream.fromIterable)
        (fun sql parameters ->
            Effect.sync (fun () ->
                calls.Add(sql, parameters)
                rows |> List.map (Map.toList >> List.map snd)))
        (fun sql parameters ->
            Effect.sync (fun () ->
                calls.Add(sql, parameters)
                rows |> List.map (Map.toList >> List.map snd)))
        execute

let private sqliteCompiler =
    Statement.makeCompiler
        { Dialect = Sqlite
          Placeholder = fun _ _ -> "?"
          OnIdentifier = fun value _ -> (Statement.defaultEscape "\"") value
          OnRecordUpdate = fun _ _ _ _ _ -> "", []
          OnCustom =
            fun custom _ _ ->
                match custom.Kind, custom.ParamA with
                | "Lit", Some value -> string value, []
                | _ -> "", []
          OnInsert = None
          OnRecordUpdateSingle = None }

describe "Effect SQL core" (fun () ->
    test "SqlError exposes message and retryability" (fun () ->
        let connection = SqlError.connection None (Some "offline") (Some "connect")
        let syntax = SqlError.syntax None None (Some "query")

        toBe (SqlError.message connection) "offline"
        toBe (SqlError.operation connection) (Some "connect")
        toBe (SqlError.isRetryable connection) true
        toBe (SqlError.isRetryable syntax) false)

    test "Statement compiler renders literals identifiers parameters arrays and custom segments" (fun () ->
        let frag =
            Statement.fragment
                [ Literal("select ", [])
                  Identifier "user.name"
                  Literal(" from users where id in ", [])
                  ArrayHelper [ box 1; box 2 ]
                  Literal(" and mode = ", [])
                  Custom
                    { Kind = "Lit"
                      ParamA = Some(box "'active'")
                      ParamB = None
                      ParamC = None } ]

        let sql, parameters = Statement.compile sqliteCompiler frag false
        toBe sql "select \"user\".\"name\" from users where id in (?,?) and mode = 'active'"
        toBe parameters.Length 2
        toBe (unbox<int> parameters.[0]) 1
        toBe (unbox<int> parameters.[1]) 2)

    test "SqlClient executes compiled statements through the connection" (fun () ->
        let calls = System.Collections.Generic.List<string * obj list>()
        let conn = testConnection calls [ row [ "id", box 1 ] ]
        let acquirer = Effect.succeed conn
        let client = run (SqlClient.makeSimple acquirer sqliteCompiler)
        let statement = client.Constructor.Unsafe "select * from users where id = ?" [ box 1 ]
        let rows = run statement.Execute

        toBe rows.Length 1
        toBe (unbox<int> rows.[0].["id"]) 1
        toBe calls.Count 1
        toBe (fst calls.[0]) "select * from users where id = ?"
        toBe (snd calls.[0]).Length 1
        toBe (unbox<int> (snd calls.[0]).[0]) 1)

    test "SqlClient transaction begins and commits around a successful effect" (fun () ->
        let calls = System.Collections.Generic.List<string * obj list>()
        let conn = testConnection calls []
        let client = run (SqlClient.makeSimple (Effect.succeed conn) sqliteCompiler)

        let result =
            client.WithTransaction (
                effect {
                    let statement = client.Constructor.Unsafe "select 1" []
                    let! _ = statement.Execute
                    return 42
                }
            )
            |> run

        toBe result 42
        let names = calls |> Seq.map fst |> Seq.toArray
        toBe names.[0] "BEGIN"
        toBe names.[1] "select 1"
        toBe names.[2] "COMMIT")

    test "Reactivity register and mutation invalidate keys on success" (fun () ->
        let reactivity = Reactivity.makeUnsafe ()
        let mutable count = 0
        let cancel = reactivity.RegisterUnsafe [ box "users" ] (fun () -> count <- count + 1)

        runCtx (Context.make Reactivity.tag reactivity) (Reactivity.mutation [ box "users" ] (Effect.succeed ()))
        toBe count 1

        cancel ()
        runCtx (Context.make Reactivity.tag reactivity) (Reactivity.invalidate [ box "users" ])
        toBe count 1))
