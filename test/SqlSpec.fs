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

    test "SqlClient transaction rolls back after a failed effect" (fun () ->
        let calls = System.Collections.Generic.List<string * obj list>()
        let conn = testConnection calls []
        let client = run (SqlClient.makeSimple (Effect.succeed conn) sqliteCompiler)
        let failure = SqlError.unknown None (Some "boom") (Some "test")

        let program =
            client.WithTransaction (
                effect {
                    let statement = client.Constructor.Unsafe "select 1" []
                    let! _ = statement.Execute
                    return! Effect.fail failure
                }
            )

        match Effect.runSync Context.empty program with
        | Success _ -> failwith "expected transaction failure"
        | Failure _ ->
            let names = calls |> Seq.map fst |> Seq.toArray
            toBe names.[0] "BEGIN"
            toBe names.[1] "select 1"
            toBe names.[2] "ROLLBACK")

    test "Statement.defaultTransforms renames row keys before execution results are exposed" (fun () ->
        let calls = System.Collections.Generic.List<string * obj list>()
        let conn = testConnection calls [ row [ "user_id", box 1; "user_name", box "Ada" ] ]
        let transforms = Statement.defaultTransforms (fun key -> key.Replace("_", ""))
        let client =
            run
                (SqlClient.make
                    { Acquirer = Effect.succeed conn
                      Compiler = sqliteCompiler
                      TransactionAcquirer = None
                      SpanAttributes = []
                      TransactionService = None
                      BeginTransaction = None
                      Rollback = None
                      Commit = None
                      Savepoint = None
                      RollbackSavepoint = None
                      TransformRows = Some transforms.Array })

        let rows = run (client.Constructor.Unsafe "select * from users" []).Execute
        toBe (rows.[0].ContainsKey "userid") true
        toBe (rows.[0].ContainsKey "user_id") false
        toBe (unbox<string> rows.[0].["username"]) "Ada")

    test "Reactivity register and mutation invalidate keys on success" (fun () ->
        let reactivity = Reactivity.makeUnsafe ()
        let mutable count = 0
        let cancel = reactivity.RegisterUnsafe [ box "users" ] (fun () -> count <- count + 1)

        runCtx (Context.make Reactivity.tag reactivity) (Reactivity.mutation [ box "users" ] (Effect.succeed ()))
        toBe count 1

        cancel ()
        runCtx (Context.make Reactivity.tag reactivity) (Reactivity.invalidate [ box "users" ])
        toBe count 1)

    test "Reactivity.query reruns with the original context on invalidation" (fun () ->
        let countTag = Tag.make<int ref> "sql-spec/reactivity-count"
        let reactivity = Reactivity.makeUnsafe ()

        let ctx =
            Context.empty
            |> Context.add Reactivity.tag reactivity
            |> Context.add countTag (ref 0)

        let source =
            Effect.service countTag
            |> Effect.map (fun count ->
                count.Value <- count.Value + 1
                count.Value)

        let program =
            effect {
                let! queue = Reactivity.query [ box "users" ] source
                let! first = Queue.take queue
                do! Reactivity.invalidate [ box "users" ]
                let! second = Queue.take queue
                return first, second
            }

        let first, second = runCtx ctx program
        toBe first 1
        toBe second 2)

    test "Reactivity.query propagates source failures through the mailbox" (fun () ->
        let reactivity = Reactivity.makeUnsafe ()
        let ctx = Context.make Reactivity.tag reactivity
        let failure = SqlError.unknown None (Some "reactive failure") (Some "query")

        let program =
            effect {
                let! queue = Reactivity.query [ box "users" ] (Effect.fail failure)
                return! Queue.take queue |> Effect.mapError unbox<SqlError>
            }

        match Effect.runSync ctx program with
        | Success _ -> failwith "expected reactive query failure"
        | Failure cause ->
            match Cause.failures cause with
            | [ error ] -> toBe (SqlError.message error) "reactive failure"
            | _ -> failwithf "unexpected cause: %s" (Cause.render cause)))
