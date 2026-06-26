namespace Effect

type SqlClient =
    abstract Constructor: SqlConstructor
    abstract Safe: SqlConstructor
    abstract WithoutTransforms: unit -> SqlConstructor
    abstract Reserve: Effect<SqlConnection, SqlError, Context>
    abstract WithTransaction<'A> : Effect<'A, SqlError, Context> -> Effect<'A, SqlError, Context>
    abstract TransactionService: Tag<SqlConnection * int>
    abstract Reactive<'A, 'E> : ReactivityKeys -> Effect<'A, 'E, Context> -> Stream<'A, 'E, Context>
    abstract ReactiveMailbox<'A, 'E> : ReactivityKeys -> Effect<'A, 'E, Context> -> Stream<'A, 'E, Context>

type SqlClientMakeOptions =
    { Acquirer: SqlConnectionAcquirer
      Compiler: Compiler
      TransactionAcquirer: SqlConnectionAcquirer option
      SpanAttributes: (string * obj) list
      TransactionService: Tag<SqlConnection * int> option
      BeginTransaction: string option
      Rollback: string option
      Commit: string option
      Savepoint: (string -> string) option
      RollbackSavepoint: (string -> string) option
      TransformRows: TransformRows option }

[<RequireQualifiedAccess>]
module SqlClient =

    let tag: Tag<SqlClient> = Tag.make<SqlClient> "effect/sql/SqlClient"

    let private transactionTag id =
        Tag.make<SqlConnection * int> (sprintf "effect/sql/SqlClient/TransactionConnection/%d" id)

    let mutable private clientId = 0

    let private nextTransactionTag () =
        let id = clientId
        clientId <- clientId + 1
        transactionTag id

    let private connectionFromEnv transactionService acquirer =
        Effect.environment<Context, SqlError>
        |> Effect.flatMap (fun ctx ->
            match Context.tryGet transactionService ctx with
            | Some(conn, _) -> Effect.succeed conn
            | None -> acquirer)

    let make (options: SqlClientMakeOptions) : Effect<SqlClient, SqlError, Context> =
        effect {
            let transactionService = defaultArg options.TransactionService (nextTransactionTag ())
            let acquirer = connectionFromEnv transactionService options.Acquirer
            let constructor = Statement.constructor acquirer options.Compiler options.TransformRows
            let noTransformConstructor = Statement.constructor acquirer options.Compiler None
            let! reactivity = Reactivity.service

            let beginTransaction = defaultArg options.BeginTransaction "BEGIN"
            let commit = defaultArg options.Commit "COMMIT"
            let rollback = defaultArg options.Rollback "ROLLBACK"
            let savepoint = defaultArg options.Savepoint (fun name -> "SAVEPOINT " + name)
            let rollbackSavepoint = defaultArg options.RollbackSavepoint (fun name -> "ROLLBACK TO SAVEPOINT " + name)
            let transactionAcquirer = defaultArg options.TransactionAcquirer options.Acquirer

            let runCommand conn sql =
                conn.ExecuteUnprepared sql [] None |> Effect.map ignore

            let withTransaction (eff: Effect<'A, SqlError, Context>) : Effect<'A, SqlError, Context> =
                Effect.environment<Context, SqlError>
                |> Effect.flatMap (fun ctx ->
                    match Context.tryGet transactionService ctx with
                    | Some(conn, depth) ->
                        let name = sprintf "effect_sql_%d" depth

                        effect {
                            do! runCommand conn (savepoint name)

                            let nested =
                                Effect.provideContext (Context.add transactionService (conn, depth + 1) ctx) eff

                            let! exit = Effect.exit nested

                            match exit with
                            | Success value -> return value
                            | Failure cause ->
                                do! runCommand conn (rollbackSavepoint name)
                                return! Effect.failCause cause
                        }
                    | None ->
                        effect {
                            let! conn = transactionAcquirer
                            do! runCommand conn beginTransaction

                            let scoped =
                                Effect.provideContext (Context.add transactionService (conn, 1) ctx) eff

                            let! exit = Effect.exit scoped

                            match exit with
                            | Success value ->
                                do! runCommand conn commit
                                return value
                            | Failure cause ->
                                do! runCommand conn rollback
                                return! Effect.failCause cause
                        })

            let client =
                { new SqlClient with
                    member _.Constructor = constructor
                    member _.Safe = constructor
                    member _.WithoutTransforms () = noTransformConstructor
                    member _.Reserve = transactionAcquirer
                    member _.WithTransaction eff = withTransaction eff
                    member _.TransactionService = transactionService
                    member _.Reactive keys eff = reactivity.Stream keys eff
                    member _.ReactiveMailbox keys eff = reactivity.Stream keys eff }

            return client
        }

    let makeSimple acquirer compiler =
        make
            { Acquirer = acquirer
              Compiler = compiler
              TransactionAcquirer = None
              SpanAttributes = []
              TransactionService = None
              BeginTransaction = None
              Rollback = None
              Commit = None
              Savepoint = None
              RollbackSavepoint = None
              TransformRows = None }

    let service<'E> : Effect<SqlClient, 'E, Context> = Effect.service tag

    let layer options : Layer<SqlError, Context> =
        Layer.effect tag (make options)
