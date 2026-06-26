namespace Effect

/// Generic SQL row shape used by low-level drivers.
type Row = Map<string, obj>

type TransformRows = Row list -> Row list

/// Low-level SQL driver connection capable of executing compiled SQL.
type SqlConnection =
    { Execute: string -> obj list -> TransformRows option -> Effect<Row list, SqlError, Context>
      ExecuteRaw: string -> obj list -> Effect<obj, SqlError, Context>
      ExecuteStream: string -> obj list -> TransformRows option -> Stream<Row, SqlError, Context>
      ExecuteValues: string -> obj list -> Effect<obj list list, SqlError, Context>
      ExecuteValuesUnprepared: string -> obj list -> Effect<obj list list, SqlError, Context>
      ExecuteUnprepared: string -> obj list -> TransformRows option -> Effect<Row list, SqlError, Context> }

type SqlConnectionAcquirer = Effect<SqlConnection, SqlError, Context>

[<RequireQualifiedAccess>]
module SqlConnection =

    let tag: Tag<SqlConnection> = Tag.make<SqlConnection> "effect/sql/SqlConnection"

    let make
        execute
        executeRaw
        executeStream
        executeValues
        executeValuesUnprepared
        executeUnprepared
        : SqlConnection =
        { Execute = execute
          ExecuteRaw = executeRaw
          ExecuteStream = executeStream
          ExecuteValues = executeValues
          ExecuteValuesUnprepared = executeValuesUnprepared
          ExecuteUnprepared = executeUnprepared }

    let service<'E> : Effect<SqlConnection, 'E, Context> = Effect.service tag
