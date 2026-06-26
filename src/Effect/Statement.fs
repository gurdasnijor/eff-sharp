namespace Effect

open System

type SqlDialect =
    | Sqlite
    | Postgres
    | Mysql
    | SqlServer
    | Clickhouse

type Custom =
    { Kind: string
      ParamA: obj option
      ParamB: obj option
      ParamC: obj option }

type Fragment =
    { Segments: Segment list }

and RecordInsertHelper =
    { Value: Map<string, obj> list
      Returning: Fragment option }

and RecordUpdateHelper =
    { Value: Map<string, obj> list
      Alias: string
      Returning: Fragment option }

and RecordUpdateHelperSingle =
    { Value: Map<string, obj>
      Omit: string list
      Returning: Fragment option }

and Segment =
    | Literal of value: string * parameters: obj list
    | Identifier of value: string
    | Parameter of value: obj
    | ArrayHelper of values: obj list
    | RecordInsertHelper of RecordInsertHelper
    | RecordUpdateHelper of RecordUpdateHelper
    | RecordUpdateHelperSingle of RecordUpdateHelperSingle
    | Custom of Custom

type CompiledStatement = string * obj list

type CompilerOptions =
    { Dialect: SqlDialect
      Placeholder: int -> obj -> string
      OnIdentifier: string -> bool -> string
      OnRecordUpdate: string -> string -> string -> obj list list -> CompiledStatement option -> CompiledStatement
      OnCustom: Custom -> (obj -> string) -> bool -> CompiledStatement
      OnInsert: (string list -> string -> obj list list -> CompiledStatement option -> CompiledStatement) option
      OnRecordUpdateSingle: (string list -> obj list -> CompiledStatement option -> CompiledStatement) option }

type Compiler =
    { Dialect: SqlDialect
      Compile: Fragment -> bool -> CompiledStatement }

type Statement<'A> =
    { Fragment: Fragment
      Raw: Effect<obj, SqlError, Context>
      WithoutTransform: Effect<'A list, SqlError, Context>
      Stream: Stream<'A, SqlError, Context>
      Values: Effect<obj list list, SqlError, Context>
      ValuesUnprepared: Effect<obj list list, SqlError, Context>
      Unprepared: Effect<'A list, SqlError, Context>
      Execute: Effect<'A list, SqlError, Context>
      Compile: bool -> CompiledStatement }

type SqlConstructor =
    { Unsafe: string -> obj list -> Statement<Row>
      Identifier: string -> Fragment
      Literal: string -> Fragment
      InValues: obj list -> Fragment
      Insert: Map<string, obj> list -> Fragment
      Update: Map<string, obj> -> string list -> Fragment
      UpdateValues: Map<string, obj> list -> string -> Fragment
      And: Fragment list -> Fragment
      Or: Fragment list -> Fragment
      Csv: Fragment list -> Fragment
      Join: string -> bool -> string -> Fragment list -> Fragment }

[<RequireQualifiedAccess>]
module Statement =

    let fragment segments = { Segments = segments }

    let emptyFragment = fragment [ Literal("", []) ]

    let literalSegment value parameters = Literal(value, parameters)

    let literal value = fragment [ Literal(value, []) ]

    let identifier value = fragment [ Identifier value ]

    let parameter value = fragment [ Parameter value ]

    let arrayHelper values = fragment [ ArrayHelper values ]

    let recordInsertHelper values = fragment [ RecordInsertHelper { Value = values; Returning = None } ]

    let recordUpdateHelper values alias =
        fragment [ RecordUpdateHelper { Value = values; Alias = alias; Returning = None } ]

    let recordUpdateHelperSingle value omit =
        fragment [ RecordUpdateHelperSingle { Value = value; Omit = omit; Returning = None } ]

    let custom kind paramA paramB paramC =
        Custom
            { Kind = kind
              ParamA = paramA
              ParamB = paramB
              ParamC = paramC }

    let custom1 kind paramA = custom kind (Some paramA) None None

    let custom2 kind paramA paramB = custom kind (Some paramA) (Some paramB) None

    let isCustom kind segment =
        match segment with
        | Custom c when c.Kind = kind -> true
        | _ -> false

    let private appendCompiled (sql: string) (binds: obj list) ((s, b): CompiledStatement) =
        sql + s, binds @ b

    let private compileReturning (compiler: Compiler) withoutTransform returning =
        returning |> Option.map (fun f -> (compiler.Compile) f withoutTransform)

    let private generateColumns (keys: string list) onIdentifier withoutTransform =
        keys |> List.map (fun k -> onIdentifier k withoutTransform) |> String.concat ", " |> sprintf "(%s)"

    let makeCompiler (options: CompilerOptions) : Compiler =
        let rec self =
            { Dialect = options.Dialect
              Compile =
                fun statement withoutTransform ->
                    let mutable sql = ""
                    let mutable binds: obj list = []
                    let mutable placeholderCount = 0

                    let placeholder value =
                        placeholderCount <- placeholderCount + 1
                        options.Placeholder placeholderCount value

                    for segment in statement.Segments do
                        match segment with
                        | Literal(value, parameters) ->
                            sql <- sql + value
                            binds <- binds @ parameters

                        | Identifier value -> sql <- sql + options.OnIdentifier value withoutTransform

                        | Parameter value ->
                            sql <- sql + placeholder value
                            binds <- binds @ [ value ]

                        | ArrayHelper values ->
                            let placeholders = values |> List.map placeholder |> String.concat ","
                            sql <- sql + "(" + placeholders + ")"
                            binds <- binds @ values

                        | Custom custom ->
                            let s, b = options.OnCustom custom placeholder withoutTransform
                            sql <- sql + s
                            binds <- binds @ b

                        | RecordInsertHelper helper ->
                            match helper.Value with
                            | [] -> ()
                            | first :: _ ->
                                let keys = first |> Map.toList |> List.map fst
                                let rows = helper.Value |> List.map (fun row -> keys |> List.map (fun k -> row.[k]))

                                let placeholders =
                                    rows
                                    |> List.map (fun row -> row |> List.map placeholder |> String.concat "," |> sprintf "(%s)")
                                    |> String.concat ","

                                match options.OnInsert with
                                | Some onInsert ->
                                    let columns =
                                        keys |> List.map (fun k -> options.OnIdentifier k withoutTransform)

                                    let s, b =
                                        onInsert columns placeholders rows (compileReturning self withoutTransform helper.Returning)

                                    sql <- sql + s
                                    binds <- binds @ b
                                | None ->
                                    sql <- sql + generateColumns keys options.OnIdentifier withoutTransform + " VALUES " + placeholders
                                    binds <- binds @ (rows |> List.collect id)

                        | RecordUpdateHelperSingle helper ->
                            let keys =
                                helper.Value
                                |> Map.toList
                                |> List.map fst
                                |> List.filter (fun key -> not (List.contains key helper.Omit))

                            let values = keys |> List.map (fun key -> helper.Value.[key])

                            match options.OnRecordUpdateSingle with
                            | Some onUpdate ->
                                let columns =
                                    keys |> List.map (fun k -> options.OnIdentifier k withoutTransform)

                                let s, b =
                                    onUpdate columns values (compileReturning self withoutTransform helper.Returning)

                                sql <- sql + s
                                binds <- binds @ b
                            | None ->
                                let assignments =
                                    keys
                                    |> List.map (fun key -> sprintf "%s = %s" (options.OnIdentifier key withoutTransform) (placeholder helper.Value.[key]))
                                    |> String.concat ", "

                                sql <- sql + assignments
                                binds <- binds @ values

                        | RecordUpdateHelper helper ->
                            match helper.Value with
                            | [] -> ()
                            | first :: _ ->
                                let keys = first |> Map.toList |> List.map fst
                                let rows = helper.Value |> List.map (fun row -> keys |> List.map (fun k -> row.[k]))

                                let placeholders =
                                    rows
                                    |> List.map (fun row -> row |> List.map placeholder |> String.concat "," |> sprintf "(%s)")
                                    |> String.concat ","

                                let s, b =
                                    options.OnRecordUpdate
                                        placeholders
                                        helper.Alias
                                        (generateColumns keys options.OnIdentifier withoutTransform)
                                        rows
                                        (compileReturning self withoutTransform helper.Returning)

                                sql <- sql + s
                                binds <- binds @ b

                    sql, binds }

        self

    let makeCompilerSimple dialect placeholder escape =
        makeCompiler
            { Dialect = dialect
              Placeholder = placeholder
              OnIdentifier = fun value _ -> escape value
              OnRecordUpdate = fun _ _ _ _ _ -> "", []
              OnCustom = fun _ _ _ -> "", []
              OnInsert = None
              OnRecordUpdateSingle = None }

    let defaultEscape (delimiter: string) =
        fun (str: string) ->
            let doubled = delimiter + delimiter
            delimiter + str.Replace(delimiter, doubled).Replace(".", delimiter + "." + delimiter) + delimiter

    let makeCompilerSqlite transform =
        let escape = defaultEscape "\""

        makeCompiler
            { Dialect = Sqlite
              Placeholder = fun _ _ -> "?"
              OnIdentifier =
                fun value withoutTransform ->
                    match transform with
                    | Some f when not withoutTransform -> escape (f value)
                    | _ -> escape value
              OnRecordUpdate = fun _ _ _ _ _ -> "", []
              OnCustom = fun _ _ _ -> "", []
              OnInsert = None
              OnRecordUpdateSingle = None }

    let compile (compiler: Compiler) fragment withoutTransform = (compiler.Compile) fragment withoutTransform

    let private rowsToA<'A> (rows: Row list) : 'A list = rows |> List.map (box >> unbox<'A>)

    let makeStatement<'A>
        (acquirer: SqlConnectionAcquirer)
        (compiler: Compiler)
        (transformRows: TransformRows option)
        (frag: Fragment)
        : Statement<'A> =
        let compiled withoutTransform = (compiler.Compile) frag withoutTransform

        let withConnection f =
            acquirer |> Effect.flatMap f

        let execute withoutTransform =
            withConnection (fun conn ->
                let sql, parameters = compiled withoutTransform
                conn.Execute sql parameters transformRows |> Effect.map rowsToA<'A>)

        let executeUnprepared withoutTransform =
            withConnection (fun conn ->
                let sql, parameters = compiled withoutTransform
                conn.ExecuteUnprepared sql parameters transformRows |> Effect.map rowsToA<'A>)

        { Fragment = frag
          Raw =
            withConnection (fun conn ->
                let sql, parameters = compiled false
                conn.ExecuteRaw sql parameters)
          WithoutTransform = execute true
          Stream =
            Stream.unwrap (
                withConnection (fun conn ->
                    let sql, parameters = compiled false
                    conn.ExecuteStream sql parameters transformRows |> Stream.map (box >> unbox<'A>) |> Effect.succeed)
            )
          Values =
            withConnection (fun conn ->
                let sql, parameters = compiled false
                conn.ExecuteValues sql parameters)
          ValuesUnprepared =
            withConnection (fun conn ->
                let sql, parameters = compiled false
                conn.ExecuteValuesUnprepared sql parameters)
          Unprepared = executeUnprepared false
          Execute = execute false
          Compile = compiled }

    let unsafe<'A>
        (acquirer: SqlConnectionAcquirer)
        (compiler: Compiler)
        (transformRows: TransformRows option)
        (sql: string)
        (parameters: obj list)
        : Statement<'A> =
        makeStatement acquirer compiler transformRows (fragment [ Literal(sql, parameters) ])

    let private convertLiteralOrFragment item =
        match item with
        | Choice1Of2 sql -> [ Literal(sql, []) ]
        | Choice2Of2 frag -> frag.Segments

    let join separator addParens fallback clauses =
        match clauses with
        | [] -> literal fallback
        | [ one ] -> one
        | first :: rest ->
            let segments =
                [ if addParens then Literal("(", [])
                  yield! first.Segments
                  for clause in rest do
                      Literal(separator, [])
                      yield! clause.Segments
                  if addParens then Literal(")", []) ]

            fragment segments

    let and' clauses = join " AND " true "1=1" clauses

    let or' clauses = join " OR " true "1=1" clauses

    let csv clauses = join "," false "" clauses

    let constructor acquirer compiler transformRows : SqlConstructor =
        { Unsafe = fun sql parameters -> unsafe<Row> acquirer compiler transformRows sql parameters
          Identifier = identifier
          Literal = literal
          InValues = arrayHelper
          Insert = recordInsertHelper
          Update = recordUpdateHelperSingle
          UpdateValues = recordUpdateHelper
          And = and'
          Or = or'
          Csv = csv
          Join = join }

    let onDialect compiler cases =
        cases
        |> Map.tryFind compiler.Dialect
        |> Option.defaultWith (fun () -> failwithf "No SQL dialect case for %A" compiler.Dialect)
        |> fun f -> f ()

    let onDialectOrElse compiler orElse cases =
        cases
        |> Map.tryFind compiler.Dialect
        |> Option.map (fun f -> f ())
        |> Option.defaultWith orElse
