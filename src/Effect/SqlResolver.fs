namespace Effect

open System.Collections.Generic

type SqlRequest<'In, 'A, 'R> =
    { Payload: 'In }

    interface Request<'A, obj, 'R>

[<RequireQualifiedAccess>]
module SqlResolver =

    let makeRequest payload : SqlRequest<'In, 'A, 'R> = { Payload = payload }

    let request (resolver: RequestResolver<'A, obj, 'R>) (payload: 'In) : Effect<'A, obj, 'R> =
        RequestResolver.resolve resolver (makeRequest payload :> Request<'A, obj, 'R>)

    let private payload<'In, 'A, 'R> (entry: Entry<'A, obj, 'R>) =
        (entry.Request :?> SqlRequest<'In, 'A, 'R>).Payload

    let private encodeInputs (schema: Schema<'In>) (entries: Entry<'A, obj, 'R> list) : Json list =
        entries |> List.map (fun entry -> Schema.encode schema (payload<'In, 'A, 'R> entry))

    let private decodeResult (schema: Schema<'A>) (row: Row) : Result<'A, obj> =
        SqlSchema.rowToJson row
        |> Schema.decode schema
        |> Result.mapError box

    let private decodeResults (schema: Schema<'A>) (values: Row list) : Result<'A list, obj> =
        let folder value acc =
            match acc, decodeResult schema value with
            | Ok values, Ok decoded -> Ok(decoded :: values)
            | Error error, _ -> Error error
            | _, Error error -> Error error

        values |> List.foldBack folder <| Ok []

    let ordered
        (requestSchema: Schema<'Req>)
        (resultSchema: Schema<'Res>)
        (execute: Json list -> Effect<Row list, 'E, 'R>)
        : RequestResolver<'Res, obj, 'R> =
        RequestResolver.make (fun entries ->
            let inputs = encodeInputs requestSchema entries

            execute inputs
            |> Effect.mapError box
            |> Effect.flatMap (fun rows ->
                if List.length rows <> List.length inputs then
                    Effect.fail (box { Expected = List.length inputs; Actual = List.length rows })
                else
                    match decodeResults resultSchema rows with
                    | Ok decoded ->
                        Effect.sync (fun () ->
                            List.zip entries decoded
                            |> List.iter (fun (entry, value) -> entry.CompleteUnsafe(Exit.succeed value)))
                    | Error error -> Effect.fail error))

    let grouped
        (requestSchema: Schema<'Req>)
        (requestGroupKey: 'Req -> 'Key)
        (resultSchema: Schema<'Res>)
        (resultGroupKey: 'Res -> Row -> 'Key)
        (execute: Json list -> Effect<Row list, 'E, 'R>)
        : RequestResolver<'Res list, obj, 'R>
        when 'Key: equality =
        RequestResolver.make (fun entries ->
            let inputs = encodeInputs requestSchema entries

            execute inputs
            |> Effect.mapError box
            |> Effect.flatMap (fun rows ->
                match decodeResults resultSchema rows with
                | Error error -> Effect.fail error
                | Ok decoded ->
                    Effect.sync (fun () ->
                        let groups = Dictionary<'Key, ResizeArray<'Res>>()

                        List.zip decoded rows
                        |> List.iter (fun (result, row) ->
                            let key = resultGroupKey result row

                            match groups.TryGetValue key with
                            | true, existing -> existing.Add result
                            | false, _ -> groups.[key] <- ResizeArray [ result ])

                        for entry in entries do
                            let key = payload<'Req, 'Res list, 'R> entry |> requestGroupKey

                            match groups.TryGetValue key with
                            | true, values when values.Count > 0 -> entry.CompleteUnsafe(Exit.succeed (List.ofSeq values))
                            | _ ->
                                entry.CompleteUnsafe(
                                    Exit.fail (box (SqlSchema.NoSuchElementError "Expected a SQL result group"))
                                ))))

    let findById
        (idSchema: Schema<'Id>)
        (resultSchema: Schema<'Res>)
        (resultId: 'Res -> Row -> 'Id)
        (execute: Json list -> Effect<Row list, 'E, 'R>)
        : RequestResolver<'Res, obj, 'R>
        when 'Id: equality =
        RequestResolver.make (fun entries ->
            let inputs = encodeInputs idSchema entries

            execute inputs
            |> Effect.mapError box
            |> Effect.flatMap (fun rows ->
                match decodeResults resultSchema rows with
                | Error error -> Effect.fail error
                | Ok decoded ->
                    Effect.sync (fun () ->
                        let byId = Dictionary<'Id, 'Res>()

                        List.zip decoded rows
                        |> List.iter (fun (result, row) -> byId.[resultId result row] <- result)

                        for entry in entries do
                            let id = payload<'Id, 'Res, 'R> entry

                            match byId.TryGetValue id with
                            | true, result -> entry.CompleteUnsafe(Exit.succeed result)
                            | false, _ ->
                                entry.CompleteUnsafe(
                                    Exit.fail (box (SqlSchema.NoSuchElementError "Expected a SQL result row"))
                                ))))

    let void'
        (requestSchema: Schema<'Req>)
        (execute: Json list -> Effect<Row list, 'E, 'R>)
        : RequestResolver<unit, obj, 'R> =
        RequestResolver.make (fun entries ->
            let inputs = encodeInputs requestSchema entries

            execute inputs
            |> Effect.mapError box
            |> Effect.flatMap (fun _ ->
                Effect.sync (fun () ->
                    entries |> List.iter (fun entry -> entry.CompleteUnsafe(Exit.succeed ())))))
