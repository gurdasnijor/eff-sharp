namespace Effect

open System

[<RequireQualifiedAccess>]
module SqlSchema =

    exception NoSuchElementError of string

    let rec valueToJson (value: obj) : Json =
        if obj.ReferenceEquals(value, null) then
            JNull
        else
            match value with
            | :? Json as json -> json
            | :? string as text -> JString text
            | :? bool as boolean -> JBool boolean
            | :? int as number -> JNumber(float number)
            | :? int64 as number -> JNumber(float number)
            | :? float as number -> JNumber number
            | :? decimal as number -> JNumber(float number)
            | :? (obj list) as values -> JArray(List.map valueToJson values)
            | :? (obj[]) as values -> JArray(values |> Array.toList |> List.map valueToJson)
            | _ -> JString(string value)

    and rowToJson (row: Row) : Json =
        row
        |> Map.toList
        |> List.map (fun (key, value) -> key, valueToJson value)
        |> Map.ofList
        |> JObject

    let private decodeRow (schema: Schema<'A>) (row: Row) : Effect<'A, obj, 'R> =
        rowToJson row
        |> Schema.decodeEffect schema
        |> Effect.mapError box

    let private decodeRows (schema: Schema<'A>) (rows: Row list) : Effect<'A list, obj, 'R> =
        Effect.forEach rows (decodeRow schema)

    let private encodeRequest (schema: Schema<'Req>) (request: 'Req) : Effect<Json, obj, 'R> =
        Schema.encodeEffect schema request |> Effect.mapError box

    let findAll
        (requestSchema: Schema<'Req>)
        (resultSchema: Schema<'Res>)
        (execute: Json -> Effect<Row list, 'E, 'R>)
        (request: 'Req)
        : Effect<'Res list, obj, 'R> =
        encodeRequest requestSchema request
        |> Effect.flatMap (fun encoded -> execute encoded |> Effect.mapError box)
        |> Effect.flatMap (decodeRows resultSchema)

    let findNonEmpty
        (requestSchema: Schema<'Req>)
        (resultSchema: Schema<'Res>)
        (execute: Json -> Effect<Row list, 'E, 'R>)
        (request: 'Req)
        : Effect<'Res list, obj, 'R> =
        findAll requestSchema resultSchema execute request
        |> Effect.flatMap (fun results ->
            match results with
            | [] -> Effect.fail (box (NoSuchElementError "Expected at least one SQL result row"))
            | _ -> Effect.succeed results)

    let findOne
        (requestSchema: Schema<'Req>)
        (resultSchema: Schema<'Res>)
        (execute: Json -> Effect<Row list, 'E, 'R>)
        (request: 'Req)
        : Effect<'Res, obj, 'R> =
        encodeRequest requestSchema request
        |> Effect.flatMap (fun encoded -> execute encoded |> Effect.mapError box)
        |> Effect.flatMap (function
            | head :: _ -> decodeRow resultSchema head
            | [] -> Effect.fail (box (NoSuchElementError "Expected one SQL result row")))

    let findOneOption
        (requestSchema: Schema<'Req>)
        (resultSchema: Schema<'Res>)
        (execute: Json -> Effect<Row list, 'E, 'R>)
        (request: 'Req)
        : Effect<'Res option, obj, 'R> =
        encodeRequest requestSchema request
        |> Effect.flatMap (fun encoded -> execute encoded |> Effect.mapError box)
        |> Effect.flatMap (function
            | head :: _ -> decodeRow resultSchema head |> Effect.map Some
            | [] -> Effect.succeed None)

    let void'
        (requestSchema: Schema<'Req>)
        (execute: Json -> Effect<'A, 'E, 'R>)
        (request: 'Req)
        : Effect<unit, obj, 'R> =
        encodeRequest requestSchema request
        |> Effect.flatMap (fun encoded -> execute encoded |> Effect.mapError box |> Effect.asVoid)
