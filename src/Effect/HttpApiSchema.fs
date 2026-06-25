namespace Effect

/// HTTP API schema metadata used by HttpApi endpoints, clients, and OpenAPI.
type HttpApiEncoding =
    | Json
    | FormUrlEncoded
    | Text
    | Uint8Array
    | Multipart

type HttpApiContentKind =
    | Buffered
    | Empty
    | StreamSse of sseMode: string * events: SchemaAst * error: SchemaAst
    | StreamUint8Array

type HttpApiContent =
    { Kind: HttpApiContentKind
      Status: int
      ContentType: string
      Schema: SchemaAst option }

[<RequireQualifiedAccess>]
module HttpApiSchema =

    [<Literal>]
    let streamFailureEvent = "effect/httpapi/stream/failure"

    let private content kind status contentType schema =
        { Kind = kind
          Status = status
          ContentType = contentType
          Schema = schema }

    let asJson (schema: Schema<'T>) : HttpApiContent =
        content Buffered 200 "application/json" (Some schema.Ast)

    let asJsonWithContentType (contentType: string) (schema: Schema<'T>) : HttpApiContent =
        content Buffered 200 contentType (Some schema.Ast)

    let asFormUrlEncoded (schema: Schema<'T>) : HttpApiContent =
        content Buffered 200 "application/x-www-form-urlencoded" (Some schema.Ast)

    let asFormUrlEncodedWithContentType (contentType: string) (schema: Schema<'T>) : HttpApiContent =
        content Buffered 200 contentType (Some schema.Ast)

    let asText (schema: Schema<'T>) : HttpApiContent =
        content Buffered 200 "text/plain" (Some schema.Ast)

    let asTextWithContentType (contentType: string) (schema: Schema<'T>) : HttpApiContent =
        content Buffered 200 contentType (Some schema.Ast)

    let asUint8Array (schema: Schema<'T>) : HttpApiContent =
        content Buffered 200 "application/octet-stream" (Some schema.Ast)

    let asUint8ArrayWithContentType (contentType: string) (schema: Schema<'T>) : HttpApiContent =
        content Buffered 200 contentType (Some schema.Ast)

    let asNoContent (statusCode: int) : HttpApiContent =
        content Empty statusCode "" None

    let noContent: HttpApiContent =
        content Empty 204 "" None

    let created: HttpApiContent =
        content Empty 201 "" None

    let accepted: HttpApiContent =
        content Empty 202 "" None

    let empty (statusCode: int) : HttpApiContent =
        content Empty statusCode "" None

    let status (statusCode: int) (schema: HttpApiContent) : HttpApiContent =
        { schema with Status = statusCode }

    let streamSse (events: Schema<'Event>) (error: Schema<'Error>) : HttpApiContent =
        content (StreamSse("events", events.Ast, error.Ast)) 200 "text/event-stream" None

    let streamSseWithContentType (contentType: string) (events: Schema<'Event>) (error: Schema<'Error>) : HttpApiContent =
        content (StreamSse("events", events.Ast, error.Ast)) 200 contentType None

    let streamSseData (data: Schema<'Data>) (error: Schema<'Error>) : HttpApiContent =
        // For OpenAPI/endpoint validation we only need to know this is an SSE
        // stream whose payload is JSON data.
        content (StreamSse("data", data.Ast, error.Ast)) 200 "text/event-stream" None

    let streamUint8Array: HttpApiContent =
        content StreamUint8Array 200 "application/octet-stream" None

    let streamUint8ArrayWithContentType (contentType: string) : HttpApiContent =
        content StreamUint8Array 200 contentType None

    let isStream (schema: HttpApiContent) : bool =
        match schema.Kind with
        | StreamSse _
        | StreamUint8Array -> true
        | _ -> false

    let isStreamSse (schema: HttpApiContent) : bool =
        match schema.Kind with
        | StreamSse _ -> true
        | _ -> false

    let isStreamUint8Array (schema: HttpApiContent) : bool =
        match schema.Kind with
        | StreamUint8Array -> true
        | _ -> false

    let isNoContent (schema: HttpApiContent) : bool =
        match schema.Kind with
        | Empty -> true
        | _ -> false

    let baseContentType (contentType: string) : string =
        match contentType.IndexOf ';' with
        | -1 -> contentType.Trim().ToLowerInvariant()
        | i -> contentType.Substring(0, i).Trim().ToLowerInvariant()

    let encoding (schema: HttpApiContent) : HttpApiEncoding =
        match schema.Kind with
        | Empty -> Json
        | StreamSse _ -> Text
        | StreamUint8Array -> Uint8Array
        | Buffered ->
            match baseContentType schema.ContentType with
            | "application/json" -> Json
            | "application/x-www-form-urlencoded" -> FormUrlEncoded
            | "text/plain" -> Text
            | "application/octet-stream" -> Uint8Array
            | "multipart/form-data" -> Multipart
            | _ -> Json

    let rec private containsReservedFailureEvent =
        function
        | AObject fields ->
            fields
            |> List.exists (function
                | "event", ALiteral(JString event) -> event = streamFailureEvent
                | _, ast -> containsReservedFailureEvent ast)
        | AArray ast
        | AOption ast
        | ARefine(ast, _)
        | AMeasured ast -> containsReservedFailureEvent ast
        | ATuple asts
        | AUnion asts -> asts |> List.exists containsReservedFailureEvent
        | ATaggedUnion cases -> cases |> List.exists (snd >> containsReservedFailureEvent)
        | _ -> false

    let hasReservedFailureEvent (schema: HttpApiContent) : bool =
        match schema.Kind with
        | StreamSse(_, events, _) -> containsReservedFailureEvent events
        | _ -> false
