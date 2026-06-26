namespace Effect

/// HTTP API schema metadata used by HttpApi endpoints, clients, and OpenAPI.
type HttpApiEncoding =
    | Json
    | FormUrlEncoded
    | Text
    | Uint8Array
    | Multipart

type HttpApiPayload =
    | Buffered of Schema<obj>
    | Empty
    | StreamSse of sseMode: string * events: Schema<obj> * failure: Schema<obj>
    | StreamBytes

type HttpApiContent =
    { Status: int
      ContentType: string
      Payload: HttpApiPayload }

[<RequireQualifiedAccess>]
module HttpApiSchema =

    [<Literal>]
    let streamFailureEvent = "effect/httpapi/stream/failure"

    let private content payload status contentType =
        { Status = status
          ContentType = contentType
          Payload = payload }

    let private causePayload (error: Schema<'Error>) : Schema<obj> =
        let cause = Schema.cause error

        { Ast = cause.Ast
          Decode = fun json -> cause.Decode json |> Result.map (Cause.map box >> box)
          Encode = fun value -> cause.Encode (unbox<Cause<obj>> value |> Cause.map unbox<'Error>) }

    let asJson (schema: Schema<'T>) : HttpApiContent =
        content (Buffered(Schema.erase schema)) 200 "application/json"

    let asJsonWithContentType (contentType: string) (schema: Schema<'T>) : HttpApiContent =
        content (Buffered(Schema.erase schema)) 200 contentType

    let asFormUrlEncoded (schema: Schema<'T>) : HttpApiContent =
        content (Buffered(Schema.erase schema)) 200 "application/x-www-form-urlencoded"

    let asFormUrlEncodedWithContentType (contentType: string) (schema: Schema<'T>) : HttpApiContent =
        content (Buffered(Schema.erase schema)) 200 contentType

    let asText (schema: Schema<'T>) : HttpApiContent =
        content (Buffered(Schema.erase schema)) 200 "text/plain"

    let asTextWithContentType (contentType: string) (schema: Schema<'T>) : HttpApiContent =
        content (Buffered(Schema.erase schema)) 200 contentType

    let asUint8Array (schema: Schema<'T>) : HttpApiContent =
        content (Buffered(Schema.erase schema)) 200 "application/octet-stream"

    let asUint8ArrayWithContentType (contentType: string) (schema: Schema<'T>) : HttpApiContent =
        content (Buffered(Schema.erase schema)) 200 contentType

    let asMultipart (schema: Schema<'T>) : HttpApiContent =
        content (Buffered(Schema.erase schema)) 200 "multipart/form-data"

    let asMultipartWithContentType (contentType: string) (schema: Schema<'T>) : HttpApiContent =
        content (Buffered(Schema.erase schema)) 200 contentType

    let asNoContent (statusCode: int) : HttpApiContent =
        content Empty statusCode ""

    let noContent: HttpApiContent =
        content Empty 204 ""

    let created: HttpApiContent =
        content Empty 201 ""

    let accepted: HttpApiContent =
        content Empty 202 ""

    let empty (statusCode: int) : HttpApiContent =
        content Empty statusCode ""

    let status (statusCode: int) (schema: HttpApiContent) : HttpApiContent =
        { schema with Status = statusCode }

    let streamSse (events: Schema<'Event>) (error: Schema<'Error>) : HttpApiContent =
        content (StreamSse("events", Schema.erase events, causePayload error)) 200 "text/event-stream"

    let streamSseWithContentType (contentType: string) (events: Schema<'Event>) (error: Schema<'Error>) : HttpApiContent =
        content (StreamSse("events", Schema.erase events, causePayload error)) 200 contentType

    let streamSseData (data: Schema<'Data>) (error: Schema<'Error>) : HttpApiContent =
        content (StreamSse("data", Schema.erase data, causePayload error)) 200 "text/event-stream"

    let streamUint8Array: HttpApiContent =
        content StreamBytes 200 "application/octet-stream"

    let streamUint8ArrayWithContentType (contentType: string) : HttpApiContent =
        content StreamBytes 200 contentType

    let isStream (schema: HttpApiContent) : bool =
        match schema.Payload with
        | StreamSse _
        | StreamBytes -> true
        | _ -> false

    let isStreamSse (schema: HttpApiContent) : bool =
        match schema.Payload with
        | StreamSse _ -> true
        | _ -> false

    let isStreamUint8Array (schema: HttpApiContent) : bool =
        match schema.Payload with
        | StreamBytes -> true
        | _ -> false

    let isNoContent (schema: HttpApiContent) : bool =
        match schema.Payload with
        | Empty -> true
        | _ -> false

    let baseContentType (contentType: string) : string =
        match contentType.IndexOf ';' with
        | -1 -> contentType.Trim().ToLowerInvariant()
        | i -> contentType.Substring(0, i).Trim().ToLowerInvariant()

    let encoding (schema: HttpApiContent) : HttpApiEncoding =
        match schema.Payload with
        | Empty -> Json
        | StreamSse _ -> Text
        | StreamBytes -> Uint8Array
        | Buffered _ ->
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
        match schema.Payload with
        | StreamSse(_, events, _) -> containsReservedFailureEvent events.Ast
        | _ -> false
