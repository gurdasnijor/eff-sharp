namespace Effect

open System
open System.Globalization

/// HTTP request/response body values.
type HttpBody =
    | EmptyBody
    | TextBody of body: string * contentType: string option
    | JsonBody of body: Json
    | BytesBody of body: byte array * contentType: string option
    | StreamBody of body: Stream<byte[], obj, Context> * contentType: string option
    | FormDataBody of formData: obj

[<RequireQualifiedAccess>]
module HttpBody =

    let empty: HttpBody = EmptyBody

    let text (body: string) : HttpBody = TextBody(body, Some "text/plain; charset=utf-8")

    let textWithContentType (contentType: string) (body: string) : HttpBody = TextBody(body, Some contentType)

    let json (body: Json) : HttpBody = JsonBody body

    let bytes (body: byte array) : HttpBody = BytesBody(body, None)

    let bytesWithContentType (contentType: string) (body: byte array) : HttpBody = BytesBody(body, Some contentType)

    let streamBytes (body: Stream<byte[], obj, Context>) : HttpBody =
        StreamBody(body, None)

    let streamBytesWithContentType (contentType: string) (body: Stream<byte[], obj, Context>) : HttpBody =
        StreamBody(body, Some contentType)

    let formData (body: obj) : HttpBody =
        FormDataBody body

    let isEmpty (body: HttpBody) : bool =
        match body with
        | EmptyBody -> true
        | _ -> false

    let isStream (body: HttpBody) : bool =
        match body with
        | StreamBody _ -> true
        | _ -> false

    let contentType (body: HttpBody) : string option =
        match body with
        | EmptyBody -> None
        | TextBody(_, contentType) -> contentType
        | JsonBody _ -> Some "application/json"
        | BytesBody(_, contentType) -> contentType
        | StreamBody(_, contentType) -> contentType
        | FormDataBody _ -> None

    let contentLength (body: HttpBody) : int option =
        match body with
        | EmptyBody -> Some 0
        | TextBody(value, _) -> Some value.Length
        | JsonBody _ -> None
        | BytesBody(value, _) -> Some value.Length
        | StreamBody _ -> None
        | FormDataBody _ -> None

    let private quoteJsonString (value: string) : string =
        let mutable escaped = value.Replace("\\", "\\\\")
        escaped <- escaped.Replace("\"", "\\\"")
        escaped <- escaped.Replace("\b", "\\b")
        escaped <- escaped.Replace("\f", "\\f")
        escaped <- escaped.Replace("\n", "\\n")
        escaped <- escaped.Replace("\r", "\\r")
        escaped <- escaped.Replace("\t", "\\t")
        "\"" + escaped + "\""

    let rec toJsonString (json: Json) : string =
        match json with
        | JNull -> "null"
        | JBool value -> if value then "true" else "false"
        | JNumber value ->
            if Double.IsNaN value || Double.IsInfinity value then
                "null"
            else
                value.ToString(CultureInfo.InvariantCulture)
        | JString value -> quoteJsonString value
        | JArray values ->
            values
            |> List.map toJsonString
            |> String.concat ","
            |> fun value -> "[" + value + "]"
        | JObject fields ->
            fields
            |> Map.toList
            |> List.map (fun (key, value) -> quoteJsonString key + ":" + toJsonString value)
            |> String.concat ","
            |> fun value -> "{" + value + "}"

    let encodedText (body: HttpBody) : string option =
        match body with
        | EmptyBody -> None
        | TextBody(value, _) -> Some value
        | JsonBody value -> Some(toJsonString value)
        | BytesBody _ -> None
        | StreamBody _ -> None
        | FormDataBody _ -> None

    let asText (body: HttpBody) : string option =
        match body with
        | EmptyBody -> Some ""
        | TextBody(value, _) -> Some value
        | JsonBody _ -> None
        | BytesBody _ -> None
        | StreamBody _ -> None
        | FormDataBody _ -> None

    let asStream (body: HttpBody) : Stream<byte[], obj, Context> option =
        match body with
        | StreamBody(stream, _) -> Some stream
        | _ -> None
