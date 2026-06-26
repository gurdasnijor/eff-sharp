namespace Effect

/// Parsed multipart/form-data part.
type MultipartPart =
    | MultipartField of key: string * value: string * contentType: string
    | MultipartFile of key: string * name: string * contentType: string * content: Stream<byte[], PlatformError, Context> * source: obj

/// Persisted multipart/form-data request data.
type MultipartPersisted =
    { Fields: Map<string, string>
      Files: Map<string, MultipartPart list> }

[<RequireQualifiedAccess>]
module Multipart =

    let field key value contentType =
        MultipartField(key, value, contentType)

    let file key name contentType content source =
        MultipartFile(key, name, contentType, content, source)

    let private fileJson (part: MultipartPart) =
        match part with
        | MultipartFile(_, name, contentType, _, _) ->
            JObject(
                Map.ofList
                    [ "name", JString name
                      "contentType", JString contentType ]
            )
        | MultipartField _ -> JNull

    let toJson (form: MultipartPersisted) : Json =
        let fields =
            form.Fields
            |> Map.toList
            |> List.map (fun (key, value) -> key, JString value)

        let files =
            form.Files
            |> Map.toList
            |> List.map (fun (key, values) ->
                let value =
                    match values with
                    | [ file ] -> fileJson file
                    | many -> JArray(List.map fileJson many)

                key, value)

        JObject(Map.ofList (fields @ files))

    let decodePersisted (schema: Schema<'T>) (form: MultipartPersisted) : Result<'T, SchemaError> =
        Schema.decode schema (toJson form)
