namespace Effect

open Fable.Core

/// Parsed multipart/form-data part.
type MultipartPart =
    | MultipartField of key: string * value: string * contentType: string
    | MultipartFile of key: string * name: string * contentType: string * content: Stream<byte[], PlatformError, Context> * source: obj

/// Persisted multipart/form-data request data.
type MultipartPersisted =
    { Fields: Map<string, string>
      Files: Map<string, MultipartPart list> }

/// Multipart-native decoder. Unlike `Schema<'T>`, this decodes from persisted
/// multipart fields/files, so file handles are preserved instead of being
/// coerced through JSON.
type MultipartSchema<'T> =
    { Decode: MultipartPersisted -> Validation<'T>
      Ast: SchemaAst }

type MultipartFieldCodec<'R, 'A> =
    { DecodeField: MultipartPersisted -> Validation<'A>
      Fields: (string * SchemaAst) list }

[<RequireQualifiedAccess>]
module Multipart =

    [<Emit("new FormData()")>]
    let private newFormData () : obj = jsNative

    [<Emit("$0.append($1, $2)")>]
    let private appendFormData (_formData: obj) (_key: string) (_value: obj) : unit = jsNative

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

    let private error issues =
        { Issue =
            match issues with
            | [ issue ] -> issue
            | many -> Composite many }

    let decode (schema: MultipartSchema<'T>) (form: MultipartPersisted) : Result<'T, SchemaError> =
        schema.Decode form |> Result.mapError error

    let erase (schema: MultipartSchema<'T>) : MultipartSchema<obj> =
        { Ast = schema.Ast
          Decode = schema.Decode >> Validation.map box }

    let fieldSchema (key: string) (schema: Schema<'F>) : MultipartFieldCodec<'R, 'F> =
        { Fields = [ key, schema.Ast ]
          DecodeField =
            fun form ->
                match Map.tryFind key form.Fields with
                | None -> Error [ MissingKey key ]
                | Some value -> schema.Decode (JString value) |> Validation.mapError (List.map (fun e -> Pointer([ key ], e))) }

    let filesSchema (key: string) : MultipartFieldCodec<'R, MultipartPart list> =
        { Fields = [ key, AArray(ADeclare "MultipartFile") ]
          DecodeField =
            fun form ->
                form.Files
                |> Map.tryFind key
                |> Option.defaultValue []
                |> Ok }

    type ObjectBuilder() =
        member _.MergeSources(a: MultipartFieldCodec<'R, 'A>, b: MultipartFieldCodec<'R, 'B>) : MultipartFieldCodec<'R, 'A * 'B> =
            { Fields = a.Fields @ b.Fields
              DecodeField = fun form -> Validation.zip (a.DecodeField form) (b.DecodeField form) }

        member _.BindReturn(c: MultipartFieldCodec<'R, 'A>, f: 'A -> 'R) : MultipartSchema<'R> =
            { Ast = AObject c.Fields
              Decode = fun form -> c.DecodeField form |> Validation.map f }

    let object = ObjectBuilder()

    let toFormDataBody (form: MultipartPersisted) : HttpBody =
        let formData = newFormData ()

        form.Fields
        |> Map.iter (fun key value -> appendFormData formData key (box value))

        form.Files
        |> Map.iter (fun key files ->
            files
            |> List.iter (function
                | MultipartFile(_, name, _, _, source) -> appendFormData formData key source
                | MultipartField(_, value, _) -> appendFormData formData key (box value)))

        HttpBody.formData formData
