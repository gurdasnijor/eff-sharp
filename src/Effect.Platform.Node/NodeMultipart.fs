namespace Effect.Platform.Node

open Effect

type MultipartPart =
    | MultipartField of key: string * value: string * contentType: string
    | MultipartFile of key: string * name: string * contentType: string * content: Stream<byte[], PlatformError, Context> * source: obj

type MultipartPersisted =
    { Fields: Map<string, string>
      Files: Map<string, MultipartPart list> }

[<RequireQualifiedAccess>]
module NodeMultipart =

    let field key value contentType =
        MultipartField(key, value, contentType)

    let file key name contentType source =
        MultipartFile(key, name, contentType, NodeStream.fromReadable (fun () -> source), source)

    let fileToReadable (part: MultipartPart) : obj =
        match part with
        | MultipartFile(_, _, _, _, source) -> source
        | MultipartField _ -> invalidArg "part" "Multipart field parts do not have an underlying readable stream"

    let private parserUnavailable () =
        PlatformError.badArgument
            { Module = "NodeMultipart"
              Method = "stream"
              Description = Some "multipart/form-data parsing depends on the upstream multipasta parser, which is not ported yet"
              Cause = None }

    let stream (_source: obj) (_headers: Headers) : Stream<MultipartPart, PlatformError, Context> =
        Stream.fromEffect (Effect.fail (parserUnavailable ()))

    let persisted (source: obj) (headers: Headers) : Effect<MultipartPersisted, PlatformError, Context> =
        stream source headers
        |> Stream.runCollect
        |> Effect.map (fun parts ->
            let fields =
                parts
                |> List.choose (function
                    | MultipartField(key, value, _) -> Some(key, value)
                    | MultipartFile _ -> None)
                |> Map.ofList

            let files =
                parts
                |> List.choose (function
                    | MultipartFile(key, _, _, _, _) as part -> Some(key, part)
                    | MultipartField _ -> None)
                |> List.groupBy fst
                |> List.map (fun (key, values) -> key, values |> List.map snd)
                |> Map.ofList

            { Fields = fields; Files = files })
