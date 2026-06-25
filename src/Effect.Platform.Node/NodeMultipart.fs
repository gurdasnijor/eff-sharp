namespace Effect.Platform.Node

open Effect
open Fable.Core

type MultipartPart =
    | MultipartField of key: string * value: string * contentType: string
    | MultipartFile of key: string * name: string * contentType: string * content: Stream<byte[], PlatformError, Context> * source: obj

type MultipartPersisted =
    { Fields: Map<string, string>
      Files: Map<string, MultipartPart list> }

[<RequireQualifiedAccess>]
module NodeMultipart =

    [<Import("make", "multipasta/node")>]
    let private makeParser (_config: obj) : obj = jsNative

    [<Import("decodeField", "multipasta/node")>]
    let private decodeFieldJs (_info: obj) (_value: byte[]) : string = jsNative

    [<Emit("({ headers: Object.fromEntries($0) })")>]
    let private configFromHeaders (_headers: (string * string) array) : obj = jsNative

    [<Emit("$0.pipe($1)")>]
    let private pipeToParser (_source: obj) (_parser: obj) : unit = jsNative

    [<Emit("$0._tag")>]
    let private partTag (_part: obj) : string = jsNative

    [<Emit("$0.info")>]
    let private partInfo (_part: obj) : obj = jsNative

    [<Emit("$0.value")>]
    let private fieldValue (_part: obj) : byte[] = jsNative

    [<Emit("$0.name")>]
    let private infoName (_info: obj) : string = jsNative

    [<Emit("$0.contentType")>]
    let private infoContentType (_info: obj) : string = jsNative

    [<Emit("$0.filename || ($0.info && $0.info.filename) || ($0.info && $0.info.name) || 'file'")>]
    let private fileName (_file: obj) : string = jsNative

    let private parserError methodName (ex: exn) =
        PlatformError.systemError
            { Tag = SystemErrorTag.InvalidData
              Module = "NodeMultipart"
              Method = methodName
              Description = Some ex.Message
              Syscall = None
              PathOrDescriptor = None
              Cause = Some(box ex) }

    let private convertPart (part: obj) =
        match partTag part with
        | "Field" ->
            let info = partInfo part
            MultipartField(infoName info, decodeFieldJs info (fieldValue part), infoContentType info)
        | "File" ->
            let info = partInfo part
            MultipartFile(infoName info, fileName part, infoContentType info, NodeStream.fromReadable (fun () -> part), part)
        | tag -> invalidOp ("Unknown multipasta part tag: " + tag)

    let field key value contentType =
        MultipartField(key, value, contentType)

    let file key name contentType source =
        MultipartFile(key, name, contentType, NodeStream.fromReadable (fun () -> source), source)

    let fileToReadable (part: MultipartPart) : obj =
        match part with
        | MultipartFile(_, _, _, _, source) -> source
        | MultipartField _ -> invalidArg "part" "Multipart field parts do not have an underlying readable stream"

    let stream (_source: obj) (_headers: Headers) : Stream<MultipartPart, PlatformError, Context> =
        let parser = makeParser (configFromHeaders (_headers |> Headers.toSeq |> Seq.toArray))
        pipeToParser _source parser

        Stream.fromAsyncIterableJS (unbox<JS.AsyncIterable<obj>> parser) (parserError "stream")
        |> Stream.map convertPart

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
