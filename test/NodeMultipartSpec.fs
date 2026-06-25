module NodeMultipartSpec

open Effect
open Effect.Platform.Node
open Effect.Vitest
open Fable.Core

[<Import("Readable", "node:stream")>]
let private readable: obj = jsNative

[<Emit("$0.from([$1])")>]
let private readableFromString (_readable: obj) (_body: string) : obj = jsNative

describe "NodeMultipart" (fun () ->
    test "constructs field parts" (fun () ->
        match NodeMultipart.field "name" "value" "text/plain" with
        | MultipartField(key, value, contentType) ->
            toBe key "name"
            toBe value "value"
            toBe contentType "text/plain"
        | MultipartFile _ -> failwith "expected field")

    test "returns readable source for file parts" (fun () ->
        let source = box {| stream = true |}
        let part = NodeMultipart.file "upload" "a.txt" "text/plain" source
        toBe (NodeMultipart.fileToReadable part) source)

    itEffect "parses multipart fields with multipasta" (fun () ->
        effect {
            let boundary = "eff-sharp-boundary"

            let body =
                "--"
                + boundary
                + "\r\nContent-Disposition: form-data; name=\"title\"\r\n\r\nhello\r\n--"
                + boundary
                + "--\r\n"

            let source = readableFromString readable body

            let! form =
                NodeMultipart.persisted
                    source
                    (Headers.ofList [ "content-type", "multipart/form-data; boundary=" + boundary ])

            toBe form.Fields.["title"] "hello"
        }))
