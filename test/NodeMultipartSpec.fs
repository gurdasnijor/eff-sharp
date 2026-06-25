module NodeMultipartSpec

open Effect
open Effect.Platform.Node
open Effect.Vitest

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

    test "reports parser support as an explicit platform error" (fun () ->
        let result: Exit<MultipartPersisted, PlatformError> =
            NodeMultipart.persisted (box null) Headers.empty
            |> Effect.runSync Runtime.defaultRuntime.Context

        match result with
        | Failure cause ->
            match Cause.failures cause with
            | (error: PlatformError) :: _ ->
                toBe error.Tag "PlatformError"
                toBe error.Message "NodeMultipart.stream: multipart/form-data parsing depends on the upstream multipasta parser, which is not ported yet"
            | _ -> failwith "expected PlatformError"
        | Success _ -> failwith "expected failure"))
