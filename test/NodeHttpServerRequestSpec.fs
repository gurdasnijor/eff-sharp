module NodeHttpServerRequestSpec

open Effect
open Effect.Platform.Node
open Effect.Vitest

describe "NodeHttpServerRequest" (fun () ->
    test "returns the stored IncomingMessage source" (fun () ->
        let source = box {| id = 1 |}
        let request = { HttpServerRequest.make "GET" "/" Headers.empty HttpBody.empty with Source = Some source }

        toBe (NodeHttpServerRequest.toIncomingMessage request) source)

    test "throws when no Node source is available" (fun () ->
        let request = HttpServerRequest.make "GET" "/" Headers.empty HttpBody.empty
        toThrow (fun () -> NodeHttpServerRequest.toIncomingMessage request)))
