module NodeHttpIncomingMessageSpec

open Effect
open Effect.Platform.Node
open Effect.Vitest
open Fable.Core

[<Emit("({ headers: { cookie: 'sid=abc', 'x-test': 'ok' }, socket: { remoteAddress: '127.0.0.1' }, on: function() { return this; } })")>]
let private fakeIncomingMessage () : obj = jsNative

describe "NodeHttpIncomingMessage" (fun () ->
    test "extracts headers and remote address from a Node message" (fun () ->
        let message = NodeHttpIncomingMessage.make (fakeIncomingMessage ())

        toBe (Headers.get "x-test" message.Headers) (Some "ok")
        toBe message.RemoteAddress (Some "127.0.0.1"))

    test "converts an incoming message wrapper into an HttpServerRequest" (fun () ->
        let message = NodeHttpIncomingMessage.make (fakeIncomingMessage ())
        let request = NodeHttpIncomingMessage.toServerRequest "POST" "/submit" (HttpBody.text "body") message

        toBe request.Method "POST"
        toBe (HttpServerRequest.path request) "/submit"
        toBe request.Cookies.["sid"] "abc"
        toBe request.RemoteAddress (Some "127.0.0.1")))
