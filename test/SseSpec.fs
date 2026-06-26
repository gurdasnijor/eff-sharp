module SseSpec

open Effect
open Effect.Vitest

let private events parsed =
    parsed
    |> List.choose (function
        | SseEvent event -> Some event
        | SseRetry _ -> None)

let private retries parsed =
    parsed
    |> List.choose (function
        | SseRetry retry -> Some retry
        | SseEvent _ -> None)

describe "Sse" (fun () ->
    test "encodes message events" (fun () ->
        let encoded = Sse.encodeEvent (Sse.message "hello")
        toBe encoded "data: hello\n\n")

    test "encodes named events with ids and multiline data" (fun () ->
        let encoded =
            Sse.encodeEvent
                { Id = Some "42"
                  Event = "token"
                  Data = "one\ntwo" }

        toBe encoded "id: 42\nevent: token\ndata: one\ndata: two\n\n")

    test "parses events across chunks" (fun () ->
        let parser = Sse.makeParser ()
        let first = parser.Feed("event: token\ndata: hel")
        let second = parser.Feed("lo\n\n")
        toEqual first []

        match events second with
        | [ event ] ->
            toBe event.Event "token"
            toBe event.Data "hello"
            toBe event.Id None
        | other -> failwithf "expected one event, got %A" other)

    test "parses comments, ids, BOM, and CRLF" (fun () ->
        let parser = Sse.makeParser ()
        let parsed = parser.Feed("\uFEFF: ignored\r\nid: abc\r\ndata: one\r\ndata: two\r\n\r\n")

        match events parsed with
        | [ event ] ->
            toBe event.Id (Some "abc")
            toBe event.Event "message"
            toBe event.Data "one\ntwo"
        | other -> failwithf "expected one event, got %A" other)

    test "handles CRLF split across chunks" (fun () ->
        let parser = Sse.makeParser ()
        let first = parser.Feed("data: one\r")
        let second = parser.Feed("\n\r\n")
        toEqual first []

        match events second with
        | [ event ] -> toBe event.Data "one"
        | other -> failwithf "expected one event, got %A" other)

    test "parses retry directives with last event id" (fun () ->
        let parser = Sse.makeParser ()
        let parsed = parser.Feed("id: last\nretry: 1500\n\n")

        match retries parsed with
        | [ retry ] ->
            toBe retry.DurationMillis 1500
            toBe retry.LastEventId (Some "last")
        | other -> failwithf "expected one retry, got %A" other))
