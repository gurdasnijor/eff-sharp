module HttpApiEndpointSpec

open Effect
open Effect.Vitest

type Event = { Event: string; Data: string }
type ErrorBody = { Reason: string }
type OkBody = { Ok: bool }
type ReservedEvent = { Event: Json; Data: string }

let private eventsSchema: Schema<Event> =
    Schema.object {
        let! event = Schema.field "event" Schema.string (fun e -> e.Event)
        and! data = Schema.field "data" Schema.string (fun e -> e.Data)
        return { Event = event; Data = data }
    }

let private errorSchema: Schema<ErrorBody> =
    Schema.object {
        let! reason = Schema.field "reason" Schema.string (fun e -> e.Reason)
        return { Reason = reason }
    }

let private okSchema: Schema<OkBody> =
    Schema.object {
        let! ok = Schema.field "ok" Schema.bool (fun o -> o.Ok)
        return { Ok = ok }
    }

let private reservedEventsSchema: Schema<ReservedEvent> =
    Schema.object {
        let! event =
            Schema.field
                "event"
                (Schema.literal (JString HttpApiSchema.streamFailureEvent))
                (fun e -> e.Event)

        and! data = Schema.field "data" Schema.string (fun e -> e.Data)
        return { Event = event; Data = data }
    }

let private sse () = HttpApiSchema.streamSse eventsSchema errorSchema
let private endpoint success = HttpApiEndpoint.get "events" "/events" { HttpApiEndpoint.empty with Success = success }

describe "HttpApiEndpoint streaming success schemas" (fun () ->
    test "GET endpoint accepts StreamSse success" (fun () ->
        let stream = sse ()
        let endpoint = endpoint [ stream ]
        toBe (endpoint.Options.Success |> List.contains stream) true)

    test "GET endpoint accepts StreamUint8Array success" (fun () ->
        let stream = HttpApiSchema.streamUint8Array
        let endpoint = HttpApiEndpoint.get "download" "/download" { HttpApiEndpoint.empty with Success = [ stream ] }
        toBe (endpoint.Options.Success |> List.contains stream) true)

    test "streaming schema in error throws during endpoint construction" (fun () ->
        toThrow (fun () ->
            HttpApiEndpoint.get "events" "/events" { HttpApiEndpoint.empty with Error = [ sse () ] } |> ignore))

    test "HEAD with streaming success throws" (fun () ->
        toThrow (fun () -> HttpApiEndpoint.head "events" "/events" { HttpApiEndpoint.empty with Success = [ sse () ] } |> ignore))

    test "streaming success mixed with NoContent at the same status throws" (fun () ->
        toThrow (fun () ->
            endpoint [ sse (); HttpApiSchema.noContent |> HttpApiSchema.status 200 ] |> ignore))

    test "streaming success mixed with a buffered success at the same status is allowed for distinct content types" (fun () ->
        let stream = sse ()
        let endpoint = endpoint [ stream; HttpApiSchema.asJson okSchema ]
        toBe (endpoint.Options.Success |> List.contains stream) true)

    test "streaming success mixed with a buffered success at the same content type throws" (fun () ->
        let stream = HttpApiSchema.streamSseWithContentType "application/json" eventsSchema errorSchema
        toThrow (fun () -> endpoint [ stream; HttpApiSchema.asJson okSchema ] |> ignore))

    test "streaming success mixed with a buffered success at content types differing only by parameters throws" (fun () ->
        let stream = HttpApiSchema.streamSseWithContentType "application/json; charset=utf-8" eventsSchema errorSchema
        toThrow (fun () -> endpoint [ stream; HttpApiSchema.asJson okSchema ] |> ignore))

    test "streaming success mixed with a buffered success at distinct statuses is allowed" (fun () ->
        let stream = sse () |> HttpApiSchema.status 206
        let endpoint = endpoint [ stream; HttpApiSchema.asJson okSchema ]
        toBe (endpoint.Options.Success |> List.contains stream) true)

    test "streaming success mixed with NoContent at distinct statuses is allowed" (fun () ->
        let stream = sse () |> HttpApiSchema.status 200
        let endpoint = endpoint [ stream; HttpApiSchema.noContent ]
        toBe (endpoint.Options.Success |> List.contains stream) true)

    test "two streaming successes for the same status throw" (fun () ->
        toThrow (fun () ->
            endpoint [ sse (); HttpApiSchema.streamUint8ArrayWithContentType "application/custom-stream" ] |> ignore))

    test "two streaming successes for distinct statuses are allowed" (fun () ->
        let stream = sse () |> HttpApiSchema.status 206
        let bytes =
            HttpApiSchema.streamUint8ArrayWithContentType "application/custom-stream"
            |> HttpApiSchema.status 200

        let endpoint = endpoint [ stream; bytes ]
        toBe (endpoint.Options.Success |> List.contains stream) true
        toBe (endpoint.Options.Success |> List.contains bytes) true)

    test "statically detectable SSE reserved failure event name throws" (fun () ->
        let stream = HttpApiSchema.streamSse reservedEventsSchema errorSchema
        toThrow (fun () -> endpoint [ stream ] |> ignore)))
