module HttpApiSchemaSpec

open Effect
open Effect.Vitest

type Event = { Event: string; Data: string }
type ErrorBody = { Reason: string }
type Payload = { Id: string }

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

let private payloadSchema: Schema<Payload> =
    Schema.object {
        let! id = Schema.field "id" Schema.string (fun p -> p.Id)
        return { Id = id }
    }

describe "HttpApiSchema" (fun () ->
    describe "StreamSse" (fun () ->
        test "stores default metadata" (fun () ->
            let stream = HttpApiSchema.streamSse eventsSchema errorSchema

            toBe (HttpApiSchema.isStream stream) true
            toBe (HttpApiSchema.isStreamSse stream) true
            toBe (HttpApiSchema.isStreamUint8Array stream) false
            toBe stream.Status 200
            toBe stream.ContentType "text/event-stream"

            match stream.Payload with
            | StreamSse(mode, events, failure) ->
                toBe mode "events"
                toBe (events.Ast = eventsSchema.Ast) true
                toBe (failure.Ast = (Schema.cause errorSchema).Ast) true

                match events.Decode (JObject(Map.ofList [ "event", JString "message"; "data", JString "hello" ])) with
                | Ok value ->
                    let event = unbox<Event> value
                    toBe event.Event "message"
                    toBe event.Data "hello"
                | Error issues -> failwithf "expected erased event codec to decode, got %A" issues

                let failureJson = Schema.encode (Schema.cause errorSchema) (Cause.fail { Reason = "boom" })

                match failure.Decode failureJson with
                | Ok value ->
                    let cause = unbox<Cause<obj>> value
                    match Cause.failures cause with
                    | [ error ] -> toBe (unbox<ErrorBody> error).Reason "boom"
                    | other -> failwithf "expected one failure, got %A" other
                | Error issues -> failwithf "expected erased failure cause codec to decode, got %A" issues
            | _ -> failwith "expected StreamSse")

        test "stores custom content type" (fun () ->
            let stream =
                HttpApiSchema.streamSseWithContentType "text/event-stream; charset=utf-8" eventsSchema errorSchema

            toBe stream.ContentType "text/event-stream; charset=utf-8")

        test "creates data-mode SSE metadata from a JSON data schema" (fun () ->
            let stream = HttpApiSchema.streamSseData payloadSchema errorSchema

            match stream.Payload with
            | StreamSse(mode, data, failure) ->
                toBe mode "data"
                toBe (data.Ast = payloadSchema.Ast) true
                toBe (failure.Ast = (Schema.cause errorSchema).Ast) true
            | _ -> failwith "expected StreamSse"))

    describe "StreamUint8Array" (fun () ->
        test "stores default metadata" (fun () ->
            let stream = HttpApiSchema.streamUint8Array

            toBe (HttpApiSchema.isStream stream) true
            toBe (HttpApiSchema.isStreamSse stream) false
            toBe (HttpApiSchema.isStreamUint8Array stream) true
            toBe stream.Status 200
            toBe stream.ContentType "application/octet-stream")

        test "stores custom content type" (fun () ->
            let stream = HttpApiSchema.streamUint8ArrayWithContentType "application/custom-binary"
            toBe stream.ContentType "application/custom-binary"))

    test "does not identify buffered schemas as stream schemas" (fun () ->
        let buffered = HttpApiSchema.asJson Schema.string
        toBe (HttpApiSchema.isStream buffered) false))
