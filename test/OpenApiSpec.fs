module OpenApiSpec

open Effect
open Effect.Vitest

type Message = { Message: string }
type Event = { Event: string; Data: string }
type ErrorBody = { Reason: string }

let private messageSchema: Schema<Message> =
    Schema.object {
        let! message = Schema.field "message" Schema.string (fun m -> m.Message)
        return { Message = message }
    }

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

let private getObject key =
    function
    | JObject m -> m |> Map.find key
    | other -> failwithf "expected object when reading %s, got %A" key other

let private asObject =
    function
    | JObject m -> m
    | other -> failwithf "expected object, got %A" other

describe "OpenApi" (fun () ->
    test "emits buffered and stream successes with the same status" (fun () ->
        let endpoint =
            HttpApiEndpoint.get
                "chat"
                "/chat"
                { HttpApiEndpoint.empty with
                    Success =
                        [ HttpApiSchema.asJson messageSchema
                          HttpApiSchema.streamSse eventsSchema errorSchema ] }

        let api =
            HttpApi.make "Api"
            |> HttpApi.add (HttpApiGroup.make "test" |> HttpApiGroup.add endpoint)

        let spec = OpenApi.fromApi api

        let content =
            spec
            |> getObject "paths"
            |> getObject "/chat"
            |> getObject "get"
            |> getObject "responses"
            |> getObject "200"
            |> getObject "content"
            |> asObject

        toBe (content |> Map.containsKey "application/json") true
        toBe (content |> Map.containsKey "text/event-stream") true

        let streamExtension =
            content
            |> Map.find "text/event-stream"
            |> getObject "x-effect-stream"
            |> asObject

        toBe ((streamExtension |> Map.find "encoding") = JString "sse") true
        toBe ((streamExtension |> Map.find "failureEvent") = JString HttpApiSchema.streamFailureEvent) true
        toBe (streamExtension |> Map.containsKey "causeSchema") true
        toBe (streamExtension |> Map.containsKey "errorSchema") true))
