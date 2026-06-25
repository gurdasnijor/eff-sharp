module NodeHttpClientSpec

open Effect
open Effect.Platform.Node
open Effect.Vitest

describe "NodeHttpClient" (fun () ->
    itEffectIn NodeHttpClient.liveContext "executes structured requests through fetch" (fun () ->
        HttpClientRequest.get "data:text/plain,hello"
        |> HttpClient.execute
        |> Effect.map (fun response ->
            toBe response.Status 200
            toBe (HttpClientResponse.bodyText response) (Some "hello")))

    itEffectIn NodeHttpClient.liveContext "supports legacy method/url requests" (fun () ->
        HttpClient.get "data:text/plain,legacy"
        |> Effect.map (fun response ->
            toBe response.Status 200
            toBe response.Body "legacy")))
