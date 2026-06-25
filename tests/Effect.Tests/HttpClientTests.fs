module Effect.Tests.HttpClientTests

open Xunit
open Effect

// Service-wiring tests for the HttpClient accessor (no real network). The Node
// (fetch) and .NET (System.Net.Http) implementations build + Fable-compile as part
// of Effect.Platform.Node; their real I/O is exercised by the cutover drivers.

[<Fact>]
let ``get routes method + url through the provided HttpClient service`` () =
    let mock: HttpClient =
        { Request = fun m u -> Effect.succeed { Status = 200; Body = m + " " + u } }

    match Effect.runSync (Context.make HttpClient.tag mock) (HttpClient.get "http://x/health") with
    | Success r ->
        Assert.Equal(200, r.Status)
        Assert.Equal("GET http://x/health", r.Body)
    | Failure c -> failwithf "request failed: %s" (Cause.render c)

[<Fact>]
let ``a transport failure surfaces as a typed HttpClientError`` () =
    let mock: HttpClient =
        { Request =
            fun m u ->
                Effect.fail
                    { Method = m
                      Url = u
                      Reason = "connection refused" } }

    match Effect.runSync (Context.make HttpClient.tag mock) (HttpClient.get "http://x") with
    | Success _ -> failwith "expected a transport failure"
    | Failure c -> Assert.Contains("connection refused", Cause.render c)
