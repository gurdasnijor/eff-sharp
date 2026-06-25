module HeadersSpec

open Effect
open Effect.Vitest

describe "Headers" (fun () ->
    test "normalizes keys and overwrites case-insensitively" (fun () ->
        let headers =
            Headers.empty
            |> Headers.set "Content-Type" "text/plain"
            |> Headers.set "content-type" "application/json"

        toBe (Headers.get "CONTENT-TYPE" headers) (Some "application/json")
        toBe (Headers.has "Content-Type" headers) true
        toEqual (Headers.toList headers) [ "content-type", "application/json" ])

    test "merges with right-hand values winning" (fun () ->
        let left = Headers.ofList [ "accept", "text/plain"; "x-left", "1" ]
        let right = Headers.ofList [ "Accept", "application/json" ]
        let merged = Headers.merge left right

        toBe (Headers.get "accept" merged) (Some "application/json")
        toBe (Headers.get "x-left" merged) (Some "1" )))
