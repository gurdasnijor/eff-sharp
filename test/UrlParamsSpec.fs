module UrlParamsSpec

open Effect
open Effect.Vitest

describe "UrlParams" (fun () ->
    test "preserves duplicate keys and insertion order" (fun () ->
        let params_ =
            UrlParams.empty
            |> UrlParams.append "tag" "a"
            |> UrlParams.append "tag" "b"
            |> UrlParams.append "q" "hello world"

        toEqual (UrlParams.getAll "tag" params_) [ "a"; "b" ]
        toBe (UrlParams.getFirst "q" params_) (Some "hello world")
        toBe (UrlParams.toQueryString params_) "tag=a&tag=b&q=hello+world")

    test "appends to urls with and without existing query strings" (fun () ->
        let params_ = UrlParams.ofList [ "a", "1"; "b", "x/y" ]

        toBe (UrlParams.appendToUrl "https://example.com" params_) "https://example.com?a=1&b=x%2Fy"
        toBe (UrlParams.appendToUrl "https://example.com?c=3" params_) "https://example.com?c=3&a=1&b=x%2Fy"))
