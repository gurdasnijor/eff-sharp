namespace Effect

open System

/// Ordered URL query parameters. Duplicate keys are preserved.
type UrlParams = (string * string) list

[<RequireQualifiedAccess>]
module UrlParams =

    let empty: UrlParams = []

    let ofList (params_: (string * string) list) : UrlParams = params_

    let ofSeq (params_: seq<string * string>) : UrlParams = Seq.toList params_

    let singleton (key: string) (value: string) : UrlParams = [ key, value ]

    let append (key: string) (value: string) (params_: UrlParams) : UrlParams = params_ @ [ key, value ]

    let set (key: string) (value: string) (params_: UrlParams) : UrlParams =
        (params_ |> List.filter (fun (k, _) -> k <> key)) @ [ key, value ]

    let remove (key: string) (params_: UrlParams) : UrlParams =
        params_ |> List.filter (fun (k, _) -> k <> key)

    let getAll (key: string) (params_: UrlParams) : string list =
        params_ |> List.choose (fun (k, v) -> if k = key then Some v else None)

    let getFirst (key: string) (params_: UrlParams) : string option =
        params_ |> List.tryPick (fun (k, v) -> if k = key then Some v else None)

    let merge (left: UrlParams) (right: UrlParams) : UrlParams = left @ right

    let toList (params_: UrlParams) : (string * string) list = params_

    let private encode (value: string) = Uri.EscapeDataString(value).Replace("%20", "+")

    let private decode (value: string) = Uri.UnescapeDataString(value.Replace("+", " "))

    let parseQueryString (query: string) : UrlParams =
        let q = if query.StartsWith("?") then query.Substring(1) else query

        if q = "" then
            empty
        else
            q.Split('&')
            |> Array.toList
            |> List.filter (fun part -> part <> "")
            |> List.map (fun part ->
                let index = part.IndexOf('=')

                if index < 0 then
                    decode part, ""
                else
                    decode (part.Substring(0, index)), decode (part.Substring(index + 1)))

    let toQueryString (params_: UrlParams) : string =
        params_
        |> List.map (fun (key, value) -> encode key + "=" + encode value)
        |> String.concat "&"

    let appendToUrl (url: string) (params_: UrlParams) : string =
        let query = toQueryString params_

        if query = "" then
            url
        else
            let separator =
                if url.Contains("?") then
                    if url.EndsWith("?") || url.EndsWith("&") then "" else "&"
                else
                    "?"

            url + separator + query
