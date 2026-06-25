namespace Effect

[<RequireQualifiedAccess>]
module internal HttpApiDocs =

    let escapeHtml (value: string) =
        value
            .Replace("&", "&amp;")
            .Replace("<", "&lt;")
            .Replace(">", "&gt;")
            .Replace("\"", "&quot;")
            .Replace("'", "&#39;")

    let rec stringify =
        function
        | JNull -> "null"
        | JBool true -> "true"
        | JBool false -> "false"
        | JNumber n ->
            if System.Double.IsNaN n || System.Double.IsInfinity n then
                "null"
            else
                n.ToString(System.Globalization.CultureInfo.InvariantCulture)
        | JString s -> "\"" + escapeJsonString s + "\""
        | JArray values -> values |> List.map stringify |> String.concat "," |> fun body -> "[" + body + "]"
        | JObject fields ->
            fields
            |> Map.toList
            |> List.map (fun (key, value) -> stringify (JString key) + ":" + stringify value)
            |> String.concat ","
            |> fun body -> "{" + body + "}"

    and escapeJsonString (value: string) =
        let builder = System.Text.StringBuilder()

        for ch in value do
            match ch with
            | '"' -> builder.Append("\\\"") |> ignore
            | '\\' -> builder.Append("\\\\") |> ignore
            | '\b' -> builder.Append("\\b") |> ignore
            | '\f' -> builder.Append("\\f") |> ignore
            | '\n' -> builder.Append("\\n") |> ignore
            | '\r' -> builder.Append("\\r") |> ignore
            | '\t' -> builder.Append("\\t") |> ignore
            | c when int c < 0x20 -> builder.Append("\\u" + (int c).ToString("x4")) |> ignore
            | c -> builder.Append(c) |> ignore

        builder.ToString()

    let route (path: string) (html: string) (router: HttpRouter) =
        HttpRouter.add "GET" path (HttpServerResponse.html html) router
