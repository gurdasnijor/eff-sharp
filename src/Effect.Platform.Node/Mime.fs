namespace Effect.Platform.Node

/// Small MIME lookup facade matching the platform-node `Mime` module shape.
[<RequireQualifiedAccess>]
module Mime =

    let private types =
        Map.ofList
            [ ".css", "text/css"
              ".csv", "text/csv"
              ".gif", "image/gif"
              ".htm", "text/html"
              ".html", "text/html"
              ".ico", "image/x-icon"
              ".jpeg", "image/jpeg"
              ".jpg", "image/jpeg"
              ".js", "text/javascript"
              ".json", "application/json"
              ".mjs", "text/javascript"
              ".pdf", "application/pdf"
              ".png", "image/png"
              ".svg", "image/svg+xml"
              ".txt", "text/plain"
              ".wasm", "application/wasm"
              ".webp", "image/webp"
              ".xml", "application/xml" ]

    let private extension (path: string) =
        let normalized = path.Split('?').[0].Split('#').[0]
        let index = normalized.LastIndexOf('.')

        if index < 0 then
            None
        else
            Some(normalized.Substring(index).ToLowerInvariant())

    /// Resolve a content type from a file path, if the extension is known.
    let getType (path: string) : string option =
        extension path |> Option.bind (fun ext -> Map.tryFind ext types)

    /// Resolve a content type, defaulting to `application/octet-stream`.
    let getTypeOrDefault (path: string) : string =
        getType path |> Option.defaultValue "application/octet-stream"
