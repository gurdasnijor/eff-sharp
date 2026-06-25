namespace Effect.Platform.Node

open Fable.Core

/// Small MIME lookup facade matching the platform-node `Mime` module shape.
[<RequireQualifiedAccess>]
module Mime =

    [<ImportDefault("mime")>]
    let private mime: obj = jsNative

    [<Emit("$0.getType($1)")>]
    let private getTypeJs (_mime: obj) (_path: string) : string = jsNative

    /// Resolve a content type from a file path, if the extension is known.
    let getType (path: string) : string option =
        match getTypeJs mime path with
        | null -> None
        | value -> Some value

    /// Resolve a content type, defaulting to `application/octet-stream`.
    let getTypeOrDefault (path: string) : string =
        getType path |> Option.defaultValue "application/octet-stream"
