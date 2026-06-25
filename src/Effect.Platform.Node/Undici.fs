namespace Effect.Platform.Node

open Fable.Core

/// Facade for the `undici` package used by Node HTTP clients upstream.
[<RequireQualifiedAccess>]
module Undici =

    [<ImportAll("undici")>]
    let exports: obj = jsNative

    let package: obj = exports
