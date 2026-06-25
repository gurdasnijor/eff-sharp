namespace Effect.Platform.Node

open Fable.Core

/// Facade for the `undici` package used by Node HTTP clients upstream.
[<RequireQualifiedAccess>]
module Undici =

    [<Emit("import('undici')")>]
    let importPackage () : JS.Promise<obj> = jsNative

    let package () : JS.Promise<obj> = importPackage ()
