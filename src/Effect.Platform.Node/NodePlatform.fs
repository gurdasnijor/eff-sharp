namespace Effect.Platform.Node

open Effect

/// `Effect.Platform.Node` package entry point.
///
/// This module is the foundation marker for the Node platform package — the F#
/// mirror of effect-smol's `platform-node`. The runtime target is always Node
/// through Fable-generated JavaScript.
[<RequireQualifiedAccess>]
module NodePlatform =

    /// The runtime this package's services bind to.
    let runtime<'E, 'R> : Effect<string, 'E, 'R> =
        Effect.sync (fun () -> "node")
