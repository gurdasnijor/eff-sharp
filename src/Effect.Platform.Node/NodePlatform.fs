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

    /// The default Node platform layer.
    let layer<'E, 'RIn> : Layer<'E, 'RIn> = NodeRuntime.layer

    /// Fetch-backed Node HTTP client layer.
    let httpClient<'E, 'RIn> : Layer<'E, 'RIn> = NodeHttpClient.layer

    /// Node filesystem layer.
    let fileSystem<'E, 'RIn> : Layer<'E, 'RIn> = NodeFileSystem.layer

    /// Node path layer.
    let path<'E, 'RIn> : Layer<'E, 'RIn> = NodePath.layer

    /// Node crypto layer.
    let crypto<'E, 'RIn> : Layer<'E, 'RIn> = NodeCrypto.layer

    /// Node HTTP platform file-response layer.
    let httpPlatform<'E, 'RIn> : Layer<'E, 'RIn> = NodeHttpPlatform.layer

    /// Start a local Node HTTP server for a router on `127.0.0.1:<port>`.
    let listenLocal (port: int) (router: HttpRouter) =
        NodeHttpServer.listenLocal port router
