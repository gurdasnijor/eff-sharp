namespace Effect.Platform.Node

open Effect

/// `Effect.Platform.Node` package entry point.
///
/// This module is the foundation marker for the Node platform package — the F#
/// mirror of effect-smol's `platform-node`. It deliberately holds only the
/// minimal surface that proves the package pipeline end to end:
///   * the project references the core `Effect` package and uses its types,
///   * it compiles on .NET (the xUnit test surface) AND Fable-compiles to JS (the
///     runtime target), and
///   * the dual-layer `#if FABLE_COMPILER` pattern — the spine of every real
///     service here (`NodeChildProcessSpawner`, `NodeStream`, `NodeFileSystem`,
///     …) — works inside this project.
///
/// The substantive services land in follow-up PRs: first the reusable
/// `PlatformStream.fromReadable` / `PlatformSink.fromWritable` adapters (the port's
/// `Readable → Stream` / `Writable → Sink` bridges, which every Node platform
/// module is built on), then `NodeChildProcessSpawner` rebuilt on them.
[<RequireQualifiedAccess>]
module NodePlatform =

    /// The runtime this package's services bind to: `"node"` under Fable (the JS
    /// target), `".net"` otherwise (the BCL-backed xUnit surface). A real, tiny
    /// value whose only job is to exercise the dual-layer split inside this project.
    let runtime<'E, 'R> : Effect<string, 'E, 'R> =
        Effect.sync (fun () ->
#if FABLE_COMPILER
            "node"
#else
            ".net"
#endif
        )
