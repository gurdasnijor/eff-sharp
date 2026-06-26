namespace Effect.Platform.Node

open Effect

/// Aggregate Node.js platform services layer.
///
/// This is the eff-sharp equivalent of `@effect/platform-node/NodeServices`:
/// provide it at the program edge when code needs the standard Node-backed
/// platform services without wiring each layer manually.
type NodeServices = Context

[<RequireQualifiedAccess>]
module NodeServices =

    /// Provides the common Node platform service set currently available in
    /// eff-sharp: child process spawning, filesystem, path, crypto, terminal,
    /// clock, and console.
    let layer<'E, 'RIn> : Layer<'E, 'RIn> =
        Layer.mergeAll
            [ NodeChildProcessSpawner.layer
              NodeFileSystem.layer
              NodePath.layer
              NodeCrypto.layer
              Terminal.layer
              Layer.succeed Clock.tag (Clock.make ())
              Layer.succeed Console.tag Console.live ]
