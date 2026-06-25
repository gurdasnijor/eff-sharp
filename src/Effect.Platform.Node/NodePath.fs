namespace Effect.Platform.Node

open Effect

/// Node path service facade. The core `Path` implementation currently uses the
/// POSIX path behavior that Effect exposes by default.
[<RequireQualifiedAccess>]
module NodePath =

    let layer<'E, 'RIn> : Layer<'E, 'RIn> = Path.layer

    let layerPosix<'E, 'RIn> : Layer<'E, 'RIn> = Path.layer

    let layerWin32<'E, 'RIn> : Layer<'E, 'RIn> = Path.layer

    let liveContext: Context = Context.make Path.tag Path.posix
