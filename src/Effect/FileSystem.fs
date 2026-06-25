namespace Effect

/// `FileSystem` — the platform file-system service (abstract).
///
/// Port of repos/effect-smol/packages/effect/src/FileSystem.ts — the
/// platform-agnostic service: the `FileSystem` interface, its data types, the
/// `Tag`, and the accessor functions. The concrete platform implementations live
/// in the platform package, exactly as upstream splits `FileSystem.ts` (core)
/// from `platform-node-shared/NodeFileSystem.ts`:
///   * `Effect.Platform.Node`'s `NodeFileSystem` — dual-backed, `System.IO` on
///     .NET (the xUnit surface) and Node's `node:fs` under Fable, provided via
///     `NodeFileSystem.layer` / `NodeFileSystem.liveContext`.
///
/// All operations return an `Effect` whose error channel is the existing
/// `PlatformError`. Unlike the earlier draft, this core module carries **no**
/// baked-in default implementation: `fileSystemWith` requires the service to be
/// provided in `Context` (supply `NodeFileSystem.layer`), matching upstream where
/// the platform layer is a required dependency. This is what keeps core free of
/// any platform binding.
///
/// SCOPE (the minimum to cut over fluent-firegrid's `verification` package):
/// `readFileString`, `writeFileString`, `exists`, `makeDirectory`,
/// `readDirectory`, `remove`, `stat`, `makeTempDirectory`, and
/// `makeTempDirectoryScoped`. Deferred ops are listed in `NodeFileSystem`.

open System

/// The kind of a file-system entry. (`File.Type`) `[<RequireQualifiedAccess>]`
/// so the `File`/`Directory`/`Unknown` cases never collide with `System.IO`'s
/// `File`/`Directory` classes or `SystemErrorTag.Unknown`.
[<RequireQualifiedAccess>]
type FileType =
    | File
    | Directory
    | SymbolicLink
    | BlockDevice
    | CharacterDevice
    | FIFO
    | Socket
    | Unknown

/// Metadata about a file-system entry, returned by `stat`. (`File.Info`)
///
/// `Size` lowers upstream's branded `Size` (bigint) to `int64` — ample for real
/// file sizes and free of a BigInt dependency. Fields the portable host APIs do
/// not surface (`Dev`/`Ino`/`Nlink`/`Uid`/`Gid`/`Rdev`/`Blksize`/`Blocks`) are
/// populated best-effort (`None`/`0`); `Type`/`Size`/`Mode`/times are real.
type FileInfo =
    { Type: FileType
      Mtime: DateTime option
      Atime: DateTime option
      Birthtime: DateTime option
      Dev: int
      Ino: int option
      Mode: int
      Nlink: int option
      Uid: int option
      Gid: int option
      Rdev: int option
      Size: int64
      Blksize: int64 option
      Blocks: int option }

/// Options for `makeDirectory`. (`recursive` is honored on both platforms;
/// `Mode` is accepted for parity but applied best-effort only.)
type MakeDirectoryOptions = { Recursive: bool; Mode: int option }

/// Options for `readDirectory`.
type ReadDirectoryOptions = { Recursive: bool }

/// Options for `remove`. `Force` ignores a missing target; `Recursive` removes a
/// non-empty directory.
type RemoveOptions = { Recursive: bool; Force: bool }

/// Options for `makeTempDirectory` / `makeTempDirectoryScoped`.
type MakeTempOptions =
    { Directory: string option
      Prefix: string option }

/// The platform file-system service. Each field returns an `Effect` whose error
/// channel is `PlatformError`. (`FileSystem.FileSystem`)
type FileSystem =
    { ReadFileString: string -> Effect<string, PlatformError, Context>
      WriteFileString: string -> string -> Effect<unit, PlatformError, Context>
      Exists: string -> Effect<bool, PlatformError, Context>
      MakeDirectory: string -> MakeDirectoryOptions -> Effect<unit, PlatformError, Context>
      ReadDirectory: string -> ReadDirectoryOptions -> Effect<string list, PlatformError, Context>
      Remove: string -> RemoveOptions -> Effect<unit, PlatformError, Context>
      Stat: string -> Effect<FileInfo, PlatformError, Context>
      MakeTempDirectory: MakeTempOptions -> Effect<string, PlatformError, Context> }

[<RequireQualifiedAccess>]
module FileSystem =

    /// Runtime marker stored on `FileSystem` implementations upstream (kept for
    /// parity; the F# type is the guard).
    [<Literal>]
    let TypeId = "~effect/platform/FileSystem"

    /// The `Tag` under which the `FileSystem` service is stored. Implementations
    /// (`NodeFileSystem`) register under this tag; accessors read it back.
    let tag: Tag<FileSystem> = Tag.make<FileSystem> "effect/platform/FileSystem"

    /// Run an effect against the `FileSystem` provided in `Context`. The service is
    /// a required dependency — supply `NodeFileSystem.layer` (or `liveContext`); a
    /// missing service is a defect, not a silent fallback to a baked-in platform.
    let fileSystemWith (f: FileSystem -> Effect<'A, 'E, Context>) : Effect<'A, 'E, Context> =
        Effect.environment<Context, 'E>
        |> Effect.flatMap (fun ctx ->
            match Context.tryGet tag ctx with
            | Some fs -> f fs
            | None ->
                Effect(fun _ _ ->
                    async {
                        return
                            Exit.die (
                                box
                                    "FileSystem service not provided — supply NodeFileSystem.layer / NodeFileSystem.liveContext"
                            )
                    }))

    // --- ergonomic option defaults ------------------------------------------

    /// `makeDirectory` options: single directory, no parents.
    let makeDirectoryOptions: MakeDirectoryOptions = { Recursive = false; Mode = None }

    /// `makeDirectory` options: create parents as needed (`mkdir -p`).
    let makeDirectoryRecursive: MakeDirectoryOptions = { Recursive = true; Mode = None }

    /// `readDirectory` options: list immediate children only.
    let readDirectoryOptions: ReadDirectoryOptions = { Recursive = false }

    /// `remove` options: a single file / empty directory, error if absent.
    let removeOptions: RemoveOptions = { Recursive = false; Force = false }

    /// `remove` options: recursive + force (`rm -rf`).
    let removeRecursive: RemoveOptions = { Recursive = true; Force = true }

    /// `makeTempDirectory` options: system temp dir, no prefix.
    let tempOptions: MakeTempOptions = { Directory = None; Prefix = None }

    // --- accessors (mirror the upstream method names) -----------------------

    /// Read a whole file as a UTF-8 string. (`fs.readFileString`)
    let readFileString (path: string) : Effect<string, PlatformError, Context> =
        fileSystemWith (fun fs -> fs.ReadFileString path)

    /// Write a string to a file, replacing existing contents. (`fs.writeFileString`)
    let writeFileString (path: string) (data: string) : Effect<unit, PlatformError, Context> =
        fileSystemWith (fun fs -> fs.WriteFileString path data)

    /// Whether a file or directory exists at `path`. (`fs.exists`)
    let exists (path: string) : Effect<bool, PlatformError, Context> =
        fileSystemWith (fun fs -> fs.Exists path)

    /// Create a directory. (`fs.makeDirectory`)
    let makeDirectory (path: string) (options: MakeDirectoryOptions) : Effect<unit, PlatformError, Context> =
        fileSystemWith (fun fs -> fs.MakeDirectory path options)

    /// List a directory's contents (basenames; relative paths when recursive).
    /// (`fs.readDirectory`)
    let readDirectory (path: string) (options: ReadDirectoryOptions) : Effect<string list, PlatformError, Context> =
        fileSystemWith (fun fs -> fs.ReadDirectory path options)

    /// Remove a file or directory. (`fs.remove`)
    let remove (path: string) (options: RemoveOptions) : Effect<unit, PlatformError, Context> =
        fileSystemWith (fun fs -> fs.Remove path options)

    /// Stat a file-system entry. (`fs.stat`)
    let stat (path: string) : Effect<FileInfo, PlatformError, Context> = fileSystemWith (fun fs -> fs.Stat path)

    /// Create a temporary directory and return its path. (`fs.makeTempDirectory`)
    let makeTempDirectory (options: MakeTempOptions) : Effect<string, PlatformError, Context> =
        fileSystemWith (fun fs -> fs.MakeTempDirectory options)

    /// Create a temporary directory whose removal (`rm -rf`) is registered on the
    /// given `scope`, so it is deleted when the scope closes. Built on the service
    /// primitives, so it works for any `FileSystem` impl. (`fs.makeTempDirectoryScoped`)
    ///
    /// The scope is `PlatformError`-typed because the registered finalizer is the
    /// `remove` effect (whose error channel is `PlatformError`).
    let makeTempDirectoryScoped
        (scope: Scope<PlatformError, Context>)
        (options: MakeTempOptions)
        : Effect<string, PlatformError, Context> =
        fileSystemWith (fun fs ->
            Scope.acquireRelease scope (fs.MakeTempDirectory options) (fun dir _ -> fs.Remove dir removeRecursive))
