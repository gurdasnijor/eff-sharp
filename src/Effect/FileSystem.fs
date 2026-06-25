namespace Effect

/// `FileSystem` — the platform file-system service.
///
/// Port of repos/effect-smol/packages/effect/src/FileSystem.ts (the abstract
/// service) and `@effect/platform-node`'s `NodeFileSystem` (the `fs`-backed
/// implementation). Like `Path`/`Terminal`/`Console`, it is a `Context` service
/// with a default platform `layer`, but here the layer is **dual-backed**:
///   * `.NET` (`#if !FABLE_COMPILER`) — `System.IO` (`File`/`Directory`/`Path`).
///     This is the xUnit test surface.
///   * Node (`#if FABLE_COMPILER`) — Node's `node:fs`, bound through the local
///     `Fable.Core` `[<Import>]`/`[<Emit>]` shims declared in `Terminal.fs`
///     (compiled earlier in the assembly), so the `.NET` build takes no
///     `Fable.Core` dependency.
///
/// All operations return an `Effect` whose error channel is the existing
/// `PlatformError`; host exceptions are normalized to a `SystemError` with a
/// `SystemErrorTag` derived from the .NET exception type / Node `error.code`.
///
/// **Synchronous backing on BOTH platforms — a deliberate choice.** Upstream's
/// `NodeFileSystem` prefers `node:fs/promises`, but eff-sharp's Effect core does
/// not wire an `Async`↔JS-`Promise` bridge into the Fable target (see the Fable
/// shim on `Effect.promise`: it accepts an `Async` thunk and leaves the
/// `Async.AwaitPromise` bridging to the JS host, and this library takes no
/// `Fable.Core` package dependency). `Terminal.fs` set the precedent of binding
/// the *synchronous* `node:fs` primitive (`readSync`). These whole-file
/// operations are semantically identical synchronous or async, so both layers
/// wrap synchronous host calls in `Effect.sync`-style effects. This keeps the
/// Node layer runnable under both `runSync` and the async runner with zero
/// Promise plumbing. Raw streaming / file handles (where async would matter) are
/// deferred (see module footer).
///
/// SCOPE (the minimum to cut over fluent-firegrid's `verification` package, which
/// calls `makeDirectory`/`writeFileString`/`makeTempDirectoryScoped`): the
/// operation set is `readFileString`, `writeFileString`, `exists`,
/// `makeDirectory`, `readDirectory`, `remove`, `stat`, `makeTempDirectory`, and
/// `makeTempDirectoryScoped`. Deferred ops are listed at the module footer.

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

    // --- error normalization (PlatformError.SystemError) --------------------

    let private systemErrorOf (tag: SystemErrorTag) (method: string) (path: string) (ex: exn) : PlatformError =
        PlatformError.systemError
            { Tag = tag
              Module = "FileSystem"
              Method = method
              Description = Some ex.Message
              Syscall = None
              PathOrDescriptor = Some(box path)
              Cause = Some(box ex) }

#if FABLE_COMPILER
    // Read Node's `error.code` (e.g. "ENOENT") off the caught JS error.
    [<Fable.Core.Emit("($0 && $0.code) ? $0.code : \"\"")>]
    let private errorCode (e: obj) : string = failwith "Fable emit only"

    let private classify (ex: exn) : SystemErrorTag =
        match errorCode (box ex) with
        | "ENOENT" -> NotFound
        | "EEXIST" -> AlreadyExists
        | "EACCES"
        | "EPERM" -> PermissionDenied
        | "EBUSY"
        | "ETXTBSY" -> Busy
        | "EAGAIN" -> WouldBlock
        | _ -> Unknown
#else
    let private classify (ex: exn) : SystemErrorTag =
        match ex with
        | :? System.IO.FileNotFoundException
        | :? System.IO.DirectoryNotFoundException -> NotFound
        | :? System.UnauthorizedAccessException -> PermissionDenied
        | :? System.IO.IOException as io when io.Message.Contains("already exists") -> AlreadyExists
        | _ -> Unknown
#endif

    let private toError (method: string) (path: string) (ex: exn) : PlatformError =
        systemErrorOf (classify ex) method path ex

    /// Run a synchronous host thunk, lifting a throw into the `PlatformError`
    /// channel. The shared bridge both platform impls build on.
    let private attempt (method: string) (path: string) (thunk: unit -> 'A) : Effect<'A, PlatformError, Context> =
        Effect(fun _ _ ->
            async {
                try
                    return Success(thunk ())
                with ex ->
                    return Exit.fail (toError method path ex)
            })

    // ------------------------------------------------------------------------
    // Platform backends. Both wrap synchronous host calls (see module header).
    // ------------------------------------------------------------------------

#if FABLE_COMPILER
    // Named ESM imports via the local `Fable.Core` shim attributes (declared in
    // Terminal.fs). Parameter lists give Fable the arity, so calls emit as
    // `fn(a, b)` rather than curried application.
    [<Fable.Core.Import("readFileSync", "node:fs")>]
    let private readFileSyncJs (path: string) (encoding: string) : string = failwith "Fable import only"

    [<Fable.Core.Import("writeFileSync", "node:fs")>]
    let private writeFileSyncJs (path: string) (data: string) : unit = failwith "Fable import only"

    [<Fable.Core.Import("existsSync", "node:fs")>]
    let private existsSyncJs (path: string) : bool = failwith "Fable import only"

    [<Fable.Core.Import("mkdirSync", "node:fs")>]
    let private mkdirSyncJs (path: string) (options: obj) : unit = failwith "Fable import only"

    [<Fable.Core.Import("readdirSync", "node:fs")>]
    let private readdirSyncJs (path: string) (options: obj) : string[] = failwith "Fable import only"

    [<Fable.Core.Import("rmSync", "node:fs")>]
    let private rmSyncJs (path: string) (options: obj) : unit = failwith "Fable import only"

    [<Fable.Core.Import("statSync", "node:fs")>]
    let private statSyncJs (path: string) : obj = failwith "Fable import only"

    [<Fable.Core.Import("mkdtempSync", "node:fs")>]
    let private mkdtempSyncJs (prefix: string) : string = failwith "Fable import only"

    [<Fable.Core.Import("tmpdir", "node:os")>]
    let private tmpdirJs () : string = failwith "Fable import only"

    // Small JS-literal builders for the `fs` options objects.
    [<Fable.Core.Emit("{ recursive: $0 }")>]
    let private recursiveOption (recursive: bool) : obj = failwith "Fable emit only"

    [<Fable.Core.Emit("{ recursive: $0, force: $1 }")>]
    let private removeOption (recursive: bool) (force: bool) : obj = failwith "Fable emit only"

    // Field accessors on a Node `Stats` object.
    [<Fable.Core.Emit("$0.isDirectory()")>]
    let private statIsDirectory (s: obj) : bool = failwith "Fable emit only"

    [<Fable.Core.Emit("$0.isSymbolicLink()")>]
    let private statIsSymlink (s: obj) : bool = failwith "Fable emit only"

    [<Fable.Core.Emit("Number($0.size)")>]
    let private statSize (s: obj) : float = failwith "Fable emit only"

    [<Fable.Core.Emit("$0.mode")>]
    let private statMode (s: obj) : int = failwith "Fable emit only"

    [<Fable.Core.Emit("$0.mtimeMs")>]
    let private statMtimeMs (s: obj) : float = failwith "Fable emit only"

    [<Fable.Core.Emit("$0.atimeMs")>]
    let private statAtimeMs (s: obj) : float = failwith "Fable emit only"

    [<Fable.Core.Emit("$0.birthtimeMs")>]
    let private statBirthtimeMs (s: obj) : float = failwith "Fable emit only"

    let private epoch = DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)
    let private msToDate (ms: float) : DateTime = epoch.AddMilliseconds ms

    let private statNode (path: string) : FileInfo =
        let s = statSyncJs path

        let typ =
            if statIsSymlink s then FileType.SymbolicLink
            elif statIsDirectory s then FileType.Directory
            else FileType.File

        { Type = typ
          Mtime = Some(msToDate (statMtimeMs s))
          Atime = Some(msToDate (statAtimeMs s))
          Birthtime = Some(msToDate (statBirthtimeMs s))
          Dev = 0
          Ino = None
          Mode = statMode s
          Nlink = None
          Uid = None
          Gid = None
          Rdev = None
          Size = int64 (statSize s)
          Blksize = None
          Blocks = None }

    let private makeTempDirNode (options: MakeTempOptions) : string =
        let baseDir = defaultArg options.Directory (tmpdirJs ())
        let prefix = defaultArg options.Prefix ""

        let trimmed =
            if baseDir.EndsWith("/") then
                baseDir.Substring(0, baseDir.Length - 1)
            else
                baseDir

        mkdtempSyncJs (trimmed + "/" + prefix)

    let private impl: FileSystem =
        { ReadFileString = fun path -> attempt "readFileString" path (fun () -> readFileSyncJs path "utf8")
          WriteFileString = fun path data -> attempt "writeFileString" path (fun () -> writeFileSyncJs path data)
          Exists = fun path -> attempt "exists" path (fun () -> existsSyncJs path)
          MakeDirectory =
            fun path options ->
                attempt "makeDirectory" path (fun () -> mkdirSyncJs path (recursiveOption options.Recursive))
          ReadDirectory =
            fun path options ->
                attempt "readDirectory" path (fun () ->
                    List.ofArray (readdirSyncJs path (recursiveOption options.Recursive)))
          Remove =
            fun path options ->
                attempt "remove" path (fun () -> rmSyncJs path (removeOption options.Recursive options.Force))
          Stat = fun path -> attempt "stat" path (fun () -> statNode path)
          MakeTempDirectory =
            fun options ->
                attempt "makeTempDirectory" (defaultArg options.Prefix "") (fun () -> makeTempDirNode options) }
#else
    let private statNet (path: string) : FileInfo =
        // `GetAttributes` throws (FileNotFound/DirectoryNotFound) if `path` is absent.
        let attrs = System.IO.File.GetAttributes path
        let isDir = attrs.HasFlag System.IO.FileAttributes.Directory
        let isSymlink = attrs.HasFlag System.IO.FileAttributes.ReparsePoint

        let fsi: System.IO.FileSystemInfo =
            if isDir then
                System.IO.DirectoryInfo(path) :> System.IO.FileSystemInfo
            else
                System.IO.FileInfo(path) :> System.IO.FileSystemInfo

        let typ =
            if isSymlink then FileType.SymbolicLink
            elif isDir then FileType.Directory
            else FileType.File

        let size =
            match fsi with
            | :? System.IO.FileInfo as f -> f.Length
            | _ -> 0L

        // POSIX mode is only available on Unix (.NET throws on Windows / pre-create).
        let mode =
            try
                int (System.IO.File.GetUnixFileMode path)
            with _ ->
                0

        { Type = typ
          Mtime = Some fsi.LastWriteTimeUtc
          Atime = Some fsi.LastAccessTimeUtc
          Birthtime = Some fsi.CreationTimeUtc
          Dev = 0
          Ino = None
          Mode = mode
          Nlink = None
          Uid = None
          Gid = None
          Rdev = None
          Size = size
          Blksize = None
          Blocks = None }

    let private makeDirNet (path: string) (options: MakeDirectoryOptions) : unit =
        if options.Recursive then
            // `CreateDirectory` is idempotent and creates intermediates.
            System.IO.Directory.CreateDirectory path |> ignore
        else
            // Mirror Node's non-recursive mkdir: EEXIST if present, ENOENT if the
            // parent is missing (.NET would otherwise create intermediates).
            if System.IO.Directory.Exists path || System.IO.File.Exists path then
                raise (System.IO.IOException(sprintf "EEXIST: file already exists, mkdir '%s'" path))

            let parent = System.IO.Directory.GetParent path

            if not (isNull parent) && not parent.Exists then
                raise (
                    System.IO.DirectoryNotFoundException(sprintf "ENOENT: no such file or directory, mkdir '%s'" path)
                )

            System.IO.Directory.CreateDirectory path |> ignore

    let private readDirNet (path: string) (options: ReadDirectoryOptions) : string list =
        if options.Recursive then
            System.IO.Directory.EnumerateFileSystemEntries(path, "*", System.IO.SearchOption.AllDirectories)
            |> Seq.map (fun full -> System.IO.Path.GetRelativePath(path, full))
            |> List.ofSeq
        else
            System.IO.Directory.EnumerateFileSystemEntries path
            |> Seq.map System.IO.Path.GetFileName
            |> List.ofSeq

    let private removeNet (path: string) (options: RemoveOptions) : unit =
        if System.IO.File.Exists path then
            System.IO.File.Delete path
        elif System.IO.Directory.Exists path then
            System.IO.Directory.Delete(path, options.Recursive)
        elif options.Force then
            ()
        else
            raise (System.IO.FileNotFoundException(sprintf "ENOENT: no such file or directory, remove '%s'" path, path))

    let private makeTempDirNet (options: MakeTempOptions) : string =
        let baseDir = defaultArg options.Directory (System.IO.Path.GetTempPath())
        let prefix = defaultArg options.Prefix ""
        let name = prefix + Guid.NewGuid().ToString("N").Substring(0, 12)
        let dir = System.IO.Path.Combine(baseDir, name)
        System.IO.Directory.CreateDirectory dir |> ignore
        dir

    let private impl: FileSystem =
        { ReadFileString = fun path -> attempt "readFileString" path (fun () -> System.IO.File.ReadAllText path)
          WriteFileString =
            fun path data -> attempt "writeFileString" path (fun () -> System.IO.File.WriteAllText(path, data))
          Exists =
            fun path -> attempt "exists" path (fun () -> System.IO.File.Exists path || System.IO.Directory.Exists path)
          MakeDirectory = fun path options -> attempt "makeDirectory" path (fun () -> makeDirNet path options)
          ReadDirectory = fun path options -> attempt "readDirectory" path (fun () -> readDirNet path options)
          Remove = fun path options -> attempt "remove" path (fun () -> removeNet path options)
          Stat = fun path -> attempt "stat" path (fun () -> statNet path)
          MakeTempDirectory =
            fun options -> attempt "makeTempDirectory" (defaultArg options.Prefix "") (fun () -> makeTempDirNet options) }
#endif

    // --- service wiring (shared) --------------------------------------------

    /// The `Tag` under which the `FileSystem` service is stored.
    let tag: Tag<FileSystem> = Tag.make<FileSystem> "effect/platform/FileSystem"

    /// The default platform `FileSystem`: `System.IO`-backed on .NET, `node:fs`
    /// under Fable. (`NodeFileSystem` / the .NET analogue.)
    let platform: FileSystem = impl

    /// A `Context` carrying the platform file system.
    let liveContext: Context = Context.make tag platform

    /// The default platform `FileSystem` layer. (`NodeFileSystem.layer`)
    let layer<'E, 'RIn> : Layer<'E, 'RIn> = Layer.succeed tag platform

    /// Run an effect against the active `FileSystem`, falling back to `platform`
    /// when none is provided (mirrors `Terminal.terminalWith`).
    let fileSystemWith (f: FileSystem -> Effect<'A, 'E, Context>) : Effect<'A, 'E, Context> =
        Effect.environment<Context, 'E>
        |> Effect.flatMap (fun ctx ->
            match Context.tryGet tag ctx with
            | Some fs -> f fs
            | None -> f platform)

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

// ----------------------------------------------------------------------------
// Deferred (not needed by `verification`; the `PlatformError` plumbing here is
// ready for a follow-up): raw-bytes `readFile`/`writeFile` (`Uint8Array`),
// non-UTF-8 `readFileString` encodings, `makeTempFile`/`makeTempFileScoped`,
// `open`/`File` handles, `stream`/`sink`, `watch`, `copy`/`copyFile`,
// `rename`, `link`/`symlink`/`readLink`, `access`, `realPath`, `truncate`,
// `utimes`, `chmod`/`chown`, and the full `File.Info` field set (only
// `Type`/`Size`/`Mode`/timestamps are populated; the rest are `None`/`0`).
// ----------------------------------------------------------------------------
