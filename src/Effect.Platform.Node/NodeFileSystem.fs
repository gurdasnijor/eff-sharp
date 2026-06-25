namespace Effect.Platform.Node

open System
open Effect

/// `NodeFileSystem` — the dual-backed implementation of the core `FileSystem`
/// service. Mirror of effect-smol's `platform-node-shared/NodeFileSystem.ts`.
///
///   * `.NET` (`#if !FABLE_COMPILER`) — `System.IO` (`File`/`Directory`/`Path`).
///     The non-Fable BCL fallback.
///   * Node (`#if FABLE_COMPILER`) — Node's `node:fs`, bound through `Fable.Core`'s
///     `[<Import>]`/`[<Emit>]` (this project takes a real `Fable.Core` package
///     reference, being a JS-interop package by nature — no local shim needed).
///
/// Host exceptions are normalized to a `PlatformError.SystemError` with a
/// `SystemErrorTag` derived from the .NET exception type / Node `error.code`.
///
/// **Synchronous backing on BOTH platforms — a deliberate choice.** Upstream's
/// `NodeFileSystem` prefers `node:fs/promises`, but eff-sharp's Effect core does
/// not wire an `Async`↔JS-`Promise` bridge into the Fable target, so both layers
/// wrap synchronous host calls in `Effect`s. These whole-file operations are
/// semantically identical sync or async; raw streaming / file handles (where
/// async would matter) are deferred (see footer).
[<RequireQualifiedAccess>]
module NodeFileSystem =

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
    /// channel. The shared bridge both platform impls build on. Uses only the
    /// public `Effect` API (the `Effect(...)` constructor is internal to the core
    /// assembly — platform packages compose via combinators, as they should).
    let private attempt (method: string) (path: string) (thunk: unit -> 'A) : Effect<'A, PlatformError, Context> =
        Effect.suspend (fun () ->
            try
                Effect.succeed (thunk ())
            with ex ->
                Effect.fail (toError method path ex))

    // ------------------------------------------------------------------------
    // Platform backends. Both wrap synchronous host calls (see module header).
    // ------------------------------------------------------------------------

#if FABLE_COMPILER
    // Named ESM imports. Parameter lists give Fable the arity, so calls emit as
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

    // Qualify System.DateTime — `open Effect` brings the port's own `DateTime`.
    let private epoch = System.DateTime(1970, 1, 1, 0, 0, 0, System.DateTimeKind.Utc)
    let private msToDate (ms: float) : System.DateTime = epoch.AddMilliseconds ms

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

    // --- service wiring -----------------------------------------------------

    /// The platform `FileSystem`: `System.IO`-backed on .NET, `node:fs` under Fable.
    let platform: FileSystem = impl

    /// A `Context` carrying the platform file system, keyed under `FileSystem.tag`.
    let liveContext: Context = Context.make FileSystem.tag platform

    /// The default platform `FileSystem` layer. (`NodeFileSystem.layer`)
    let layer<'E, 'RIn> : Layer<'E, 'RIn> = Layer.succeed FileSystem.tag platform

// ----------------------------------------------------------------------------
// Deferred (not needed by `verification`): raw-bytes `readFile`/`writeFile`
// (`Uint8Array`), non-UTF-8 encodings, `makeTempFile`/`makeTempFileScoped`,
// `open`/`File` handles, `stream`/`sink`, `watch`, `copy`/`copyFile`, `rename`,
// `link`/`symlink`/`readLink`, `access`, `realPath`, `truncate`, `utimes`,
// `chmod`/`chown`, and the full `File.Info` field set.
// ----------------------------------------------------------------------------
