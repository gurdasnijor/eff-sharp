namespace Effect.Platform.Node

open System
open Effect

/// `NodeFileSystem` — the Node implementation of the core `FileSystem` service.
/// Mirror of effect-smol's `platform-node-shared/NodeFileSystem.ts`, bound to
/// Node's `node:fs` through `Fable.Core` `[<Import>]`/`[<Emit>]`.
///
/// Host exceptions are normalized to a `PlatformError.SystemError` with a
/// `SystemErrorTag` derived from the Node `error.code`.
///
/// **Synchronous backing is deliberate.** Upstream's `NodeFileSystem` prefers
/// `node:fs/promises`, but these whole-file operations are semantically identical
/// sync or async; raw streaming / file handles are deferred (see footer).
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

    // --- service wiring -----------------------------------------------------

    /// The platform `FileSystem`, backed by `node:fs`.
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
