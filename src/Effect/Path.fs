namespace Effect

/// `Path` — the file-path service.
///
/// Port of repos/effect-smol/packages/effect/src/Path.ts. Exposes POSIX path
/// operations (`join`/`basename`/`dirname`/`extname`/`normalize`/`resolve`/
/// `relative`/`isAbsolute`/`parse`/`format`/`toNamespacedPath`) as a `Context`
/// service, with a default `layer` providing the built-in POSIX implementation.
/// The string algorithms are a faithful, char-code-for-char-code port of the
/// Node.js `path.posix` routines upstream adapts (resolving `.`/`..`, collapsing
/// separators, etc.).
///
/// Porting notes (see CONVENTIONS.md):
///   * The default `layer` is the **POSIX** implementation (separator `/`),
///     exactly as upstream's `Path.layer` — *not* `System.IO.Path`, whose output
///     is platform-dependent (backslashes on Windows) and would diverge from
///     upstream's POSIX semantics. `resolve`/`relative` fall back to
///     `System.IO.Directory.GetCurrentDirectory()` for the current directory
///     (the analogue of `process.cwd()`), used only for relative inputs.
///   * `fromFileUrl`/`toFileUrl` (URL ↔ path, with `decodeURIComponent`/
///     percent-encoding) are deferred — they are outside the brief's core
///     surface and need faithful WHATWG-URL handling; noted here, the `BadArgument`
///     plumbing via `PlatformError` is ready for a follow-up.
///   * The `TypeId` brand on the service record is dropped (F#'s static types
///     make the runtime marker dead weight); the constant is kept for parity.
/// Structured representation of a parsed path. (`Path.Parsed`)
type ParsedPath =
    { Root: string
      Dir: string
      Base: string
      Ext: string
      Name: string }

/// The platform path service. (`Path`)
type Path =
    { Sep: string
      Basename: string -> string option -> string
      Dirname: string -> string
      Extname: string -> string
      Format: ParsedPath -> string
      IsAbsolute: string -> bool
      Join: string list -> string
      Normalize: string -> string
      Parse: string -> ParsedPath
      Relative: string -> string -> string
      Resolve: string list -> string
      ToNamespacedPath: string -> string }

[<RequireQualifiedAccess>]
module Path =

    /// Runtime marker stored on `Path` implementations upstream (kept for parity).
    [<Literal>]
    let TypeId = "~effect/platform/Path"

    [<Literal>]
    let private SLASH = 47 // '/'

    [<Literal>]
    let private DOT = 46 // '.'

    let private code (s: string) (i: int) : int = int s.[i]

    /// An empty parsed path — fields default to `""`. (`Path.Parsed` defaults)
    let parsedEmpty: ParsedPath =
        { Root = ""
          Dir = ""
          Base = ""
          Ext = ""
          Name = "" }

    // Resolve `.` and `..` segments. Faithful port of Node's `normalizeStringPosix`.
    let private normalizeStringPosix (path: string) (allowAboveRoot: bool) : string =
        let mutable res = ""
        let mutable lastSegmentLength = 0
        let mutable lastSlash = -1
        let mutable dots = 0
        let mutable c = 0
        let mutable i = 0
        let mutable broke = false

        while i <= path.Length && not broke do
            if i < path.Length then c <- code path i
            elif c = SLASH then broke <- true
            else c <- SLASH

            if not broke then
                if c = SLASH then
                    let mutable skip = false

                    if lastSlash = i - 1 || dots = 1 then
                        ()
                    elif lastSlash <> i - 1 && dots = 2 then
                        let mutable doAboveRoot = true

                        if
                            res.Length < 2
                            || lastSegmentLength <> 2
                            || int res.[res.Length - 1] <> DOT
                            || int res.[res.Length - 2] <> DOT
                        then
                            if res.Length > 2 then
                                let lastSlashIndex = res.LastIndexOf('/')

                                if lastSlashIndex <> res.Length - 1 then
                                    if lastSlashIndex = -1 then
                                        res <- ""
                                        lastSegmentLength <- 0
                                    else
                                        res <- res.Substring(0, lastSlashIndex)
                                        lastSegmentLength <- res.Length - 1 - res.LastIndexOf('/')

                                    lastSlash <- i
                                    dots <- 0
                                    skip <- true
                                    doAboveRoot <- false
                            elif res.Length = 2 || res.Length = 1 then
                                res <- ""
                                lastSegmentLength <- 0
                                lastSlash <- i
                                dots <- 0
                                skip <- true
                                doAboveRoot <- false

                        if doAboveRoot && allowAboveRoot then
                            res <- if res.Length > 0 then res + "/.." else ".."
                            lastSegmentLength <- 2
                    else
                        let seg = path.Substring(lastSlash + 1, i - lastSlash - 1)
                        res <- if res.Length > 0 then res + "/" + seg else seg
                        lastSegmentLength <- i - lastSlash - 1

                    if not skip then
                        lastSlash <- i
                        dots <- 0
                elif c = DOT && dots <> -1 then
                    dots <- dots + 1
                else
                    dots <- -1

                i <- i + 1

        res

#if FABLE_COMPILER
    // Fable shim: `System.IO.Directory.GetCurrentDirectory` is unsupported. Node's
    // `process.cwd()` is the direct analogue — and exactly the current-directory
    // source upstream's POSIX `Path` uses. The `Fable.Core.Emit` attribute is the
    // Fable-only shim declared in `Terminal.fs` (compiled earlier in this assembly);
    // Fable resolves it by full name. (severity: refactor — JS platform layer)
    [<Fable.Core.Emit("process.cwd()")>]
    let private cwd () : string = failwith "Fable emit only"

    let private currentDir () : string = cwd ()
#else
    let private currentDir () : string =
        System.IO.Directory.GetCurrentDirectory()
#endif

    // Faithful port of Node's `path.posix.resolve`.
    let private resolveImpl (segments: string list) : string =
        let args = List.toArray segments
        let mutable resolvedPath = ""
        let mutable resolvedAbsolute = false
        let mutable cwd: string option = None
        let mutable i = args.Length - 1

        while i >= -1 && not resolvedAbsolute do
            let path =
                if i >= 0 then
                    args.[i]
                else
                    match cwd with
                    | Some c -> c
                    | None ->
                        let c = currentDir ()
                        cwd <- Some c
                        c

            if path.Length = 0 then
                ()
            else
                resolvedPath <- path + "/" + resolvedPath
                resolvedAbsolute <- code path 0 = SLASH

            i <- i - 1

        let resolvedPath = normalizeStringPosix resolvedPath (not resolvedAbsolute)

        if resolvedAbsolute then
            if resolvedPath.Length > 0 then "/" + resolvedPath else "/"
        elif resolvedPath.Length > 0 then
            resolvedPath
        else
            "."

    let private normalizeImpl (path: string) : string =
        if path.Length = 0 then
            "."
        else
            let isAbs = code path 0 = SLASH
            let trailing = code path (path.Length - 1) = SLASH
            let mutable p = normalizeStringPosix path (not isAbs)

            if p.Length = 0 && not isAbs then
                p <- "."

            if p.Length > 0 && trailing then
                p <- p + "/"

            if isAbs then "/" + p else p

    let private isAbsoluteImpl (path: string) : bool = path.Length > 0 && code path 0 = SLASH

    let private joinImpl (paths: string list) : string =
        match paths with
        | [] -> "."
        | _ ->
            let mutable joined: string option = None

            for arg in paths do
                if arg.Length > 0 then
                    joined <-
                        Some(
                            match joined with
                            | None -> arg
                            | Some j -> j + "/" + arg
                        )

            match joined with
            | None -> "."
            | Some j -> normalizeImpl j

    // Faithful port of Node's `path.posix.relative`.
    let private relativeImpl (from0: string) (to0: string) : string =
        if from0 = to0 then
            ""
        else
            let from = resolveImpl [ from0 ]
            let to_ = resolveImpl [ to0 ]

            if from = to_ then
                ""
            else
                let mutable fromStart = 1

                while fromStart < from.Length && code from fromStart = SLASH do
                    fromStart <- fromStart + 1

                let fromEnd = from.Length
                let fromLen = fromEnd - fromStart

                let mutable toStart = 1

                while toStart < to_.Length && code to_ toStart = SLASH do
                    toStart <- toStart + 1

                let toEnd = to_.Length
                let toLen = toEnd - toStart

                let length = if fromLen < toLen then fromLen else toLen
                let mutable lastCommonSep = -1
                let mutable i = 0
                let mutable broke = false
                let mutable result: string option = None

                while i <= length && not broke do
                    if i = length then
                        if toLen > length then
                            if code to_ (toStart + i) = SLASH then
                                result <- Some(to_.Substring(toStart + i + 1))
                            elif i = 0 then
                                result <- Some(to_.Substring(toStart + i))
                        elif fromLen > length then
                            if code from (fromStart + i) = SLASH then
                                lastCommonSep <- i
                            elif i = 0 then
                                lastCommonSep <- 0

                        broke <- true
                    else
                        let fromCode = code from (fromStart + i)
                        let toCode = code to_ (toStart + i)

                        if fromCode <> toCode then
                            broke <- true
                        elif fromCode = SLASH then
                            lastCommonSep <- i

                        if not broke then
                            i <- i + 1

                match result with
                | Some r -> r
                | None ->
                    let mutable out = ""
                    let mutable j = fromStart + lastCommonSep + 1

                    while j <= fromEnd do
                        if j = fromEnd || code from j = SLASH then
                            out <- if out.Length = 0 then ".." else out + "/.."

                        j <- j + 1

                    if out.Length > 0 then
                        out + to_.Substring(toStart + lastCommonSep)
                    else
                        let mutable ts = toStart + lastCommonSep

                        if ts < to_.Length && code to_ ts = SLASH then
                            ts <- ts + 1

                        to_.Substring(ts)

    let private dirnameImpl (path: string) : string =
        if path.Length = 0 then
            "."
        else
            let hasRoot = code path 0 = SLASH
            let mutable endIdx = -1
            let mutable matchedSlash = true
            let mutable i = path.Length - 1
            let mutable broke = false

            while i >= 1 && not broke do
                if code path i = SLASH then
                    if not matchedSlash then
                        endIdx <- i
                        broke <- true
                else
                    matchedSlash <- false

                if not broke then
                    i <- i - 1

            if endIdx = -1 then (if hasRoot then "/" else ".")
            elif hasRoot && endIdx = 1 then "//"
            else path.Substring(0, endIdx)

    let private basenameImpl (path: string) (ext: string option) : string =
        let mutable start = 0
        let mutable endIdx = -1
        let mutable matchedSlash = true

        match ext with
        | Some ext when ext.Length > 0 && ext.Length <= path.Length ->
            if ext.Length = path.Length && ext = path then
                ""
            else
                let mutable extIdx = ext.Length - 1
                let mutable firstNonSlashEnd = -1
                let mutable i = path.Length - 1
                let mutable broke = false

                while i >= 0 && not broke do
                    let c = code path i

                    if c = SLASH then
                        if not matchedSlash then
                            start <- i + 1
                            broke <- true
                    else
                        if firstNonSlashEnd = -1 then
                            matchedSlash <- false
                            firstNonSlashEnd <- i + 1

                        if extIdx >= 0 then
                            if c = int ext.[extIdx] then
                                extIdx <- extIdx - 1

                                if extIdx = -1 then
                                    endIdx <- i
                            else
                                extIdx <- -1
                                endIdx <- firstNonSlashEnd

                    if not broke then
                        i <- i - 1

                if start = endIdx then
                    endIdx <- firstNonSlashEnd
                elif endIdx = -1 then
                    endIdx <- path.Length

                path.Substring(start, endIdx - start)
        | _ ->
            let mutable i = path.Length - 1
            let mutable broke = false

            while i >= 0 && not broke do
                if code path i = SLASH then
                    if not matchedSlash then
                        start <- i + 1
                        broke <- true
                elif endIdx = -1 then
                    matchedSlash <- false
                    endIdx <- i + 1

                if not broke then
                    i <- i - 1

            if endIdx = -1 then
                ""
            else
                path.Substring(start, endIdx - start)

    let private extnameImpl (path: string) : string =
        let mutable startDot = -1
        let mutable startPart = 0
        let mutable endIdx = -1
        let mutable matchedSlash = true
        let mutable preDotState = 0
        let mutable i = path.Length - 1
        let mutable broke = false

        while i >= 0 && not broke do
            let c = code path i

            if c = SLASH then
                if not matchedSlash then
                    startPart <- i + 1
                    broke <- true
            else
                if endIdx = -1 then
                    matchedSlash <- false
                    endIdx <- i + 1

                if c = DOT then
                    if startDot = -1 then
                        startDot <- i
                    elif preDotState <> 1 then
                        preDotState <- 1
                elif startDot <> -1 then
                    preDotState <- -1

            if not broke then
                i <- i - 1

        if
            startDot = -1
            || endIdx = -1
            || preDotState = 0
            || (preDotState = 1 && startDot = endIdx - 1 && startDot = startPart + 1)
        then
            ""
        else
            path.Substring(startDot, endIdx - startDot)

    let private parseImpl (path: string) : ParsedPath =
        if path.Length = 0 then
            parsedEmpty
        else
            let isAbs = code path 0 = SLASH
            let mutable root = ""

            let start =
                if isAbs then
                    (root <- "/"
                     1)
                else
                    0

            let mutable startDot = -1
            let mutable startPart = 0
            let mutable endIdx = -1
            let mutable matchedSlash = true
            let mutable preDotState = 0
            let mutable i = path.Length - 1
            let mutable broke = false

            while i >= start && not broke do
                let c = code path i

                if c = SLASH then
                    if not matchedSlash then
                        startPart <- i + 1
                        broke <- true
                else
                    if endIdx = -1 then
                        matchedSlash <- false
                        endIdx <- i + 1

                    if c = DOT then
                        if startDot = -1 then
                            startDot <- i
                        elif preDotState <> 1 then
                            preDotState <- 1
                    elif startDot <> -1 then
                        preDotState <- -1

                if not broke then
                    i <- i - 1

            let mutable name = ""
            let mutable baseStr = ""
            let mutable ext = ""

            if
                startDot = -1
                || endIdx = -1
                || preDotState = 0
                || (preDotState = 1 && startDot = endIdx - 1 && startDot = startPart + 1)
            then
                if endIdx <> -1 then
                    if startPart = 0 && isAbs then
                        baseStr <- path.Substring(1, endIdx - 1)
                        name <- baseStr
                    else
                        baseStr <- path.Substring(startPart, endIdx - startPart)
                        name <- baseStr
            else
                if startPart = 0 && isAbs then
                    name <- path.Substring(1, startDot - 1)
                    baseStr <- path.Substring(1, endIdx - 1)
                else
                    name <- path.Substring(startPart, startDot - startPart)
                    baseStr <- path.Substring(startPart, endIdx - startPart)

                ext <- path.Substring(startDot, endIdx - startDot)

            let dir =
                if startPart > 0 then path.Substring(0, startPart - 1)
                elif isAbs then "/"
                else ""

            { Root = root
              Dir = dir
              Base = baseStr
              Ext = ext
              Name = name }

    let private formatImpl (p: ParsedPath) : string =
        let dir = if p.Dir <> "" then p.Dir else p.Root
        let baseStr = if p.Base <> "" then p.Base else p.Name + p.Ext

        if dir = "" then baseStr
        elif dir = p.Root then dir + baseStr
        else dir + "/" + baseStr

    /// The built-in POSIX `Path` implementation. (`posixImpl`)
    let posix: Path =
        { Sep = "/"
          Basename = basenameImpl
          Dirname = dirnameImpl
          Extname = extnameImpl
          Format = formatImpl
          IsAbsolute = isAbsoluteImpl
          Join = joinImpl
          Normalize = normalizeImpl
          Parse = parseImpl
          Relative = relativeImpl
          Resolve = resolveImpl
          ToNamespacedPath = id }

    /// Service tag for the `Path` service. (`Path.Path`)
    let tag: Tag<Path> = Tag.make<Path> "effect/Path"

    /// Layer providing the built-in POSIX `Path` implementation. (`Path.layer`)
    let layer<'E, 'RIn> : Layer<'E, 'RIn> = Layer.succeed tag posix

    /// Access the `Path` service from the environment. (`yield* Path.Path`)
    let path<'E> : Effect<Path, 'E, Context> = Effect.service tag
