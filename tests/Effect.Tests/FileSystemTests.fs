module Effect.Tests.FileSystemTests

open Xunit
open Effect

// Upstream ships no FileSystem.test.ts for the abstract service; these Facts
// exercise the ported operation set against the REAL `.NET` (`System.IO`) Layer
// — the platform test surface per the brief. They do real I/O under the system
// temp directory and clean up after themselves. The Node (`node:fs`) Layer is
// proven separately by the Fable probe + Node smoke test (see fable-spike/filesystem).

let private prefixed (p: string) : MakeTempOptions =
    { FileSystem.tempOptions with
        Prefix = Some p }

let private runExit (program: Effect<'A, PlatformError, Context>) : Exit<'A, PlatformError> =
    // `liveContext` provides the platform FileSystem; the ops are synchronous, so
    // `runSync` completes without suspending.
    Effect.runSync FileSystem.liveContext program

// ----------------------------------------------------------------------------
// write -> read -> stat -> exists -> remove  (the brief's canonical round-trip)
// ----------------------------------------------------------------------------

[<Fact>]
let ``write, read, stat, exists round-trip on the .NET FileSystem`` () =
    let program =
        effect {
            let! dir = FileSystem.makeTempDirectory (prefixed "effsharp-fs-rt-")
            let file = System.IO.Path.Combine(dir, "hello.txt")
            do! FileSystem.writeFileString file "hello, eff-sharp"
            let! back = FileSystem.readFileString file
            let! existsFile = FileSystem.exists file
            let! existsMissing = FileSystem.exists (System.IO.Path.Combine(dir, "nope.txt"))
            let! info = FileSystem.stat file
            do! FileSystem.remove dir FileSystem.removeRecursive
            let! existsAfter = FileSystem.exists dir
            return (back, existsFile, existsMissing, info, existsAfter)
        }

    match runExit program with
    | Success(back, existsFile, existsMissing, info, existsAfter) ->
        Assert.Equal("hello, eff-sharp", back)
        Assert.True(existsFile, "the written file should exist")
        Assert.False(existsMissing, "a sibling that was never written should not exist")
        Assert.Equal(FileType.File, info.Type)
        Assert.Equal(16L, info.Size) // "hello, eff-sharp" is 16 ASCII bytes
        Assert.False(existsAfter, "the temp dir should be gone after recursive remove")
    | Failure c -> failwithf "unexpected failure: %s" (Cause.render c)

// ----------------------------------------------------------------------------
// makeDirectory + readDirectory
// ----------------------------------------------------------------------------

[<Fact>]
let ``makeDirectory then readDirectory lists the children`` () =
    let program =
        effect {
            let! root = FileSystem.makeTempDirectory (prefixed "effsharp-fs-dir-")
            do! FileSystem.makeDirectory (System.IO.Path.Combine(root, "sub")) FileSystem.makeDirectoryOptions
            do! FileSystem.writeFileString (System.IO.Path.Combine(root, "a.txt")) "a"
            do! FileSystem.writeFileString (System.IO.Path.Combine(root, "b.txt")) "b"
            let! entries = FileSystem.readDirectory root FileSystem.readDirectoryOptions
            do! FileSystem.remove root FileSystem.removeRecursive
            return List.sort entries
        }

    match runExit program with
    | Success entries -> Assert.Equal<string list>([ "a.txt"; "b.txt"; "sub" ], entries)
    | Failure c -> failwithf "unexpected failure: %s" (Cause.render c)

[<Fact>]
let ``makeDirectory recursive creates intermediate directories`` () =
    let program =
        effect {
            let! root = FileSystem.makeTempDirectory (prefixed "effsharp-fs-mkp-")
            let nested = System.IO.Path.Combine(root, "x", "y", "z")
            do! FileSystem.makeDirectory nested FileSystem.makeDirectoryRecursive
            let! present = FileSystem.exists nested
            let! info = FileSystem.stat nested
            do! FileSystem.remove root FileSystem.removeRecursive
            return (present, info.Type)
        }

    match runExit program with
    | Success(present, typ) ->
        Assert.True(present, "the nested directory chain should exist")
        Assert.Equal(FileType.Directory, typ)
    | Failure c -> failwithf "unexpected failure: %s" (Cause.render c)

// ----------------------------------------------------------------------------
// makeTempDirectoryScoped — auto-cleanup on scope close (verification uses this)
// ----------------------------------------------------------------------------

[<Fact>]
let ``makeTempDirectoryScoped removes the directory when the scope closes`` () =
    let program: Effect<bool * bool, PlatformError, Context> =
        Scope.scoped (fun scope ->
            effect {
                let! dir = FileSystem.makeTempDirectoryScoped scope FileSystem.tempOptions
                let! inScope = FileSystem.exists dir
                return (dir, inScope)
            })
        |> Effect.flatMap (fun (dir, inScope) ->
            FileSystem.exists dir |> Effect.map (fun afterScope -> (inScope, afterScope)))

    match runExit program with
    | Success(inScope, afterScope) ->
        Assert.True(inScope, "temp dir should exist inside the scope")
        Assert.False(afterScope, "temp dir should be removed once the scope closes")
    | Failure c -> failwithf "unexpected failure: %s" (Cause.render c)

// ----------------------------------------------------------------------------
// error channel — host failures normalize to a typed PlatformError
// ----------------------------------------------------------------------------

[<Fact>]
let ``reading a missing file fails with a typed PlatformError (NotFound)`` () =
    let missing =
        System.IO.Path.Combine(System.IO.Path.GetTempPath(), "effsharp-absent-" + System.Guid.NewGuid().ToString("N"))

    match runExit (FileSystem.readFileString missing) with
    | Success _ -> Assert.Fail "expected a failure reading a missing file"
    | Failure cause ->
        match Cause.failures cause with
        | (e: PlatformError) :: _ ->
            Assert.Equal("PlatformError", e.Tag)

            match e.Reason with
            | SystemErrorReason s ->
                Assert.Equal(NotFound, s.Tag)
                Assert.Equal("FileSystem", s.Module)
                Assert.Equal("readFileString", s.Method)
            | BadArgumentReason _ -> Assert.Fail "expected a SystemError reason"
        | [] -> Assert.Fail "expected a typed PlatformError in the error channel"

[<Fact>]
let ``non-recursive makeDirectory over an existing path fails with AlreadyExists`` () =
    let program =
        effect {
            let! root = FileSystem.makeTempDirectory (prefixed "effsharp-fs-eexist-")
            // `root` already exists; a non-recursive mkdir must reject it.
            return! FileSystem.makeDirectory root FileSystem.makeDirectoryOptions
        }

    match runExit program with
    | Success _ -> Assert.Fail "expected mkdir over an existing directory to fail"
    | Failure cause ->
        match Cause.failures cause with
        | (e: PlatformError) :: _ ->
            match e.Reason with
            | SystemErrorReason s -> Assert.Equal(AlreadyExists, s.Tag)
            | BadArgumentReason _ -> Assert.Fail "expected a SystemError reason"
        | [] -> Assert.Fail "expected a typed PlatformError in the error channel"

// ----------------------------------------------------------------------------
// service / layer wiring — `FileSystem.layer` is what `verification` provides
// ----------------------------------------------------------------------------

[<Fact>]
let ``layer provides a working FileSystem`` () =
    let program =
        effect {
            let! dir = FileSystem.makeTempDirectory (prefixed "effsharp-fs-layer-")
            let! present = FileSystem.exists dir
            do! FileSystem.remove dir FileSystem.removeRecursive
            return present
        }

    // Mirrors PathTests: provide the layer, discharge Context, run with unit env.
    match Effect.runSync () (Layer.provide FileSystem.layer program) with
    | Success present -> Assert.True(present, "the layer-provided FileSystem should create the temp dir")
    | Failure c -> failwithf "unexpected failure: %s" (Cause.render c)
