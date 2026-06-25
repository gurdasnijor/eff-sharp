module Program

open Effect

// Node (`node:fs`) smoke test for the REAL src/Effect/FileSystem.fs, driven
// through the Fable-compiled Node Layer (`FileSystem.liveContext`). This is the
// real proof that the `node:fs` bindings work on Node v24: write a temp file,
// read it back, assert contents; exists true/false; stat; mkdir+readdir; remove;
// and scoped temp-dir auto-cleanup. Effects are sequenced with flatMap/zipRight
// (the proven Fable pattern from ../Program.fs) and run through the async path.

let mutable passed = 0
let mutable failed = 0

let check (name: string) (cond: bool) (detail: string) =
    if cond then
        passed <- passed + 1
        printfn "  PASS  %s  ->  %s" name detail
    else
        failed <- failed + 1
        printfn "  FAIL  %s  ->  %s" name detail

// ---- the main round-trip: returns a tuple of everything we want to assert ----
let private program =
    FileSystem.makeTempDirectory
        { FileSystem.tempOptions with
            Prefix = Some "effsharp-node-fs-" }
    |> Effect.flatMap (fun dir ->
        let file = dir + "/hello.txt"
        let sub = dir + "/sub"

        FileSystem.writeFileString file "hello from node:fs"
        |> Effect.zipRight (FileSystem.readFileString file)
        |> Effect.flatMap (fun back ->
            FileSystem.exists file
            |> Effect.flatMap (fun existsFile ->
                FileSystem.exists (dir + "/nope.txt")
                |> Effect.flatMap (fun existsMissing ->
                    FileSystem.stat file
                    |> Effect.flatMap (fun info ->
                        FileSystem.makeDirectory sub FileSystem.makeDirectoryOptions
                        |> Effect.zipRight (FileSystem.readDirectory dir FileSystem.readDirectoryOptions)
                        |> Effect.flatMap (fun entries ->
                            FileSystem.remove dir FileSystem.removeRecursive
                            |> Effect.zipRight (FileSystem.exists dir)
                            |> Effect.map (fun gone ->
                                (back, existsFile, existsMissing, info.Size, info.Type, List.sort entries, gone))))))))

// ---- scoped temp dir: exists inside the scope, removed once it closes ----
let private scopedProgram =
    Scope.scoped (fun scope ->
        FileSystem.makeTempDirectoryScoped scope FileSystem.tempOptions
        |> Effect.flatMap (fun d -> FileSystem.exists d |> Effect.map (fun inside -> (d, inside))))
    |> Effect.flatMap (fun (d, inside) -> FileSystem.exists d |> Effect.map (fun after -> (inside, after)))

let private asyncTests: Async<unit> =
    async {
        let! e1 = Effect.runExit FileSystem.liveContext program

        match e1 with
        | Success(back, existsFile, existsMissing, size, typ, entries, gone) ->
            check "readFileString round-trips written contents" (back = "hello from node:fs") back
            check "exists(file) = true" existsFile (string existsFile)
            check "exists(missing) = false" (not existsMissing) (string existsMissing)
            check "stat size = 18 bytes" (size = 18L) (string size)
            check "stat type = File" (typ = FileType.File) (sprintf "%A" typ)
            check "readDirectory = [hello.txt; sub]" (entries = [ "hello.txt"; "sub" ]) (sprintf "%A" entries)
            check "remove recursive -> dir gone" (not gone) (string gone)
        | Failure c ->
            failed <- failed + 1
            printfn "  FAIL  program  ->  %s" (Cause.render c)

        let! e2 = Effect.runExit FileSystem.liveContext scopedProgram

        match e2 with
        | Success(inside, after) ->
            check "scoped temp dir exists inside the scope" inside (string inside)
            check "scoped temp dir removed after the scope closes" (not after) (string after)
        | Failure c ->
            failed <- failed + 1
            printfn "  FAIL  scoped  ->  %s" (Cause.render c)

        printfn ""
        printfn "==== RESULTS: %d passed, %d failed ====" passed failed
    }

[<EntryPoint>]
let main _ =
    printfn "==== eff-sharp FileSystem Node (node:fs) smoke ===="
    // Bridge the async workflow to the JS event loop without blocking.
    Async.StartImmediate asyncTests
    0
