module NodeFileSystemSpec

open Effect
open Effect.Platform.Node
open Effect.Vitest

let private nodeFs = NodeFileSystem.liveContext

describe "NodeFileSystem" (fun () ->
    itEffectIn nodeFs "writes, reads, stats, lists, and removes files" (fun () ->
        let tempOptions = { Directory = None; Prefix = Some "eff-sharp-fs-" }

        FileSystem.makeTempDirectory tempOptions
        |> Effect.flatMap (fun dir ->
            let nested = dir + "/nested"
            let file = nested + "/hello.txt"

            FileSystem.makeDirectory nested FileSystem.makeDirectoryRecursive
            |> Effect.zipRight (FileSystem.writeFileString file "hello")
            |> Effect.zipRight (FileSystem.readFileString file)
            |> Effect.flatMap (fun contents ->
                FileSystem.stat file
                |> Effect.flatMap (fun info ->
                    FileSystem.readDirectory dir { Recursive = true }
                    |> Effect.flatMap (fun entries ->
                        FileSystem.exists file
                        |> Effect.flatMap (fun existsBefore ->
                            FileSystem.remove dir FileSystem.removeRecursive
                            |> Effect.flatMap (fun () ->
                                FileSystem.exists dir
                                |> Effect.map (fun existsAfter ->
                                    toBe contents "hello"
                                    toBe (info.Type = FileType.File) true
                                    toBe (info.Size >= 5L) true
                                    toBe (entries |> List.contains "nested/hello.txt") true
                                    toBe existsBefore true
                                    toBe existsAfter false))))))))

    itEffectIn nodeFs "force remove ignores missing targets" (fun () ->
        FileSystem.remove "/tmp/eff-sharp-missing-target" FileSystem.removeRecursive))
