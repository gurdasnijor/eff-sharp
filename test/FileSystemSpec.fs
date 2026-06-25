module FileSystemSpec

open Effect
open Effect.Vitest

let private fakeFileSystem () =
    let files = System.Collections.Generic.Dictionary<string, string>()
    let directories = System.Collections.Generic.HashSet<string>()

    { ReadFileString = fun path -> Effect.succeed files.[path]
      WriteFileString = fun path data -> Effect.sync (fun () -> files.[path] <- data)
      Exists = fun path -> Effect.succeed (files.ContainsKey path || directories.Contains path)
      MakeDirectory = fun path _ -> Effect.sync (fun () -> directories.Add path |> ignore)
      ReadDirectory =
        fun path _ ->
            Effect.succeed
                ([ yield! files.Keys
                   yield! directories ]
                 |> List.ofSeq
                 |> List.filter (fun entry -> entry.StartsWith(path + "/"))
                 |> List.map (fun entry -> entry.Substring(path.Length + 1)))
      Remove =
        fun path _ ->
            Effect.sync (fun () ->
                files.Remove path |> ignore
                directories.Remove path |> ignore)
      Stat =
        fun path ->
            Effect.succeed
                { Type = if directories.Contains path then FileType.Directory else FileType.File
                  Mtime = None
                  Atime = None
                  Birthtime = None
                  Dev = 0
                  Ino = None
                  Mode = 0
                  Nlink = None
                  Uid = None
                  Gid = None
                  Rdev = None
                  Size = files.TryGetValue path |> function | true, value -> int64 value.Length | _ -> 0L
                  Blksize = None
                  Blocks = None }
      MakeTempDirectory =
        fun options ->
            Effect.sync (fun () ->
                let path = (defaultArg options.Directory "/tmp") + "/" + (defaultArg options.Prefix "tmp") + "1"
                directories.Add path |> ignore
                path) }

describe "FileSystem" (fun () ->
    itEffectIn (Context.make FileSystem.tag (fakeFileSystem ())) "accessors dispatch to the provided service" (fun () ->
        FileSystem.makeDirectory "/workspace" FileSystem.makeDirectoryOptions
        |> Effect.zipRight (FileSystem.writeFileString "/workspace/file.txt" "contents")
        |> Effect.zipRight (FileSystem.readFileString "/workspace/file.txt")
        |> Effect.flatMap (fun contents ->
            FileSystem.exists "/workspace/file.txt"
            |> Effect.flatMap (fun exists ->
                FileSystem.stat "/workspace/file.txt"
                |> Effect.flatMap (fun info ->
                    FileSystem.readDirectory "/workspace" FileSystem.readDirectoryOptions
                    |> Effect.map (fun entries ->
                        toBe contents "contents"
                        toBe exists true
                        toBe (info.Type = FileType.File) true
                        toEqual entries [ "file.txt" ])))))

    itEffectIn (Context.make FileSystem.tag (fakeFileSystem ())) "makeTempDirectoryScoped removes the directory on scope close" (fun () ->
        Scope.scoped (fun scope ->
            FileSystem.makeTempDirectoryScoped scope { Directory = Some "/tmp"; Prefix = Some "scoped-" }
            |> Effect.flatMap (fun dir ->
                FileSystem.exists dir
                |> Effect.map (fun exists ->
                    toBe dir "/tmp/scoped-1"
                    toBe exists true)))))
