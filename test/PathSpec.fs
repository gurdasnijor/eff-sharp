module PathSpec

open Effect
open Effect.Vitest

let private p = Path.posix

describe "Path" (fun () ->
    test "join" (fun () ->
        toBe (p.Join [ "/foo"; "bar"; "baz/asdf"; "quux"; ".." ]) "/foo/bar/baz/asdf"
        toBe (p.Join [ "foo"; "../../bar" ]) "../bar"
        toBe (p.Join [ "a"; ""; "b" ]) "a/b"
        toBe (p.Join []) "."
        toBe (p.Join [ ""; "foo" ]) "foo")

    test "normalize" (fun () ->
        toBe (p.Normalize "/foo/bar//baz/asdf/quux/..") "/foo/bar/baz/asdf"
        toBe (p.Normalize "./path/../to/file.txt") "to/file.txt"
        toBe (p.Normalize "a//b//../b") "a/b"
        toBe (p.Normalize "a//b//./c") "a/b/c"
        toBe (p.Normalize "") "."
        toBe (p.Normalize "/") "/"
        toBe (p.Normalize "fixtures///b/../b/c.js") "fixtures/b/c.js")

    test "basename dirname extname" (fun () ->
        toBe (p.Basename "/foo/bar/baz/asdf/quux.html" None) "quux.html"
        toBe (p.Basename "/foo/bar/baz/asdf/quux.html" (Some ".html")) "quux"
        toBe (p.Basename "/path/to/file.txt" None) "file.txt"
        toBe (p.Basename "/path/to/file.txt" (Some ".txt")) "file"
        toBe (p.Basename "/foo/" None) "foo"
        toBe (p.Basename "" None) ""
        toBe (p.Dirname "/path/to/file.txt") "/path/to"
        toBe (p.Dirname "/a/b/c") "/a/b"
        toBe (p.Dirname "/a") "/"
        toBe (p.Dirname "a") "."
        toBe (p.Dirname "") "."
        toBe (p.Extname "index.html") ".html"
        toBe (p.Extname "index.coffee.md") ".md"
        toBe (p.Extname "index.") "."
        toBe (p.Extname "index") ""
        toBe (p.Extname ".index") ""
        toBe (p.Extname ".index.md") ".md"
        toBe (p.Extname "/path/to/file.txt") ".txt")

    test "isAbsolute resolve relative" (fun () ->
        toBe (p.IsAbsolute "/foo/bar") true
        toBe (p.IsAbsolute "/foo/..") true
        toBe (p.IsAbsolute "qux/") false
        toBe (p.IsAbsolute ".") false
        toBe (p.IsAbsolute "") false
        toBe (p.Resolve [ "/foo/bar"; "./baz" ]) "/foo/bar/baz"
        toBe (p.Resolve [ "/foo/bar"; "/tmp/file/" ]) "/tmp/file"
        toBe (p.Resolve [ "/a"; "b"; "c" ]) "/a/b/c"
        toBe (p.Resolve [ "/foo/bar"; "baz" ]) "/foo/bar/baz"
        toBe (p.Relative "/data/orandea/test/aaa" "/data/orandea/impl/bbb") "../../impl/bbb"
        toBe (p.Relative "/foo/bar" "/foo/bar/baz") "baz"
        toBe (p.Relative "/foo/bar/baz" "/foo/bar") ".."
        toBe (p.Relative "/a/b/c" "/a/b/c") "")

    test "parse and format" (fun () ->
        toEqual
            (p.Parse "/home/user/dir/file.txt")
            { Root = "/"
              Dir = "/home/user/dir"
              Base = "file.txt"
              Ext = ".txt"
              Name = "file" }

        toEqual
            (p.Parse "file.txt")
            { Root = ""
              Dir = ""
              Base = "file.txt"
              Ext = ".txt"
              Name = "file" }

        toBe
            (p.Format
                { Path.parsedEmpty with
                    Dir = "/home/user"
                    Name = "newfile"
                    Ext = ".ts" })
            "/home/user/newfile.ts"

        toBe
            (p.Format
                { Path.parsedEmpty with
                    Root = "/"
                    Base = "file.txt" })
            "/file.txt"

        toBe
            (p.Format
                { Path.parsedEmpty with
                    Name = "x"
                    Ext = ".y" })
            "x.y"

        let parsed = p.Parse "/home/user/dir/file.txt"
        toBe (p.Format parsed) "/home/user/dir/file.txt")

    test "sep and toNamespacedPath" (fun () ->
        toBe p.Sep "/"
        toBe (p.ToNamespacedPath "/a/b") "/a/b")

    itEffect "layer provides the POSIX Path service" (fun () ->
        Path.path
        |> Effect.map (fun path -> path.Join [ "home"; "user"; "file.txt" ])
        |> Layer.provide Path.layer
        |> Effect.map (fun joined -> toBe joined "home/user/file.txt"))

    itEffectIn (Context.make Path.tag Path.posix)
        "service can be read from a Context"
        (fun () ->
            Path.path
            |> Effect.map (fun path -> toBe path.Sep "/")))

