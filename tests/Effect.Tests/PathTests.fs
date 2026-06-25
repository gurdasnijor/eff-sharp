module Effect.Tests.PathTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/src/Path.ts (no upstream
// Path.test.ts exists). The POSIX implementation is adapted char-for-char from
// Node's `path.posix`, so these Facts assert the documented Node POSIX vectors.

let private p = Path.posix

// ----------------------------------------------------------------------------
// join / normalize
// ----------------------------------------------------------------------------

[<Fact>]
let ``join`` () =
    Assert.Equal("/foo/bar/baz/asdf", p.Join [ "/foo"; "bar"; "baz/asdf"; "quux"; ".." ])
    Assert.Equal("../bar", p.Join [ "foo"; "../../bar" ])
    Assert.Equal("a/b", p.Join [ "a"; ""; "b" ])
    Assert.Equal(".", p.Join [])
    Assert.Equal("foo", p.Join [ ""; "foo" ])

[<Fact>]
let ``normalize`` () =
    Assert.Equal("/foo/bar/baz/asdf", p.Normalize "/foo/bar//baz/asdf/quux/..")
    Assert.Equal("to/file.txt", p.Normalize "./path/../to/file.txt")
    Assert.Equal("a/b", p.Normalize "a//b//../b")
    Assert.Equal("a/b/c", p.Normalize "a//b//./c")
    Assert.Equal(".", p.Normalize "")
    Assert.Equal("/", p.Normalize "/")
    Assert.Equal("fixtures/b/c.js", p.Normalize "fixtures///b/../b/c.js")

// ----------------------------------------------------------------------------
// basename / dirname / extname
// ----------------------------------------------------------------------------

[<Fact>]
let ``basename`` () =
    Assert.Equal("quux.html", p.Basename "/foo/bar/baz/asdf/quux.html" None)
    Assert.Equal("quux", p.Basename "/foo/bar/baz/asdf/quux.html" (Some ".html"))
    Assert.Equal("file.txt", p.Basename "/path/to/file.txt" None)
    Assert.Equal("file", p.Basename "/path/to/file.txt" (Some ".txt"))
    Assert.Equal("foo", p.Basename "/foo/" None)
    Assert.Equal("", p.Basename "" None)

[<Fact>]
let ``dirname`` () =
    Assert.Equal("/path/to", p.Dirname "/path/to/file.txt")
    Assert.Equal("/a/b", p.Dirname "/a/b/c")
    Assert.Equal("/", p.Dirname "/a")
    Assert.Equal(".", p.Dirname "a")
    Assert.Equal(".", p.Dirname "")

[<Fact>]
let ``extname`` () =
    Assert.Equal(".html", p.Extname "index.html")
    Assert.Equal(".md", p.Extname "index.coffee.md")
    Assert.Equal(".", p.Extname "index.")
    Assert.Equal("", p.Extname "index")
    Assert.Equal("", p.Extname ".index")
    Assert.Equal(".md", p.Extname ".index.md")
    Assert.Equal(".txt", p.Extname "/path/to/file.txt")

// ----------------------------------------------------------------------------
// isAbsolute / resolve / relative
// ----------------------------------------------------------------------------

[<Fact>]
let ``isAbsolute`` () =
    Assert.True(p.IsAbsolute "/foo/bar")
    Assert.True(p.IsAbsolute "/foo/..")
    Assert.False(p.IsAbsolute "qux/")
    Assert.False(p.IsAbsolute ".")
    Assert.False(p.IsAbsolute "")

[<Fact>]
let ``resolve - with absolute segments`` () =
    Assert.Equal("/foo/bar/baz", p.Resolve [ "/foo/bar"; "./baz" ])
    Assert.Equal("/tmp/file", p.Resolve [ "/foo/bar"; "/tmp/file/" ])
    Assert.Equal("/a/b/c", p.Resolve [ "/a"; "b"; "c" ])
    Assert.Equal("/foo/bar/baz", p.Resolve [ "/foo/bar"; "baz" ])

[<Fact>]
let ``relative`` () =
    Assert.Equal("../../impl/bbb", p.Relative "/data/orandea/test/aaa" "/data/orandea/impl/bbb")
    Assert.Equal("baz", p.Relative "/foo/bar" "/foo/bar/baz")
    Assert.Equal("..", p.Relative "/foo/bar/baz" "/foo/bar")
    Assert.Equal("", p.Relative "/a/b/c" "/a/b/c")

// ----------------------------------------------------------------------------
// parse / format / toNamespacedPath / sep
// ----------------------------------------------------------------------------

[<Fact>]
let ``parse - absolute`` () =
    Assert.Equal(
        { Root = "/"; Dir = "/home/user/dir"; Base = "file.txt"; Ext = ".txt"; Name = "file" },
        p.Parse "/home/user/dir/file.txt"
    )

[<Fact>]
let ``parse - relative`` () =
    Assert.Equal(
        { Root = ""; Dir = ""; Base = "file.txt"; Ext = ".txt"; Name = "file" },
        p.Parse "file.txt"
    )

[<Fact>]
let ``format`` () =
    Assert.Equal("/home/user/newfile.ts", p.Format { Path.parsedEmpty with Dir = "/home/user"; Name = "newfile"; Ext = ".ts" })
    Assert.Equal("/file.txt", p.Format { Path.parsedEmpty with Root = "/"; Base = "file.txt" })
    Assert.Equal("x.y", p.Format { Path.parsedEmpty with Name = "x"; Ext = ".y" })

[<Fact>]
let ``parse then format round-trips`` () =
    let parsed = p.Parse "/home/user/dir/file.txt"
    Assert.Equal("/home/user/dir/file.txt", p.Format parsed)

[<Fact>]
let ``sep and toNamespacedPath`` () =
    Assert.Equal("/", p.Sep)
    Assert.Equal("/a/b", p.ToNamespacedPath "/a/b")

// ----------------------------------------------------------------------------
// service / layer wiring
// ----------------------------------------------------------------------------

[<Fact>]
let ``layer provides the POSIX Path service`` () =
    let program: Effect<string, string, Context> =
        Path.path |> Effect.map (fun path -> path.Join [ "home"; "user"; "file.txt" ])
    Assert.Equal<Exit<string, string>>(Exit.succeed "home/user/file.txt", Effect.runSync () (Layer.provide Path.layer program))

[<Fact>]
let ``service can be read from a Context`` () =
    let program: Effect<string, string, Context> = Path.path |> Effect.map (fun path -> path.Sep)
    Assert.Equal<Exit<string, string>>(Exit.succeed "/", Effect.runSync (Context.make Path.tag Path.posix) program)
