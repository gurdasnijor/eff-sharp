module Effect.Tests.PlatformErrorTests

open Xunit
open Effect

// No upstream PlatformError.test.ts exists; these assert the message-formatting
// and reason-wrapping behaviour described in
// repos/effect-smol/packages/effect/src/PlatformError.ts (the `.message` getters
// and the `systemError` / `badArgument` constructors).

let private badArg =
    { Module = "FileSystem"
      Method = "readFile"
      Description = None
      Cause = None }

[<Fact>]
let ``badArgument message without description`` () =
    let e = PlatformError.badArgument badArg
    Assert.Equal("FileSystem.readFile", e.Message)
    Assert.Equal("PlatformError", e.Tag)

[<Fact>]
let ``badArgument message with description`` () =
    let e =
        PlatformError.badArgument
            { badArg with
                Description = Some "empty path" }

    Assert.Equal("FileSystem.readFile: empty path", e.Message)

[<Fact>]
let ``systemError message with tag, path and description`` () =
    let e =
        PlatformError.systemError
            { Tag = NotFound
              Module = "FileSystem"
              Method = "readFile"
              Description = Some "missing"
              Syscall = None
              PathOrDescriptor = Some(box "/tmp/x")
              Cause = None }

    Assert.Equal("NotFound: FileSystem.readFile (/tmp/x): missing", e.Message)

[<Fact>]
let ``systemError message without optional details`` () =
    let e =
        PlatformError.systemError
            { Tag = PermissionDenied
              Module = "FileSystem"
              Method = "open"
              Description = None
              Syscall = None
              PathOrDescriptor = None
              Cause = None }

    Assert.Equal("PermissionDenied: FileSystem.open", e.Message)

[<Fact>]
let ``reasons have structural equality`` () =
    let a = PlatformError.badArgument badArg
    let b = PlatformError.badArgument badArg
    Assert.True((a.Reason = b.Reason))
    Assert.Equal<Reason>(BadArgumentReason badArg, a.Reason)

[<Fact>]
let ``cause is preserved on the wrapper`` () =
    let inner = exn "boom"
    let e = PlatformError.badArgument { badArg with Cause = Some(box inner) }
    Assert.Equal<obj option>(Some(box inner), e.Cause)
    Assert.Same(inner, e.InnerException)
