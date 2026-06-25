module PlatformErrorSpec

open Effect
open Effect.Vitest

let private badArg =
    { Module = "FileSystem"
      Method = "readFile"
      Description = None
      Cause = None }

describe "PlatformError" (fun () ->
    test "badArgument message formatting" (fun () ->
        let e = PlatformError.badArgument badArg
        toBe e.Message "FileSystem.readFile"
        toBe e.Tag "PlatformError"

        let described =
            PlatformError.badArgument
                { badArg with
                    Description = Some "empty path" }

        toBe described.Message "FileSystem.readFile: empty path")

    test "systemError message formatting" (fun () ->
        let withDetails =
            PlatformError.systemError
                { Tag = NotFound
                  Module = "FileSystem"
                  Method = "readFile"
                  Description = Some "missing"
                  Syscall = None
                  PathOrDescriptor = Some(box "/tmp/x")
                  Cause = None }

        toBe withDetails.Message "NotFound: FileSystem.readFile (/tmp/x): missing"

        let minimal =
            PlatformError.systemError
                { Tag = PermissionDenied
                  Module = "FileSystem"
                  Method = "open"
                  Description = None
                  Syscall = None
                  PathOrDescriptor = None
                  Cause = None }

        toBe minimal.Message "PermissionDenied: FileSystem.open")

    test "reasons and causes are preserved" (fun () ->
        let a = PlatformError.badArgument badArg
        let b = PlatformError.badArgument badArg
        toBe (a.Reason = b.Reason) true
        toEqual a.Reason (BadArgumentReason badArg)

        let inner = exn "boom"
        let e = PlatformError.badArgument { badArg with Cause = Some(box inner) }
        toEqual e.Cause (Some(box inner))
        toBe (obj.ReferenceEquals(e.InnerException, inner)) true))

