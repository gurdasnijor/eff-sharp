namespace Effect

/// Port of repos/effect-smol/packages/effect/src/PlatformError.ts.
///
/// Normalized errors for platform APIs (file systems, terminals, sockets). A
/// `PlatformError` wraps a `reason` that is either a `BadArgument` (caller input
/// rejected before an operation runs) or a `SystemError` (a host/OS failure with
/// a normalized tag).
///
/// Idiomatic lowering (CONVENTIONS §1, §2, §5):
///   - Upstream's `Data.TaggedError`/`Data.Error` classes (which exist only to
///     graft structural equality onto JS objects) become an F# exception whose
///     payload is a record DU — F# gives structural equality for free, so the
///     `Data` dependency is dropped and noted here per the brief's decoupling
///     rule.
///   - `SystemErrorTag` is a discriminated union; its `_tag` lowers onto the
///     active case, matched with native `match`.
///   - The `TypeId` runtime brand is kept only as a `[<Literal>]` for parity;
///     the `[TypeId]` instance marker used for JS guards is dropped (the F# type
///     is the guard).

open System

/// Normalized category for host/system operation failures. (`SystemErrorTag`)
type SystemErrorTag =
    | AlreadyExists
    | BadResource
    | Busy
    | InvalidData
    | NotFound
    | PermissionDenied
    | TimedOut
    | UnexpectedEof
    | Unknown
    | WouldBlock
    | WriteZero

/// Reason data for an invalid argument rejected before a platform operation.
/// (`BadArgument`)
type BadArgument =
    { Module: string
      Method: string
      Description: string option
      Cause: obj option }

/// Reason data for a platform/system operation failure. `PathOrDescriptor` is a
/// string path or numeric descriptor. (`SystemError`)
type SystemError =
    { Tag: SystemErrorTag
      Module: string
      Method: string
      Description: string option
      Syscall: string option
      PathOrDescriptor: obj option
      Cause: obj option }

/// The underlying reason of a `PlatformError`. (`reason: BadArgument | SystemError`)
type Reason =
    | BadArgumentReason of BadArgument
    | SystemErrorReason of SystemError

[<RequireQualifiedAccess>]
module Reason =

    let private describe (d: string option) =
        match d with
        | Some s -> ": " + s
        | None -> ""

    /// The formatted message for a reason (upstream `.message` getters).
    let message (reason: Reason) : string =
        match reason with
        | BadArgumentReason a -> sprintf "%s.%s%s" a.Module a.Method (describe a.Description)
        | SystemErrorReason s ->
            // F# DU ToString renders a nullary case as its case name (the `_tag`).
            let tag = string s.Tag
            let path =
                match s.PathOrDescriptor with
                | Some p -> sprintf " (%O)" p
                | None -> ""
            sprintf "%s: %s.%s%s%s" tag s.Module s.Method path (describe s.Description)

    /// The preserved cause of a reason, if any.
    let cause (reason: Reason) : obj option =
        match reason with
        | BadArgumentReason a -> a.Cause
        | SystemErrorReason s -> s.Cause

/// Tagged error used by platform APIs to report invalid arguments or system
/// failures through a single error channel. (`PlatformError`)
[<Sealed>]
type PlatformError(reason: Reason) =
    inherit Exception(
        Reason.message reason,
        (match Reason.cause reason with
         | Some (:? exn as e) -> e
         | _ -> null))

    /// The discriminator tag (effect's `_tag`).
    member _.Tag : string = "PlatformError"

    /// The underlying `BadArgument` or `SystemError`. (`reason`)
    member _.Reason : Reason = reason

    /// The preserved cause, if the reason carried one. (`cause`)
    member _.Cause : obj option = Reason.cause reason

    override _.Message : string = Reason.message reason

[<RequireQualifiedAccess>]
module PlatformError =


    /// Create a `PlatformError` whose reason is a `SystemError`. (`systemError`)
    let systemError (error: SystemError) : PlatformError =
        PlatformError(SystemErrorReason error)

    /// Create a `PlatformError` whose reason is a `BadArgument`. (`badArgument`)
    let badArgument (error: BadArgument) : PlatformError =
        PlatformError(BadArgumentReason error)
