/// @title Resources, scopes, and guaranteed finalization
///
/// A `Scope` collects finalizers and runs them when it closes — on success,
/// typed failure, defect, OR interruption. `Scope.acquireRelease` ties a
/// resource's release to the scope; `Effect.ensuring` attaches a one-off
/// finalizer; and because eff-sharp finalizers run under an uninterruptible mask,
/// cleanup cannot be skipped by a cancellation racing in.
///
/// Mirrors upstream `01_effect/04_resources`. ⭐ F# edge: the `effect { }` CE also
/// understands `use` for any `IDisposable`, so .NET resources drop straight in.
module EffSharp.AiDocs.Resources

open Effect
open EffSharp.AiDocs.Shared

// A toy resource with explicit open/close so we can watch finalization order.
type Conn = { Id: int }

let private openConn (id: int) : Effect<Conn, string, unit> =
    Effect.sync (fun () ->
        printfn "    open  conn #%d" id
        { Id = id })

let private closeConn (c: Conn) : Effect<unit, string, unit> =
    Effect.sync (fun () -> printfn "    close conn #%d" c.Id)

// ---------------------------------------------------------------------------
// Scoped acquire/release: the release is registered with the scope and runs when
// the scope closes — even if the body fails. Acquisitions release in LIFO order.
// ---------------------------------------------------------------------------

let scopedWork: Effect<int, string, unit> =
    Scope.scoped (fun scope ->
        effect {
            // `release` receives the resource and the scope's exit; here we ignore
            // the exit and just close. Two resources -> closed newest-first.
            let! a = Scope.acquireRelease scope (openConn 1) (fun c _exit -> closeConn c)
            let! b = Scope.acquireRelease scope (openConn 2) (fun c _exit -> closeConn c)
            printfn "    using conns #%d and #%d" a.Id b.Id
            return a.Id + b.Id
        })

// ---------------------------------------------------------------------------
// `ensuring`: attach a finalizer to a single effect, no scope needed.
// ---------------------------------------------------------------------------

let withEnsuring: Effect<int, string, unit> =
    Effect.sync (fun () ->
        printfn "    doing work"
        99)
    |> Effect.ensuring (Effect.sync (fun () -> printfn "    finalizer ran"))

// ---------------------------------------------------------------------------
// `use` in the CE — guaranteed Dispose() for any IDisposable.  ⭐ F# edge
// ---------------------------------------------------------------------------

type private Handle(name: string) =
    do printfn "    acquire %s" name

    interface System.IDisposable with
        member _.Dispose() = printfn "    dispose %s" name

let withUse: Effect<int, string, unit> =
    effect {
        use _h = new Handle("report.csv") // Dispose() runs when the CE block exits
        printfn "    writing report"
        return 7
    }

let demo () =
    banner "04 · Resources — scopes, ensuring, use"
    printfn "  scopedWork  => %d" (run scopedWork)
    printfn "  withEnsuring => %d" (run withEnsuring)
    printfn "  withUse      => %d" (run withUse)
