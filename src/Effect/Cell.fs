namespace Effect

/// One-shot completion cell — the Fable-portable replacement for
/// `System.Threading.Tasks.TaskCompletionSource`.
///
/// Why this exists: the `Effect` core is already a reader into `Async<Exit>`, so
/// "suspend a fiber until X happens" only needs an `Async<'T>` that resolves
/// later. `TaskCompletionSource` provided that on .NET, but Fable's runtime
/// implements neither `TaskCompletionSource` nor `Async.AwaitTask`. A `Cell`
/// rebuilds the same one-shot, first-writer-wins, multi-waiter semantics on
/// `Async.FromContinuations` (which Fable *does* implement), so a single
/// implementation compiles on both targets with no `#if`.
///
/// Thread-safety: `lock` guards the check-and-register (`await`) against the
/// complete-and-drain (`trySet`). On .NET that lock is real; under Fable it is a
/// verified no-op and single-threaded JS makes the critical sections free. This
/// is the keystone primitive behind `Deferred`, `Semaphore`, and `PubSub`.
type internal Cell<'T> =
    { mutable Value: 'T option
      Waiters: ResizeArray<'T -> unit> }

[<RequireQualifiedAccess>]
module internal Cell =

    /// Allocate an empty cell.
    let make () : Cell<'T> =
        { Value = None
          Waiters = ResizeArray() }

    /// Whether the cell has been completed.
    let isDone (c: Cell<'T>) : bool = Option.isSome c.Value

    /// The stored value, if completed. (Backs `Deferred.poll`.)
    let value (c: Cell<'T>) : 'T option = c.Value

    /// Resume a waiter *asynchronously* — never inline on the completer's own
    /// stack. This reproduces `TaskCreationOptions.RunContinuationsAsynchronously`:
    /// a completer must not run an awaiter's continuation reentrantly, or
    /// Semaphore/PubSub fairness breaks. `Async.Start` schedules onto the thread
    /// pool (.NET) / the microtask-style async scheduler (Fable).
    let private resume (ok: 'T -> unit) (v: 'T) : unit = Async.Start(async { ok v })

    /// Attempt to complete the cell. First writer wins: returns `true` iff this
    /// call filled it, `false` if it was already completed. Drains all waiters.
    let trySet (c: Cell<'T>) (v: 'T) : bool =
        lock c (fun () ->
            match c.Value with
            | Some _ -> false
            | None ->
                c.Value <- Some v
                let pending = c.Waiters.ToArray()
                c.Waiters.Clear()

                for ok in pending do
                    resume ok v

                true)

    /// Suspend until the cell is completed. If already complete, resolves with the
    /// stored value on the caller's own stack (no thread hop needed — there is no
    /// completer to protect).
    let await (c: Cell<'T>) : Async<'T> =
        Async.FromContinuations(fun (ok, _, _) ->
            lock c (fun () ->
                match c.Value with
                | Some v -> ok v
                | None -> c.Waiters.Add ok))
