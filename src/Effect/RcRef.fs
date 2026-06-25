namespace Effect

/// `RcRef` — a reference-counted handle sharing one scoped resource across many
/// borrowing scopes.
///
/// Port of repos/effect-smol/packages/effect/src/RcRef.ts (internal/rcRef.ts).
/// The resource is acquired lazily on the first `get`, reused while at least one
/// borrower holds it, and released when the last borrower's scope closes. Each
/// `get` increments the reference count and registers a release finalizer on the
/// borrower's scope. When the owning parent scope closes, the `RcRef` is closed:
/// any cached resource is released and further `get`s fail (interrupted).
///
/// Decoupling (per CONVENTIONS): upstream threads `Scope` as an *environment*
/// requirement (`Scope.provide`). The ported `Scope` is passed *explicitly*, so
/// `make` takes the owning parent `Scope` and `get` takes the borrower `Scope`.
/// State transitions are serialized through a `SynchronizedRef` permit.
/// Omissions: `idleTimeToLive` (the idle-release timer) is not ported — release
/// is immediate when the count reaches zero; `invalidate` is best-effort for a
/// non-zero reference count (noted at the function). The closed-`RcRef` `get`
/// failure is modelled as an interrupt cause, matching upstream's interrupted
/// exit.

type internal RcState<'A, 'E, 'R> =
    { Count: int
      Cached: ('A * Scope<'E, 'R>) option
      Closed: bool }

/// A reference-counted, lazily-acquired shared resource.
type RcRef<'A, 'E, 'R> =
    internal
        { State: SynchronizedRef<RcState<'A, 'E, 'R>>
          Acquire: Scope<'E, 'R> -> Effect<'A, 'E, 'R> }

[<RequireQualifiedAccess>]
module RcRef =

    let private voidExit: Exit<obj, obj> = Success(box ())

    /// Create an `RcRef` owned by `parent`: acquiring `acquire` lazily on first
    /// `get`, and closing the cached resource when `parent` closes. (RcRef.make)
    let make
        (parent: Scope<'E, 'R>)
        (acquire: Scope<'E, 'R> -> Effect<'A, 'E, 'R>)
        : Effect<RcRef<'A, 'E, 'R>, 'E, 'R> =
        Effect.suspend (fun () ->
            let self =
                { State =
                    SynchronizedRef.makeUnsafe
                        { Count = 0
                          Cached = None
                          Closed = false }
                  Acquire = acquire }

            // On owner close: release any cached resource and mark closed.
            let onClose =
                SynchronizedRef.modifyEffect self.State (fun st ->
                    match st.Cached with
                    | Some(_, scope) ->
                        Scope.close scope voidExit
                        |> Effect.map (fun () -> ((), { st with Cached = None; Closed = true }))
                    | None -> Effect.succeed ((), { st with Closed = true }))

            Scope.addFinalizer parent onClose |> Effect.map (fun () -> self))

    /// Borrow the shared resource for `borrower`, acquiring it if needed. The
    /// reference is released when `borrower` closes; the resource is released
    /// when the last reference drops. Fails (interrupted) if the `RcRef` is
    /// closed. (RcRef.get)
    let get (self: RcRef<'A, 'E, 'R>) (borrower: Scope<'E, 'R>) : Effect<'A, 'E, 'R> =
        SynchronizedRef.modifyEffect self.State (fun st ->
            if st.Closed then
                Effect.failCause (Cause.interrupt None)
            else
                match st.Cached with
                | Some(value, _) -> Effect.succeed (value, { st with Count = st.Count + 1 })
                | None ->
                    let resScope = Scope.make<'E, 'R> ()

                    self.Acquire resScope
                    |> Effect.map (fun value ->
                        (value,
                         { st with
                             Count = st.Count + 1
                             Cached = Some(value, resScope) })))
        |> Effect.flatMap (fun value ->
            // Register the release for this borrowed reference.
            let release =
                SynchronizedRef.modifyEffect self.State (fun st ->
                    let newCount = st.Count - 1

                    if newCount <= 0 then
                        match st.Cached with
                        | Some(_, scope) ->
                            Scope.close scope voidExit
                            |> Effect.map (fun () -> ((), { st with Count = 0; Cached = None }))
                        | None -> Effect.succeed ((), { st with Count = 0 })
                    else
                        Effect.succeed ((), { st with Count = newCount }))

            Scope.addFinalizer borrower release |> Effect.map (fun () -> value))

    /// Invalidate the cached resource so the next `get` acquires a fresh one.
    /// Releases immediately when no references are outstanding; with active
    /// references it only clears the cache pointer (best-effort, see doc).
    /// (RcRef.invalidate)
    let invalidate (self: RcRef<'A, 'E, 'R>) : Effect<unit, 'E, 'R> =
        SynchronizedRef.modifyEffect self.State (fun st ->
            match st.Cached with
            | Some(_, scope) when st.Count = 0 ->
                Scope.close scope voidExit
                |> Effect.map (fun () -> ((), { st with Cached = None }))
            | _ -> Effect.succeed ((), st))
