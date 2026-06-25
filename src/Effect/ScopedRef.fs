namespace Effect

/// `ScopedRef` — a value paired with the `Scope` that owns its resources.
///
/// Port of repos/effect-smol/packages/effect/src/ScopedRef.ts. Replacing the
/// value (`set`) acquires the replacement in a fresh scope and closes the scope
/// that owned the previous value, so resources are released as the value is
/// swapped. The current value's scope is also closed when the owning parent
/// scope closes. Replacements are serialized through the backing
/// `SynchronizedRef`'s permit.
///
/// Decoupling (per CONVENTIONS): upstream threads `Scope` as an *environment*
/// requirement (`Scope.provide`, `Effect.addFinalizer`). The ported `Scope` is
/// passed *explicitly* (matching `Scope.fs`/`SynchronizedRef.fs`), so `make`/
/// `fromAcquire` take the owning parent `Scope`, and the acquire callback
/// receives the value's fresh `Scope` to register its release on. The
/// `tapCause`-on-acquire-failure cleanup and `idleTimeToLive` are omitted (no
/// failing acquisitions in scope here); noted.
/// A reference whose value owns a `Scope`'s resources.
type ScopedRef<'A, 'E, 'R> =
    internal
        { Backing: SynchronizedRef<Scope<'E, 'R> * 'A> }

[<RequireQualifiedAccess>]
module ScopedRef =

    let private voidExit: Exit<obj, obj> = Success(box ())

    /// Close the scope that currently owns the value (read at run time).
    let private closeCurrent (self: ScopedRef<'A, 'E, 'R>) : Effect<unit, 'E, 'R> =
        Effect.suspend (fun () ->
            let (scope, _) = SynchronizedRef.getUnsafe self.Backing
            Scope.close scope voidExit)

    /// Reads the current value synchronously. (ScopedRef.getUnsafe)
    let getUnsafe (self: ScopedRef<'A, 'E, 'R>) : 'A =
        snd (SynchronizedRef.getUnsafe self.Backing)

    /// Reads the current value. (ScopedRef.get)
    let get (self: ScopedRef<'A, 'E, 'R>) : Effect<'A, 'E, 'R> = Effect.sync (fun () -> getUnsafe self)

    /// Create a `ScopedRef` from a pure initial value; its scope closes when
    /// `parent` closes. (ScopedRef.make)
    let make (parent: Scope<'E, 'R>) (evaluate: unit -> 'A) : Effect<ScopedRef<'A, 'E, 'R>, 'E, 'R> =
        Effect.suspend (fun () ->
            let scope = Scope.make<'E, 'R> ()
            let self = { Backing = SynchronizedRef.makeUnsafe (scope, evaluate ()) }

            Scope.addFinalizer parent (closeCurrent self) |> Effect.map (fun () -> self))

    /// Create a `ScopedRef` from an effect that acquires the initial value into a
    /// fresh scope. (ScopedRef.fromAcquire)
    let fromAcquire
        (parent: Scope<'E, 'R>)
        (acquire: Scope<'E, 'R> -> Effect<'A, 'E, 'R>)
        : Effect<ScopedRef<'A, 'E, 'R>, 'E, 'R> =
        Effect.suspend (fun () ->
            let scope = Scope.make<'E, 'R> ()

            acquire scope
            |> Effect.flatMap (fun value ->
                let self = { Backing = SynchronizedRef.makeUnsafe (scope, value) }
                Scope.addFinalizer parent (closeCurrent self) |> Effect.map (fun () -> self)))

    /// Replace the value with a newly acquired one, releasing the previous
    /// value's resources. Serialized through the backing permit. (ScopedRef.set)
    let set (self: ScopedRef<'A, 'E, 'R>) (acquire: Scope<'E, 'R> -> Effect<'A, 'E, 'R>) : Effect<unit, 'E, 'R> =
        SynchronizedRef.modifyEffect self.Backing (fun (oldScope, _) ->
            Scope.close oldScope voidExit
            |> Effect.flatMap (fun () ->
                let newScope = Scope.make<'E, 'R> ()
                acquire newScope |> Effect.map (fun value -> ((), (newScope, value)))))
