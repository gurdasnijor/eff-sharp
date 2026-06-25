namespace Effect

open System.Collections.Generic

/// `FiberMap` — a scoped, keyed set of fibers.
///
/// Port of repos/effect-smol/packages/effect/src/FiberMap.ts. Each key holds at
/// most one fiber; installing a fiber at a key interrupts the one it replaces.
/// Closing the owning scope (or `clear`) interrupts every member, and completed
/// fibers are dropped.
///
/// Decoupling / omissions (per CONVENTIONS), driven by the simplified fiber
/// kernel: the owning `Scope` is passed *explicitly* to `make`; interruption is
/// cooperative; completed fibers are pruned lazily on read (no `addObserver`);
/// `run`/`set` await the replaced fiber so callers observe its finalizers;
/// `join` awaits all members and surfaces the first typed failure. Dropped:
/// `runtime`/`*Promise`, `propagateInterruption`, the `deferred` field, `TypeId`.
/// A supervised, keyed set of fibers.
type FiberMap<'K, 'A, 'E> when 'K: equality =
    internal
        { Fibers: Dictionary<'K, Fiber<'A, 'E>>
          mutable Closed: bool
          Gate: obj }

[<RequireQualifiedAccess>]
module FiberMap =

    let private isLive (fiber: Fiber<'A, 'E>) : bool = not fiber.Task.IsCompleted

    /// Drop completed entries (caller holds the lock).
    let private prune (self: FiberMap<'K, 'A, 'E>) : unit =
        let dead =
            [ for kv in self.Fibers do
                  if not (isLive kv.Value) then
                      kv.Key ]

        for k in dead do
            self.Fibers.Remove k |> ignore

    let private interruptAll (fibers: Fiber<'A, 'E> list) : Effect<unit, 'E2, 'R> =
        Effect.forEach fibers Effect.interrupt |> Effect.map ignore

    /// Create an empty map outside the `Effect` context. (FiberMap.makeUnsafe)
    let makeUnsafe<'K, 'A, 'E when 'K: equality> () : FiberMap<'K, 'A, 'E> =
        { Fibers = Dictionary<'K, Fiber<'A, 'E>>()
          Closed = false
          Gate = System.Object() }

    let private closeEffect (self: FiberMap<'K, 'A, 'E>) : Effect<unit, 'E2, 'R> =
        Effect.suspend (fun () ->
            let live =
                lock self.Gate (fun () ->
                    self.Closed <- true
                    let snapshot = [ for kv in self.Fibers -> kv.Value ]
                    self.Fibers.Clear()
                    snapshot |> List.filter isLive)

            interruptAll live)

    /// Create a scoped map; all members are interrupted when `parent` closes.
    /// (FiberMap.make)
    let make (parent: Scope<'E2, 'R>) : Effect<FiberMap<'K, 'A, 'E>, 'E2, 'R> =
        Effect.suspend (fun () ->
            let self = makeUnsafe<'K, 'A, 'E> ()
            Scope.addFinalizer parent (closeEffect self) |> Effect.map (fun () -> self))

    /// Install `fiber` at `key`, flag-interrupting any fiber it replaces.
    /// (FiberMap.setUnsafe)
    let setUnsafe (self: FiberMap<'K, 'A, 'E>) (key: 'K) (fiber: Fiber<'A, 'E>) : unit =
        lock self.Gate (fun () ->
            if self.Closed then
                fiber.Runtime.Interrupted <- true
            else
                match self.Fibers.TryGetValue key with
                | true, old when isLive old -> old.Runtime.Interrupted <- true
                | _ -> ()

                self.Fibers.[key] <- fiber)

    /// Fork `effect` at `key`, interrupting (and awaiting) any replaced fiber.
    /// (FiberMap.run)
    let run (self: FiberMap<'K, 'A, 'E>) (key: 'K) (effect: Effect<'A, 'E, 'R>) : Effect<Fiber<'A, 'E>, 'E2, 'R> =
        Effect.fork effect
        |> Effect.flatMap (fun fiber ->
            let previous =
                lock self.Gate (fun () ->
                    if self.Closed then
                        Some fiber
                    else
                        match self.Fibers.TryGetValue key with
                        | true, old when isLive old ->
                            self.Fibers.[key] <- fiber
                            Some old
                        | _ ->
                            self.Fibers.[key] <- fiber
                            None)

            match previous with
            | None -> Effect.succeed fiber
            | Some toInterrupt -> Effect.interrupt toInterrupt |> Effect.map (fun () -> fiber))

    /// The live fiber at `key`, or `None`. (FiberMap.getUnsafe)
    let getUnsafe (self: FiberMap<'K, 'A, 'E>) (key: 'K) : Fiber<'A, 'E> option =
        lock self.Gate (fun () ->
            match self.Fibers.TryGetValue key with
            | true, f when not self.Closed && isLive f -> Some f
            | _ -> None)

    /// The live fiber at `key`, or `None`. (FiberMap.get)
    let get (self: FiberMap<'K, 'A, 'E>) (key: 'K) : Effect<Fiber<'A, 'E> option, 'E2, 'R> =
        Effect.sync (fun () -> getUnsafe self key)

    /// Whether `key` holds a live fiber. (FiberMap.has)
    let contains (self: FiberMap<'K, 'A, 'E>) (key: 'K) : Effect<bool, 'E2, 'R> =
        Effect.sync (fun () -> (getUnsafe self key).IsSome)

    /// Interrupt and remove the fiber at `key`. (FiberMap.remove)
    let remove (self: FiberMap<'K, 'A, 'E>) (key: 'K) : Effect<unit, 'E2, 'R> =
        Effect.suspend (fun () ->
            let cur =
                lock self.Gate (fun () ->
                    match self.Fibers.TryGetValue key with
                    | true, f ->
                        self.Fibers.Remove key |> ignore
                        Some f
                    | _ -> None)

            match cur with
            | Some f -> Effect.interrupt f
            | None -> Effect.succeed ())

    /// The number of live fibers. (FiberMap.size)
    let size (self: FiberMap<'K, 'A, 'E>) : Effect<int, 'E2, 'R> =
        Effect.sync (fun () ->
            lock self.Gate (fun () ->
                prune self
                self.Fibers.Count))

    /// Interrupt every member and empty the map. (FiberMap.clear)
    let clear (self: FiberMap<'K, 'A, 'E>) : Effect<unit, 'E2, 'R> =
        Effect.suspend (fun () ->
            let live =
                lock self.Gate (fun () ->
                    let snapshot = [ for kv in self.Fibers -> kv.Value ] |> List.filter isLive
                    self.Fibers.Clear()
                    snapshot)

            interruptAll live)

    /// Wait for all members and surface the first typed failure. (FiberMap.join)
    let join (self: FiberMap<'K, 'A, 'E>) : Effect<unit, 'E, 'R> =
        Effect.suspend (fun () ->
            // Await all members (including just-completed ones carrying failures).
            let all = lock self.Gate (fun () -> [ for kv in self.Fibers -> kv.Value ])

            Effect.forEach all Effect.await
            |> Effect.flatMap (fun exits ->
                let firstError =
                    exits
                    |> List.tryPick (fun exit ->
                        match exit with
                        | Failure cause -> Cause.failures cause |> List.tryHead
                        | Success _ -> None)

                match firstError with
                | Some e -> Effect.fail e
                | None -> Effect.succeed ()))
