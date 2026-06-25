namespace Effect

/// Wave 2: `Latch` — a reusable open/close synchronization gate.
///
/// Port of repos/effect-smol/packages/effect/src/Latch.ts (and the `Latch` class
/// in internal/effect.ts). A `Latch` is either open or closed. While closed,
/// `await`/`whenOpen` suspend; `open` opens it and releases current *and* future
/// waiters; `release` wakes only the currently-suspended waiters but leaves the
/// latch closed (future waiters suspend again); `close` re-arms a previously open
/// latch.
///
/// Representation — a `bool` state plus a "gate" `Cell<unit>` that current waiters
/// await. Opening or releasing completes the live gate (waking its waiters);
/// closing or releasing installs a *fresh* incomplete gate so subsequent waiters
/// suspend again. A lock guards the tiny state transitions so the primitive is
/// safe under the threadpool the async core may run on (upstream relies on JS's
/// single thread instead). Per the brief, `Latch` only depends on `Effect` (and
/// the `Cell` completion primitive).
///
/// Omissions vs upstream: the cooperative scheduler hop (`scheduleUnsafe` /
/// `currentDispatcher.scheduleTask`) is replaced by `Cell` continuations
/// (configured to resume asynchronously), which deliver the same "waiters resume
/// after the opener returns" behaviour without a fiber runtime. `Cell` replaces
/// `TaskCompletionSource`, which Fable's runtime does not implement.
type Latch =
    internal
        { mutable State: bool
          mutable Gate: Cell<unit>
          Lock: obj }

[<RequireQualifiedAccess>]
module Latch =

    let private newGate () : Cell<unit> = Cell.make ()

    // --- constructors ---

    /// Allocate a `Latch` synchronously. Starts closed unless `isOpen` is true.
    /// (Latch.makeUnsafe)
    let makeUnsafe (isOpen: bool) : Latch =
        let gate = newGate ()

        if isOpen then
            Cell.trySet gate () |> ignore

        { State = isOpen
          Gate = gate
          Lock = obj () }

    /// Allocate a `Latch` inside an `Effect`. Starts closed unless `isOpen` is
    /// true. (Latch.make)
    let make (isOpen: bool) : Effect<Latch, 'E, 'R> =
        Effect.sync (fun () -> makeUnsafe isOpen)

    // --- synchronous state transitions ---

    /// Whether the latch is currently open. (Latch.isOpen)
    let isOpen (self: Latch) : bool = lock self.Lock (fun () -> self.State)

    /// Open synchronously, releasing current and future waiters. Returns `true`
    /// iff this changed the latch from closed to open. (Latch.openUnsafe)
    let openUnsafe (self: Latch) : bool =
        lock self.Lock (fun () ->
            match self.State with
            | true -> false
            | false ->
                self.State <- true
                Cell.trySet self.Gate () |> ignore
                true)

    /// Close synchronously so future waiters suspend again. Returns `true` iff
    /// this changed the latch from open to closed. (Latch.closeUnsafe)
    let closeUnsafe (self: Latch) : bool =
        lock self.Lock (fun () ->
            match self.State with
            | false -> false
            | true ->
                self.State <- false
                self.Gate <- newGate ()
                true)

    // --- effectful combinators ---

    /// Open the latch, releasing current and future waiters. Succeeds with `true`
    /// iff this changed it from closed to open. (Latch.open)
    let ``open`` (self: Latch) : Effect<bool, 'E, 'R> = Effect.sync (fun () -> openUnsafe self)

    /// Close the latch so future waiters suspend again. Succeeds with `true` iff
    /// this changed it from open to closed. (Latch.close)
    let close (self: Latch) : Effect<bool, 'E, 'R> =
        Effect.sync (fun () -> closeUnsafe self)

    /// Release the currently-suspended waiters **without** opening the latch;
    /// future waiters still suspend. Succeeds with `true` iff the latch was
    /// closed (so a release was actually requested), else `false`. (Latch.release)
    let release (self: Latch) : Effect<bool, 'E, 'R> =
        Effect.sync (fun () ->
            lock self.Lock (fun () ->
                match self.State with
                | true -> false
                | false ->
                    let waiting = self.Gate
                    self.Gate <- newGate ()
                    Cell.trySet waiting () |> ignore
                    true))

    /// Wait for the latch to be opened or the current waiters to be released. An
    /// open latch completes immediately. (Latch.await)
    let await (self: Latch) : Effect<unit, 'E, 'R> =
        Effect(fun _ _ ->
            async {
                let wait = lock self.Lock (fun () -> if self.State then None else Some self.Gate)

                match wait with
                | None -> ()
                | Some gate -> do! Cell.await gate

                return Success()
            })

    /// Wait on the latch, then run `effect`. Open latches run it immediately;
    /// closed latches gate it until opened or released. (Latch.whenOpen)
    let whenOpen (self: Latch) (effect: Effect<'A, 'E, 'R>) : Effect<'A, 'E, 'R> =
        await self |> Effect.flatMap (fun () -> effect)
