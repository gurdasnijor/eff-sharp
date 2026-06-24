namespace Effect

/// Port of repos/effect-smol/packages/effect/src/TxSemaphore.ts — a transactional
/// semaphore whose available-permit count lives in a `TxRef<int>`, so permit
/// accounting commits atomically with other STM state. Blocking acquires
/// (`acquire`/`acquireN`) `retry` until enough permits are available; the
/// `tryAcquire*` variants never block.
///
/// Porting adaptations (per CONVENTIONS):
///   - Each public op is its own atomic transaction (`TxRef.atomically`), matching
///     our STM boundary (see TxRef). `capacity` is a plain `int` getter (upstream
///     wraps it in `Effect.succeed`).
///   - Bad-input guards (`n <= 0`, negative permits) surface as **defects** via
///     `Cause.die`, matching upstream's `Effect.die(new Error(...))`.
///   - JS-runtime machinery (`TypeId`, `Proto`, `toJSON`, `isTxSemaphore`,
///     `pipe`/`dual`) is dropped; ops take `self` first.
type TxSemaphore =
    internal
        { PermitsRef: TxRef<int>
          Capacity: int }

[<RequireQualifiedAccess>]
module TxSemaphore =

    /// Fail with a defect (Die), mirroring upstream `Effect.die(new Error(msg))`.
    let private die (msg: string) : Effect<'a, 'e, 'r> = Effect.failCause (Cause.die (box (System.Exception msg)))

    // --- constructors ---

    /// Create a semaphore with `permits` permits (also its capacity). A negative
    /// count dies with a defect. (TxSemaphore.make)
    let make (permits: int) : Effect<TxSemaphore, 'E, 'R> =
        if permits < 0 then
            die "Permits must be non-negative"
        else
            TxRef.make permits |> Effect.map (fun ref -> { PermitsRef = ref; Capacity = permits })

    // --- getters ---

    /// The current number of available permits. (TxSemaphore.available)
    let available (self: TxSemaphore) : Effect<int, 'E, 'R> = TxRef.atomically (TxRef.get self.PermitsRef)

    /// The fixed total permit count. (TxSemaphore.capacity)
    let capacity (self: TxSemaphore) : int = self.Capacity

    // --- blocking acquisition ---

    /// Acquire one permit, retrying until one is available. (TxSemaphore.acquire)
    let acquire (self: TxSemaphore) : Effect<unit, 'E, 'R> =
        TxRef.atomically (
            stm {
                let! permits = TxRef.get self.PermitsRef

                if permits <= 0 then
                    return! TxRef.retry
                else
                    do! TxRef.set self.PermitsRef (permits - 1)
            }
        )

    /// Acquire `n` permits, retrying until all are available. A non-positive `n`
    /// dies with a defect. (TxSemaphore.acquireN)
    let acquireN (self: TxSemaphore) (n: int) : Effect<unit, 'E, 'R> =
        if n <= 0 then
            die "Number of permits must be positive"
        else
            TxRef.atomically (
                stm {
                    let! permits = TxRef.get self.PermitsRef

                    if permits < n then
                        return! TxRef.retry
                    else
                        do! TxRef.set self.PermitsRef (permits - n)
                }
            )

    // --- non-blocking acquisition ---

    /// Try to acquire one permit; `true` on success, `false` if none available.
    /// (TxSemaphore.tryAcquire)
    let tryAcquire (self: TxSemaphore) : Effect<bool, 'E, 'R> =
        TxRef.atomically (
            TxRef.modify self.PermitsRef (fun permits ->
                if permits > 0 then (true, permits - 1) else (false, permits))
        )

    /// Try to acquire `n` permits; `true` on success, `false` if too few. A
    /// non-positive `n` dies with a defect. (TxSemaphore.tryAcquireN)
    let tryAcquireN (self: TxSemaphore) (n: int) : Effect<bool, 'E, 'R> =
        if n <= 0 then
            die "Number of permits must be positive"
        else
            TxRef.atomically (
                TxRef.modify self.PermitsRef (fun permits ->
                    if permits >= n then (true, permits - n) else (false, permits))
            )

    // --- release ---

    /// Release one permit, capped at capacity. (TxSemaphore.release)
    let release (self: TxSemaphore) : Effect<unit, 'E, 'R> =
        TxRef.atomically (
            TxRef.update self.PermitsRef (fun permits -> if permits >= self.Capacity then permits else permits + 1)
        )

    /// Release `n` permits, capped at capacity. A non-positive `n` dies with a
    /// defect. (TxSemaphore.releaseN)
    let releaseN (self: TxSemaphore) (n: int) : Effect<unit, 'E, 'R> =
        if n <= 0 then
            die "Number of permits must be positive"
        else
            TxRef.atomically (
                TxRef.update self.PermitsRef (fun permits -> min self.Capacity (permits + n))
            )

    // --- scoped / wrapped usage ---

    /// Run `effect` holding one permit, released afterwards even on failure or
    /// interruption. (TxSemaphore.withPermit)
    let withPermit (self: TxSemaphore) (effect: Effect<'A, 'E, 'R>) : Effect<'A, 'E, 'R> =
        Effect.acquireUseRelease (acquire self) (fun () -> effect) (fun () _ -> release self)

    /// Run `effect` holding `n` permits, released afterwards even on failure or
    /// interruption. (TxSemaphore.withPermits)
    let withPermits (self: TxSemaphore) (n: int) (effect: Effect<'A, 'E, 'R>) : Effect<'A, 'E, 'R> =
        Effect.acquireUseRelease (acquireN self n) (fun () -> effect) (fun () _ -> releaseN self n)

    /// Acquire one permit for the lifetime of `scope`, released when it closes.
    /// (TxSemaphore.withPermitScoped — our `Scope` is passed explicitly rather
    /// than required from the environment.)
    let withPermitScoped (self: TxSemaphore) (scope: Scope<'E, 'R>) : Effect<unit, 'E, 'R> =
        Scope.acquireRelease scope (acquire self) (fun () _ -> release self)
