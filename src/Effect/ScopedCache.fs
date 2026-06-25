namespace Effect

/// `ScopedCache` — keyed memoization whose entries own scoped resources.
///
/// Port of repos/effect-smol/packages/effect/src/ScopedCache.ts. Like `Cache`,
/// but each entry owns a `Scope`: resources acquired while building a value stay
/// alive while the entry is cached and are **released when the entry is removed**
/// (invalidated, refreshed, or evicted by capacity) or when the whole cache is
/// invalidated. Built on the merged `Deferred`, `MutableHashMap`, `Clock`,
/// `Scope`, and `Exit`.
///
/// Porting notes (see CONVENTIONS.md):
///   * F#'s `Scope` is an explicit value (no `Scope.Scope` env service /
///     `Scope.provide`), so the lookup receives its entry scope as an explicit
///     argument: `lookup : 'Key -> Scope<'E,Context> -> Effect<'A,'E,Context>`.
///     Resources are registered on that scope with `Scope.addFinalizer`/
///     `Scope.acquireRelease`. The upstream `R | Scope.Scope` requirement and the
///     auto-close on an outer env scope are modelled by the explicit
///     `invalidateAll` (closes every entry scope); the env is fixed to `Context`.
///   * Scope release is driven on the **effectful** paths — `invalidate`,
///     `invalidateAll`, `refresh`, and capacity eviction (within `get`/`set`) all
///     `Scope.close` the affected entries. Expiry pruning inside the synchronous
///     `has`/`keys`/`entries` reads only drops the map entry (its scope is closed
///     on the next `invalidate*`/eviction); upstream forks the close — noted.
///   * `refresh` is `invalidate` (closing the old scope) followed by `get`
///     (fresh scope + lookup); the upstream in-flight-sharing nuance is dropped.
///   * `capacity` is an `int`; JS-runtime machinery (`TypeId`/`Pipeable`/`toJSON`)
///     is dropped.
/// A scoped cache entry: the shared `Deferred`, its resolved `Exit`, the optional
/// expiry timestamp, and the per-entry `Scope` released when the entry is removed.
type ScopedCacheEntry<'A, 'E> =
    { mutable ExpiresAt: int64 option
      Deferred: Deferred<'A, 'E>
      mutable Result: Exit<'A, 'E> option
      Scope: Scope<'E, Context> }

/// A keyed cache whose lookups acquire scoped resources.
type ScopedCache<'Key, 'A, 'E when 'Key: equality> =
    internal
        { Map: MutableHashMap<'Key, ScopedCacheEntry<'A, 'E>>
          Capacity: int
          Lookup: 'Key -> Scope<'E, Context> -> Effect<'A, 'E, Context>
          TimeToLive: Exit<'A, 'E> -> 'Key -> Duration }

[<RequireQualifiedAccess>]
module ScopedCache =

    // --- internal helpers ---

    let private ofExit (exit: Exit<'A, 'E>) : Effect<'A, 'E, unit> =
        match exit with
        | Success a -> Effect.succeed a
        | Failure c -> Effect.failCause c

    let private onExit (f: Exit<'A, 'E> -> Effect<unit, 'E, 'R>) (Effect run) : Effect<'A, 'E, 'R> =
        Effect(fun fib r ->
            async {
                let! exit = run fib r
                let (Effect runFin) = f exit
                let! _ = runFin fib r
                return exit
            })

    let private hasExpired (entry: ScopedCacheEntry<'A, 'E>) (now: int64) : bool =
        match entry.ExpiresAt with
        | None -> false
        | Some t -> now >= t

    let private expiryFromTtl (now: int64) (ttl: Duration) : int64 option =
        if Duration.isFinite ttl then
            Some(now + int64 (Duration.toMillis ttl))
        else
            None

    /// Close every supplied entry's scope, in order. (Scope.close per entry)
    let private closeAll (entries: ScopedCacheEntry<'A, 'E> list) : Effect<unit, 'E, Context> =
        entries
        |> List.fold
            (fun acc entry -> acc |> Effect.flatMap (fun () -> Scope.close entry.Scope (Success(box ()))))
            (Effect.succeed ())

    /// Evict the oldest entries beyond capacity, returning the evicted ones so the
    /// caller can close their scopes. (ScopedCache checkCapacity)
    let private checkCapacity (self: ScopedCache<'Key, 'A, 'E>) : ScopedCacheEntry<'A, 'E> list =
        let diff = MutableHashMap.size self.Map - self.Capacity

        if diff > 0 then
            let oldest = MutableHashMap.toSeq self.Map |> Seq.truncate diff |> Seq.toList

            for (k, _) in oldest do
                MutableHashMap.remove self.Map k |> ignore

            oldest |> List.map snd
        else
            []

    let private getImpl
        (self: ScopedCache<'Key, 'A, 'E>)
        (key: 'Key)
        (now: int64)
        (isRead: bool)
        : ScopedCacheEntry<'A, 'E> option =
        match MutableHashMap.get self.Map key with
        | None -> None
        | Some entry ->
            if hasExpired entry now then
                MutableHashMap.remove self.Map key |> ignore
                None
            elif isRead then
                MutableHashMap.remove self.Map key |> ignore
                MutableHashMap.set self.Map key entry |> ignore
                Some entry
            else
                Some entry

    // --- constructors ---

    let private defaultTimeToLive: Exit<'A, 'E> -> 'Key -> Duration =
        fun _ _ -> Duration.infinity

    /// Creates a scoped cache with dynamic time-to-live. (ScopedCache.makeWith)
    let makeWith
        (capacity: int)
        (timeToLive: Exit<'A, 'E> -> 'Key -> Duration)
        (lookup: 'Key -> Scope<'E, Context> -> Effect<'A, 'E, Context>)
        : Effect<ScopedCache<'Key, 'A, 'E>, 'F, 'R> =
        Effect.sync (fun () ->
            { Map = MutableHashMap.empty ()
              Capacity = capacity
              Lookup = lookup
              TimeToLive = timeToLive })

    /// Creates a scoped cache whose entries never expire. (ScopedCache.make)
    let make
        (capacity: int)
        (lookup: 'Key -> Scope<'E, Context> -> Effect<'A, 'E, Context>)
        : Effect<ScopedCache<'Key, 'A, 'E>, 'F, 'R> =
        makeWith capacity defaultTimeToLive lookup

    /// Creates a scoped cache with a fixed time-to-live. (ScopedCache.make, TTL)
    let makeWithTtl
        (capacity: int)
        (ttl: Duration)
        (lookup: 'Key -> Scope<'E, Context> -> Effect<'A, 'E, Context>)
        : Effect<ScopedCache<'Key, 'A, 'E>, 'F, 'R> =
        makeWith capacity (fun _ _ -> ttl) lookup

    // --- reads ---

    /// Retrieves the value for `key`, running `lookup` (in a fresh entry scope) on
    /// a miss or expired entry. Concurrent misses share one lookup. (ScopedCache.get)
    let get (self: ScopedCache<'Key, 'A, 'E>) (key: 'Key) : Effect<'A, 'E, Context> =
        Clock.clockWith (fun clock ->
            Effect.suspend (fun () ->
                let now = clock.CurrentTimeMillisUnsafe()

                match MutableHashMap.get self.Map key with
                | Some entry when not (hasExpired entry now) ->
                    MutableHashMap.remove self.Map key |> ignore
                    MutableHashMap.set self.Map key entry |> ignore
                    Deferred.await entry.Deferred
                | _ ->
                    let scope = Scope.make ()
                    let deferred = Deferred.makeUnsafe ()

                    let entry =
                        { ExpiresAt = None
                          Deferred = deferred
                          Result = None
                          Scope = scope }

                    MutableHashMap.set self.Map key entry |> ignore
                    let evicted = checkCapacity self

                    closeAll evicted
                    |> Effect.flatMap (fun () ->
                        self.Lookup key scope
                        |> onExit (fun exit ->
                            Effect.sync (fun () ->
                                Deferred.doneUnsafe deferred (ofExit exit) |> ignore
                                entry.Result <- Some exit
                                let ttl = self.TimeToLive exit key

                                if Duration.isFinite ttl then
                                    entry.ExpiresAt <- expiryFromTtl (clock.CurrentTimeMillisUnsafe()) ttl
                                elif Duration.isZero ttl then
                                    MutableHashMap.remove self.Map key |> ignore)))))

    /// Reads an existing entry without invoking `lookup`. (ScopedCache.getOption)
    let getOption (self: ScopedCache<'Key, 'A, 'E>) (key: 'Key) : Effect<'A option, 'E, Context> =
        Clock.clockWith (fun clock ->
            Effect.suspend (fun () ->
                let now = clock.CurrentTimeMillisUnsafe()

                match getImpl self key now true with
                | Some entry -> Deferred.await entry.Deferred |> Effect.map Some
                | None -> Effect.succeed None))

    /// Returns the value only if a non-expired entry has already resolved
    /// successfully. (ScopedCache.getSuccess)
    let getSuccess (self: ScopedCache<'Key, 'A, 'E>) (key: 'Key) : Effect<'A option, 'F, Context> =
        Clock.clockWith (fun clock ->
            Effect.sync (fun () ->
                let now = clock.CurrentTimeMillisUnsafe()

                match getImpl self key now false with
                | Some entry ->
                    match entry.Result with
                    | Some(Success a) -> Some a
                    | _ -> None
                | None -> None))

    // --- inspection ---

    /// Whether a non-expired entry exists. (ScopedCache.has)
    let has (self: ScopedCache<'Key, 'A, 'E>) (key: 'Key) : Effect<bool, 'F, Context> =
        Clock.clockWith (fun clock ->
            Effect.sync (fun () ->
                let now = clock.CurrentTimeMillisUnsafe()
                (getImpl self key now false).IsSome))

    /// The number of stored entries. (ScopedCache.size)
    let size (self: ScopedCache<'Key, 'A, 'E>) : Effect<int, 'F, Context> =
        Effect.sync (fun () -> MutableHashMap.size self.Map)

    /// All active (non-expired) keys; expired entries are pruned. (ScopedCache.keys)
    let keys (self: ScopedCache<'Key, 'A, 'E>) : Effect<'Key list, 'F, Context> =
        Clock.clockWith (fun clock ->
            Effect.sync (fun () ->
                let now = clock.CurrentTimeMillisUnsafe()
                let all = MutableHashMap.toSeq self.Map |> Seq.toList

                for (k, e) in all do
                    if hasExpired e now then
                        MutableHashMap.remove self.Map k |> ignore

                all |> List.choose (fun (k, e) -> if hasExpired e now then None else Some k)))

    // --- invalidation (closes entry scopes) ---

    /// Removes the entry for `key` and closes its scope. (ScopedCache.invalidate)
    let invalidate (self: ScopedCache<'Key, 'A, 'E>) (key: 'Key) : Effect<unit, 'E, Context> =
        Effect.suspend (fun () ->
            match MutableHashMap.get self.Map key with
            | Some entry ->
                MutableHashMap.remove self.Map key |> ignore
                Scope.close entry.Scope (Success(box ()))
            | None -> Effect.succeed ())

    /// Removes all entries and closes every entry scope. (ScopedCache.invalidateAll)
    let invalidateAll (self: ScopedCache<'Key, 'A, 'E>) : Effect<unit, 'E, Context> =
        Effect.suspend (fun () ->
            let all = MutableHashMap.toSeq self.Map |> Seq.map snd |> Seq.toList
            MutableHashMap.clear self.Map |> ignore
            closeAll all)

    /// Forces a refresh: invalidates `key` (closing its old scope) then re-runs
    /// the lookup in a fresh scope, returning the new value. (ScopedCache.refresh)
    let refresh (self: ScopedCache<'Key, 'A, 'E>) (key: 'Key) : Effect<'A, 'E, Context> =
        invalidate self key |> Effect.flatMap (fun () -> get self key)
