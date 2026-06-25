namespace Effect

/// `Cache` — keyed, effectful memoization with TTL and capacity.
///
/// Port of repos/effect-smol/packages/effect/src/Cache.ts. A `Cache` stores the
/// `Exit` of a `lookup` effect per key: concurrent misses for the same key share
/// one in-flight lookup (via `Deferred`), failures are cached like successes,
/// entries expire by time-to-live, and the oldest entries are evicted once the
/// `capacity` is exceeded. Built on the merged `Deferred`, `MutableHashMap`,
/// `Clock`, and `Exit`.
///
/// Porting notes (see CONVENTIONS.md):
///   * The environment is fixed to `Context` (the `Clock`-bearing env): the
///     `lookup` is `'Key -> Effect<'A,'E,Context>` and every operation is
///     `Effect<_,_,Context>`. Upstream's per-call `R`, `requireServicesAt`
///     service-mode and the lookup context-merge are not modelled.
///   * `Entry` keeps the resolved `Exit` directly (a `mutable` field set when the
///     lookup completes) so `getSuccess`/`entries`/`values` can read a succeeded
///     value synchronously — upstream reads it back off the `Deferred`'s stored
///     effect, which the F# `Deferred` does not expose.
///   * `capacity` is an `int` (always finite); the `Number.isFinite` guard is
///     dropped. LRU recency uses the upstream remove+set trick; with a .NET
///     `Dictionary` backing, eviction order after deletions is best-effort
///     (insertion order is not guaranteed once slots are freed) — noted.
///   * JS-runtime machinery (`TypeId`, `Pipeable`, `toJSON`) is dropped.
/// A low-level cache entry: the shared `Deferred` lookup result, its resolved
/// `Exit` (once complete), and an optional expiry timestamp (epoch millis).
type CacheEntry<'A, 'E> =
    { mutable ExpiresAt: int64 option
      Deferred: Deferred<'A, 'E>
      mutable Result: Exit<'A, 'E> option }

/// A keyed cache over an effectful `lookup`.
type Cache<'Key, 'A, 'E when 'Key: equality> =
    internal
        { Map: MutableHashMap<'Key, CacheEntry<'A, 'E>>
          Capacity: int
          Lookup: 'Key -> Effect<'A, 'E, Context>
          TimeToLive: Exit<'A, 'E> -> 'Key -> Duration }

[<RequireQualifiedAccess>]
module Cache =

    // --- internal helpers ---

    let private ofExit (exit: Exit<'A, 'E>) : Effect<'A, 'E, unit> =
        match exit with
        | Success a -> Effect.succeed a
        | Failure c -> Effect.failCause c

    /// Run `f` with the effect's `Exit` after it settles, then return that `Exit`.
    /// (effect.onExit — local; FiberRuntime threading preserved.)
    let private onExit (f: Exit<'A, 'E> -> Effect<unit, 'E, 'R>) (Effect run) : Effect<'A, 'E, 'R> =
        Effect(fun fib r ->
            async {
                let! exit = run fib r
                let (Effect runFin) = f exit
                let! _ = runFin fib r
                return exit
            })

    let private catchCause (f: Cause<'E> -> Effect<'A, 'E2, 'R>) (Effect run) : Effect<'A, 'E2, 'R> =
        Effect(fun fib r ->
            async {
                let! exit = run fib r

                match exit with
                | Success a -> return Success a
                | Failure c ->
                    let (Effect run2) = f c
                    return! run2 fib r
            })

    let private hasExpired (entry: CacheEntry<'A, 'E>) (now: int64) : bool =
        match entry.ExpiresAt with
        | None -> false
        | Some t -> now >= t

    let private checkCapacity (self: Cache<'Key, 'A, 'E>) : unit =
        let diff = MutableHashMap.size self.Map - self.Capacity

        if diff > 0 then
            // MutableHashMap preserves insertion order, so the front entries are oldest.
            let oldest =
                MutableHashMap.toSeq self.Map |> Seq.truncate diff |> Seq.map fst |> Seq.toList

            for k in oldest do
                MutableHashMap.remove self.Map k |> ignore

    /// Look up a live (non-expired) entry, removing it if expired. `isRead` bumps
    /// LRU recency. (Cache.getImpl)
    let private getImpl
        (self: Cache<'Key, 'A, 'E>)
        (key: 'Key)
        (now: int64)
        (isRead: bool)
        : CacheEntry<'A, 'E> option =
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

    let private expiryFromTtl (now: int64) (ttl: Duration) : int64 option =
        if Duration.isFinite ttl then
            Some(now + int64 (Duration.toMillis ttl))
        else
            None

    // --- constructors ---

    let private defaultTimeToLive: Exit<'A, 'E> -> 'Key -> Duration =
        fun _ _ -> Duration.infinity

    /// Creates a cache with dynamic time-to-live computed from the lookup `Exit`
    /// and key. (Cache.makeWith)
    let makeWith
        (capacity: int)
        (timeToLive: Exit<'A, 'E> -> 'Key -> Duration)
        (lookup: 'Key -> Effect<'A, 'E, Context>)
        : Effect<Cache<'Key, 'A, 'E>, 'F, 'R> =
        Effect.sync (fun () ->
            { Map = MutableHashMap.empty ()
              Capacity = capacity
              Lookup = lookup
              TimeToLive = timeToLive })

    /// Creates a cache whose entries never expire. (Cache.make, infinite TTL)
    let make (capacity: int) (lookup: 'Key -> Effect<'A, 'E, Context>) : Effect<Cache<'Key, 'A, 'E>, 'F, 'R> =
        makeWith capacity defaultTimeToLive lookup

    /// Creates a cache with a fixed time-to-live for every entry. (Cache.make, TTL)
    let makeWithTtl
        (capacity: int)
        (ttl: Duration)
        (lookup: 'Key -> Effect<'A, 'E, Context>)
        : Effect<Cache<'Key, 'A, 'E>, 'F, 'R> =
        makeWith capacity (fun _ _ -> ttl) lookup

    // --- reads that may trigger a lookup ---

    /// Retrieves the value for `key`, invoking `lookup` on a miss or expired
    /// entry. Concurrent misses for the same key share one lookup; failures are
    /// cached. (Cache.get)
    let get (self: Cache<'Key, 'A, 'E>) (key: 'Key) : Effect<'A, 'E, Context> =
        Clock.clockWith (fun clock ->
            Effect.suspend (fun () ->
                let now = clock.CurrentTimeMillisUnsafe()

                match MutableHashMap.get self.Map key with
                | Some entry when not (hasExpired entry now) ->
                    MutableHashMap.remove self.Map key |> ignore
                    MutableHashMap.set self.Map key entry |> ignore
                    Deferred.await entry.Deferred
                | _ ->
                    let deferred = Deferred.makeUnsafe ()

                    let entry =
                        { ExpiresAt = None
                          Deferred = deferred
                          Result = None }

                    MutableHashMap.set self.Map key entry |> ignore
                    checkCapacity self

                    self.Lookup key
                    |> onExit (fun exit ->
                        Effect.sync (fun () ->
                            Deferred.doneUnsafe deferred (ofExit exit) |> ignore
                            entry.Result <- Some exit
                            let ttl = self.TimeToLive exit key

                            if Duration.isFinite ttl then
                                entry.ExpiresAt <- expiryFromTtl (clock.CurrentTimeMillisUnsafe()) ttl
                            elif Duration.isZero ttl then
                                MutableHashMap.remove self.Map key |> ignore))))

    /// Reads an existing entry without invoking `lookup`: `None` if missing or
    /// expired, otherwise awaits the (possibly pending) result. (Cache.getOption)
    let getOption (self: Cache<'Key, 'A, 'E>) (key: 'Key) : Effect<'A option, 'E, Context> =
        Clock.clockWith (fun clock ->
            Effect.suspend (fun () ->
                let now = clock.CurrentTimeMillisUnsafe()

                match getImpl self key now true with
                | Some entry -> Deferred.await entry.Deferred |> Effect.map Some
                | None -> Effect.succeed None))

    /// Returns the value only if a non-expired entry has already resolved
    /// successfully; `None` for missing/expired/failed/pending. (Cache.getSuccess)
    let getSuccess (self: Cache<'Key, 'A, 'E>) (key: 'Key) : Effect<'A option, 'F, Context> =
        Clock.clockWith (fun clock ->
            Effect.sync (fun () ->
                let now = clock.CurrentTimeMillisUnsafe()

                match getImpl self key now false with
                | Some entry ->
                    match entry.Result with
                    | Some(Success a) -> Some a
                    | _ -> None
                | None -> None))

    /// Forces a refresh: always runs `lookup`, overwriting any existing value and
    /// resetting its TTL. (Cache.refresh)
    let refresh (self: Cache<'Key, 'A, 'E>) (key: 'Key) : Effect<'A, 'E, Context> =
        Clock.clockWith (fun clock ->
            Effect.suspend (fun () ->
                let deferred = Deferred.makeUnsafe ()

                let entry =
                    { ExpiresAt = None
                      Deferred = deferred
                      Result = None }

                let now = clock.CurrentTimeMillisUnsafe()
                let existing = (getImpl self key now false).IsSome

                if not existing then
                    MutableHashMap.set self.Map key entry |> ignore
                    checkCapacity self

                self.Lookup key
                |> onExit (fun exit ->
                    Effect.sync (fun () ->
                        Deferred.doneUnsafe deferred (ofExit exit) |> ignore
                        entry.Result <- Some exit
                        let ttl = self.TimeToLive exit key

                        if Duration.isZero ttl then
                            MutableHashMap.remove self.Map key |> ignore
                        else
                            entry.ExpiresAt <- expiryFromTtl (clock.CurrentTimeMillisUnsafe()) ttl

                            if existing then
                                MutableHashMap.set self.Map key entry |> ignore))))

    // --- direct writes ---

    /// Sets `value` for `key` directly, skipping `lookup` and applying TTL. (Cache.set)
    let set (self: Cache<'Key, 'A, 'E>) (key: 'Key) (value: 'A) : Effect<unit, 'F, Context> =
        Clock.clockWith (fun clock ->
            Effect.sync (fun () ->
                let exit = Exit.succeed value
                let deferred = Deferred.makeUnsafe ()
                Deferred.doneUnsafe deferred (ofExit exit) |> ignore
                let ttl = self.TimeToLive exit key

                if Duration.isZero ttl then
                    MutableHashMap.remove self.Map key |> ignore
                else
                    let entry =
                        { ExpiresAt = expiryFromTtl (clock.CurrentTimeMillisUnsafe()) ttl
                          Deferred = deferred
                          Result = Some exit }

                    MutableHashMap.set self.Map key entry |> ignore
                    checkCapacity self))

    // --- inspection ---

    /// Whether a non-expired entry exists, without invoking `lookup`. (Cache.has)
    let has (self: Cache<'Key, 'A, 'E>) (key: 'Key) : Effect<bool, 'F, Context> =
        Clock.clockWith (fun clock ->
            Effect.sync (fun () ->
                let now = clock.CurrentTimeMillisUnsafe()
                (getImpl self key now false).IsSome))

    /// The number of stored entries (expired-but-unaccessed entries still count).
    /// (Cache.size)
    let size (self: Cache<'Key, 'A, 'E>) : Effect<int, 'F, Context> =
        Effect.sync (fun () -> MutableHashMap.size self.Map)

    /// All active (non-expired) keys; expired entries are pruned. (Cache.keys)
    let keys (self: Cache<'Key, 'A, 'E>) : Effect<'Key list, 'F, Context> =
        Clock.clockWith (fun clock ->
            Effect.sync (fun () ->
                let now = clock.CurrentTimeMillisUnsafe()
                let all = MutableHashMap.toSeq self.Map |> Seq.toList

                for (k, e) in all do
                    if hasExpired e now then
                        MutableHashMap.remove self.Map k |> ignore

                all |> List.choose (fun (k, e) -> if hasExpired e now then None else Some k)))

    /// All resolved-successful, non-expired key/value pairs; expired entries are
    /// pruned. (Cache.entries)
    let entries (self: Cache<'Key, 'A, 'E>) : Effect<('Key * 'A) list, 'F, Context> =
        Clock.clockWith (fun clock ->
            Effect.sync (fun () ->
                let now = clock.CurrentTimeMillisUnsafe()
                let all = MutableHashMap.toSeq self.Map |> Seq.toList

                for (k, e) in all do
                    if hasExpired e now then
                        MutableHashMap.remove self.Map k |> ignore

                all
                |> List.choose (fun (k, e) ->
                    if hasExpired e now then
                        None
                    else
                        match e.Result with
                        | Some(Success a) -> Some(k, a)
                        | _ -> None)))

    /// All resolved-successful, non-expired values. (Cache.values)
    let values (self: Cache<'Key, 'A, 'E>) : Effect<'A list, 'F, Context> =
        entries self |> Effect.map (List.map snd)

    // --- invalidation ---

    /// Removes the entry for `key` (no-op if absent). (Cache.invalidate)
    let invalidate (self: Cache<'Key, 'A, 'E>) (key: 'Key) : Effect<unit, 'F, Context> =
        Effect.sync (fun () -> MutableHashMap.remove self.Map key |> ignore)

    /// Removes the entry for `key` when `predicate` holds for its resolved value;
    /// returns whether it was removed. Failed/pending/missing → `false`.
    /// (Cache.invalidateWhen)
    let invalidateWhen (self: Cache<'Key, 'A, 'E>) (key: 'Key) (predicate: 'A -> bool) : Effect<bool, 'E, Context> =
        Clock.clockWith (fun clock ->
            Effect.suspend (fun () ->
                let now = clock.CurrentTimeMillisUnsafe()

                match getImpl self key now false with
                | None -> Effect.succeed false
                | Some entry ->
                    Deferred.await entry.Deferred
                    |> Effect.map (fun value ->
                        if predicate value then
                            MutableHashMap.remove self.Map key |> ignore
                            true
                        else
                            false)
                    |> catchCause (fun _ -> Effect.succeed false)))

    /// Removes all entries. (Cache.invalidateAll)
    let invalidateAll (self: Cache<'Key, 'A, 'E>) : Effect<unit, 'F, Context> =
        Effect.sync (fun () -> MutableHashMap.clear self.Map |> ignore)
