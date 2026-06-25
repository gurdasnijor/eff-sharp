namespace Effect

/// `LayerMap` — a scoped, keyed map of memoized layer-built service contexts.
///
/// Port of repos/effect-smol/packages/effect/src/LayerMap.ts. A key is turned
/// into a cached `Context` built from a `Layer`, shared (reference-counted) by
/// concurrent borrowers, and released when the last borrower's scope closes
/// (subject to a per-key idle time-to-live). Built directly on the ported
/// `RcMap`.
///
/// Decoupling / omissions (per CONVENTIONS), driven by the simplified kernel:
///   * The map-owning `Scope` is passed *explicitly* to `make`/`fromRecord`, and
///     `contextEffect`/`get` route the borrower `Scope` through `RcMap.get` —
///     mirroring how the ported `RcMap`/`Scope` work.
///   * Idle-TTL eviction uses **real wall-clock time** (RcMap's live `Clock` +
///     `Effect.sleep`); there is no `TestClock`, so tests use small real delays.
///   * `Layer.MemoMap` sub-layer dedup is deferred (the port's `Layer` is not
///     memoized); `RcMap` still memoizes one built context per key.
///   * The `Service` class form and `preloadKeys` are dropped; the layer input is
///     fixed to `unit`.
/// A keyed, reference-counted map of layer-built contexts.
type LayerMap<'K, 'E when 'K: equality> =
    internal
        { RcMap: RcMap<'K, Context, 'E>
          Lookup: 'K -> Layer<'E, unit> }

[<RequireQualifiedAccess>]
module LayerMap =

    /// Create a `LayerMap` whose entries live no longer than `mapScope`, building
    /// `lookup key` on first request and releasing it after `idleTimeToLive key`
    /// of disuse. (LayerMap.make)
    let make
        (mapScope: Scope<'E, unit>)
        (idleTimeToLive: 'K -> Duration)
        (lookup: 'K -> Layer<'E, unit>)
        : Effect<LayerMap<'K, 'E>, 'E, unit> =
        RcMap.makeWith mapScope None idleTimeToLive (fun key entryScope -> (lookup key).Build entryScope)
        |> Effect.map (fun rc -> { RcMap = rc; Lookup = lookup })

    /// `make` with immediate release at reference count zero. (LayerMap.make, no TTL)
    let makeDefault (mapScope: Scope<'E, unit>) (lookup: 'K -> Layer<'E, unit>) : Effect<LayerMap<'K, 'E>, 'E, unit> =
        make mapScope (fun _ -> Duration.zero) lookup

    /// Create a `LayerMap` from a finite map of predefined layers. (LayerMap.fromRecord)
    let fromRecord
        (mapScope: Scope<'E, unit>)
        (idleTimeToLive: 'K -> Duration)
        (layers: Map<'K, Layer<'E, unit>>)
        : Effect<LayerMap<'K, 'E>, 'E, unit> =
        make mapScope idleTimeToLive (fun key -> Map.find key layers)

    /// The cached context for `key`, retained for `callerScope`. (LayerMap.contextEffect)
    let contextEffect (self: LayerMap<'K, 'E>) (key: 'K) (callerScope: Scope<'E, unit>) : Effect<Context, 'E, unit> =
        RcMap.get self.RcMap key callerScope

    /// A `Layer` that, when built, retains `key`'s context for the building scope.
    /// (LayerMap.get)
    let get (self: LayerMap<'K, 'E>) (key: 'K) : Layer<'E, unit> =
        { Build = fun scope -> RcMap.get self.RcMap key scope }

    /// Invalidate the cached entry for `key`, releasing it if unused. (LayerMap.invalidate)
    let invalidate (self: LayerMap<'K, 'E>) (key: 'K) : Effect<unit, 'E, unit> = RcMap.invalidate self.RcMap key

    /// Whether `key` currently has a cached entry. (LayerMap.rcMap has)
    let contains (self: LayerMap<'K, 'E>) (key: 'K) : Effect<bool, 'E, unit> = RcMap.has self.RcMap key
