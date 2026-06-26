namespace Effect

/// Slice: wave-5. `Layer` — composable, scoped dependency injection (port of
/// Layer.ts). A layer **builds an output `Context`** (its provided services) from
/// an input environment `'RIn`, within a `Scope` so scoped services are released
/// when the providing scope closes.
///
/// Departure from upstream (HKT limitation, per CONVENTIONS §4): upstream tracks
/// the precise provided service-set in the `ROut` type parameter
/// (`Layer<ROut, E, RIn>`). F# has no type-level set computation, and our `Context`
/// is a runtime-keyed map, so we drop `ROut` — a `Layer<'E,'RIn>` provides services
/// checked at runtime by `Tag`. Memoization (`MemoMap`) and the Stream-based layer
/// features are deferred (the core DI surface needs neither).
type Layer<'E, 'RIn> =
    internal
        { Build: Scope<'E, 'RIn> -> Effect<Context, 'E, 'RIn> }

[<RequireQualifiedAccess>]
module Layer =

    let private contextFromEnv (env: 'R) : Context =
        match box env with
        | :? Context as ctx -> ctx
        | _ -> Context.empty

    // --- constructors ---

    /// A layer providing exactly the given context. (Layer.succeedContext)
    let succeedContext (ctx: Context) : Layer<'E, 'RIn> = { Build = fun _ -> Effect.succeed ctx }

    /// The empty layer (provides nothing). (Layer.empty)
    let empty<'E, 'RIn> : Layer<'E, 'RIn> = succeedContext Context.empty

    /// A layer providing one service. (Layer.succeed)
    let succeed (tag: Tag<'Service>) (service: 'Service) : Layer<'E, 'RIn> =
        succeedContext (Context.make tag service)

    /// A layer providing one reference override.
    let succeedReference (reference: Reference<'Service>) (service: 'Service) : Layer<'E, 'RIn> =
        succeedContext (Context.makeReference reference service)

    /// A layer providing one service from a thunk. (Layer.sync)
    let sync (tag: Tag<'Service>) (f: unit -> 'Service) : Layer<'E, 'RIn> =
        { Build = fun _ -> Effect.sync f |> Effect.map (Context.make tag) }

    /// A layer providing one reference override from a thunk.
    let syncReference (reference: Reference<'Service>) (f: unit -> 'Service) : Layer<'E, 'RIn> =
        { Build = fun _ -> Effect.sync f |> Effect.map (Context.makeReference reference) }

    /// A layer providing one service from an effect. (Layer.effect)
    let effect (tag: Tag<'Service>) (eff: Effect<'Service, 'E, 'RIn>) : Layer<'E, 'RIn> =
        { Build = fun _ -> eff |> Effect.map (Context.make tag) }

    /// A layer providing one reference override from an effect.
    let effectReference (reference: Reference<'Service>) (eff: Effect<'Service, 'E, 'RIn>) : Layer<'E, 'RIn> =
        { Build = fun _ -> eff |> Effect.map (Context.makeReference reference) }

    /// A layer providing a whole context from an effect. (Layer.effectContext)
    let effectContext (eff: Effect<Context, 'E, 'RIn>) : Layer<'E, 'RIn> = { Build = fun _ -> eff }

    /// A layer whose service has a scoped lifetime: `f` receives the layer's scope
    /// to register finalizers, run when the providing scope closes. (Layer.scoped)
    let scoped (tag: Tag<'Service>) (f: Scope<'E, 'RIn> -> Effect<'Service, 'E, 'RIn>) : Layer<'E, 'RIn> =
        { Build = fun scope -> f scope |> Effect.map (Context.make tag) }

    /// A layer whose reference override has a scoped lifetime.
    let scopedReference
        (reference: Reference<'Service>)
        (f: Scope<'E, 'RIn> -> Effect<'Service, 'E, 'RIn>)
        : Layer<'E, 'RIn> =
        { Build = fun scope -> f scope |> Effect.map (Context.makeReference reference) }

    // --- composition ---

    /// Provide both layers' services (right wins on key conflict). (Layer.merge)
    let merge (a: Layer<'E, 'RIn>) (b: Layer<'E, 'RIn>) : Layer<'E, 'RIn> =
        { Build =
            fun scope ->
                a.Build scope
                |> Effect.flatMap (fun ca -> b.Build scope |> Effect.map (fun cb -> Context.merge ca cb)) }

    /// Merge a list of layers. (Layer.mergeAll)
    let mergeAll (layers: Layer<'E, 'RIn> list) : Layer<'E, 'RIn> = layers |> List.fold merge empty

    // --- build / provide ---

    /// Build the layer's context using a caller-supplied scope (the scope governs
    /// scoped services' lifetime). (Layer.buildWithScope)
    let buildWithScope (scope: Scope<'E, 'RIn>) (layer: Layer<'E, 'RIn>) : Effect<Context, 'E, 'RIn> = layer.Build scope

    /// Build the layer's context in a fresh scope that is closed immediately after.
    /// NOTE: only safe for layers with no scoped services; use `provide` to keep a
    /// scoped layer alive for the duration of a consuming effect. (Layer.build)
    let build (layer: Layer<'E, 'RIn>) : Effect<Context, 'E, 'RIn> =
        Scope.scoped (fun scope -> layer.Build scope)

    /// Provide a layer to an effect, preserving any ambient `Context` services.
    /// The layer is built in a scope that wraps the consuming effect, so scoped
    /// services live for its duration and are released after. Layer services win
    /// on key conflict, matching Effect's "provided service overrides ambient"
    /// semantics. (Layer.provide)
    let provide (layer: Layer<'E, 'RIn>) (eff: Effect<'A, 'E, Context>) : Effect<'A, 'E, 'RIn> =
        Scope.scoped (fun scope ->
            Effect.environment<'RIn, 'E>
            |> Effect.flatMap (fun env ->
                layer.Build scope
                |> Effect.flatMap (fun provided ->
                    let ctx = Context.merge (contextFromEnv env) provided
                    Effect.provideContext ctx eff)))
