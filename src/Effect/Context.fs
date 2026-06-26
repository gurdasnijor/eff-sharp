namespace Effect

/// `Context` — the dependency-injection (DI) service map.
///
/// Port of repos/effect-smol/packages/effect/src/Context.ts, **decoupled from
/// upstream's internals**. Upstream's `Context` imports `Effectable` (which pulls
/// in `Fiber`); per the slice brief we do NOT follow that. In F# a `Context` is
/// just a typed, immutable map from a service `Tag` to its instance — it needs
/// nothing from `Effect`, so it compiles *before* `Effect.fs` and the async core
/// wires service access on top (`Effect.service` / `Effect.provideService`).
///
/// Omissions vs upstream (per CONVENTIONS):
///   * The HKT / `Effectable` / `Pipeable` / `Inspectable` plumbing is dropped.
///   * The JS runtime guard `isKey`/`isContext` over arbitrary values is dropped
///     (F#'s static types make it dead weight).
/// A typed key identifying a service of type `'Service` stored in a `Context`.
/// The `'Service` parameter is phantom (it only tracks the value type at compile
/// time); identity at runtime is the unique `Key` string, mirroring upstream's
/// string `key`.
type Tag<'Service> = internal { Key: string }

/// A typed context key with a built-in default value. This is the Effect v4
/// replacement for the old FiberRef family: ambient defaults live in Context and
/// can be overridden by providing a value at the same key.
type Reference<'Service> = { Key: string; Default: 'Service }

/// An immutable, typed map from service `Tag`s to their instances. Services are
/// boxed internally and recovered by the typed `Tag` on read.
type Context = internal { Services: Map<string, obj> }

[<RequireQualifiedAccess>]
module Tag =

    /// Create a typed service tag from a unique key string. Two tags with the
    /// same key address the same slot (last write wins), matching upstream.
    let make<'Service> (key: string) : Tag<'Service> = { Key = key }

[<RequireQualifiedAccess>]
module Reference =

    /// Create a typed reference from a unique key string and default value.
    let make (key: string) (def: 'Service) : Reference<'Service> = { Key = key; Default = def }

    /// Read the built-in default value.
    let defaultValue (r: Reference<'Service>) : 'Service = r.Default

    /// Treat a reference as a plain service tag. This keeps Layer/Context APIs
    /// interoperable: a provided service at the same key overrides the default.
    let toTag (r: Reference<'Service>) : Tag<'Service> = { Key = r.Key }

[<RequireQualifiedAccess>]
module Context =


    /// The empty context — no services. (Context.empty)
    let empty: Context = { Services = Map.empty }

    /// Add (or replace) a service under `tag`. (Context.add)
    let add (tag: Tag<'Service>) (service: 'Service) (ctx: Context) : Context =
        { Services = ctx.Services |> Map.add tag.Key (box service) }

    /// Add (or replace) a service under `reference`, overriding its default.
    let addReference (reference: Reference<'Service>) (service: 'Service) (ctx: Context) : Context =
        { Services = ctx.Services |> Map.add reference.Key (box service) }

    /// A one-service context. (Context.make)
    let make (tag: Tag<'Service>) (service: 'Service) : Context = add tag service empty

    /// A one-service context keyed by a reference.
    let makeReference (reference: Reference<'Service>) (service: 'Service) : Context =
        addReference reference service empty

    /// Read a service, or `None` if absent. (Context.getOption)
    let tryGet (tag: Tag<'Service>) (ctx: Context) : 'Service option =
        ctx.Services |> Map.tryFind tag.Key |> Option.map unbox<'Service>

    /// Read a reference override, or `None` if absent.
    let tryGetReference (reference: Reference<'Service>) (ctx: Context) : 'Service option =
        ctx.Services |> Map.tryFind reference.Key |> Option.map unbox<'Service>

    /// Read a service, raising if absent. (Context.get / unsafeGet)
    /// `Map.find` already raises `KeyNotFoundException` on a missing key.
    let get (tag: Tag<'Service>) (ctx: Context) : 'Service =
        ctx.Services |> Map.find tag.Key |> unbox<'Service>

    /// Read a reference override, falling back to the built-in default.
    let getReference (reference: Reference<'Service>) (ctx: Context) : 'Service =
        ctx |> tryGetReference reference |> Option.defaultValue reference.Default

    /// Alias of `get`, mirroring upstream's `unsafeGet`. (Context.unsafeGet)
    let unsafeGet (tag: Tag<'Service>) (ctx: Context) : 'Service = get tag ctx

    /// Whether a service is present. (Context.has)
    let contains (tag: Tag<'Service>) (ctx: Context) : bool = ctx.Services |> Map.containsKey tag.Key

    /// Whether a reference has an override in the context.
    let containsReference (reference: Reference<'Service>) (ctx: Context) : bool =
        ctx.Services |> Map.containsKey reference.Key

    /// Merge two contexts; on key conflict the services in `b` win. (Context.merge)
    let merge (a: Context) (b: Context) : Context =
        { Services = (a.Services, b.Services) ||> Map.fold (fun acc k v -> Map.add k v acc) }
