namespace Effect

open Fable.Core
open Fable.Core.JsInterop

/// eff-sharp port of `@effect/vitest`.
///
/// Port of repos/effect-smol/packages/vitest. A thin F# surface over Vitest plus
/// Effect-aware test runners, so a test is authored ONCE in F# (compiled through
/// Fable) and runs on Node against the shipped artifact — no per-test TypeScript.
///
/// Vitest's `describe` / `it` / `expect` are imported from the npm `vitest`
/// package, so generated specs are explicit ESM modules and do not rely on
/// `globals: true`.
///
/// `itEffect` runs against a test environment by default (currently a fresh
/// `TestClock` per test), mirroring upstream's `it.effect`. `itLive` runs without
/// test services. `layer` shares a built layer across a block of tests and hands
/// the block an `it` object with `.effect` / `.live` / `.layer` methods.
///
/// Not `RequireQualifiedAccess`: specs `open Effect.Vitest` and use `describe` /
/// `test` / `itEffect` / `toBe` unqualified, like a native test DSL.
module Vitest =

    // ── Vitest interop ──────────────────────────────────────────────────────

    [<AllowNullLiteral>]
    type private Expectation<'a> =
        abstract toBe: expected: 'a -> unit
        abstract toEqual: expected: 'a -> unit
        abstract toBeTruthy: unit -> unit
        abstract toBeFalsy: unit -> unit
        abstract toContain<'b> : item: 'b -> unit
        abstract toHaveLength: length: int -> unit
        abstract toBeGreaterThan: expected: float -> unit
        abstract toBeLessThan: expected: float -> unit
        abstract toThrow: unit -> unit

    [<Import("expect", "vitest")>]
    let private expect<'a> (actual: 'a) : Expectation<'a> = nativeOnly

    [<Import("describe", "vitest")>]
    let describe (name: string) (body: unit -> unit) : unit = nativeOnly

    /// A synchronous test (no Effect). The body throws (via `toBe`/`toEqual`) on
    /// failure, which Vitest reports.
    [<Import("it", "vitest")>]
    let test (name: string) (body: unit -> unit) : unit = nativeOnly

    [<Import("it", "vitest")>]
    let private itAsync (name: string) (body: unit -> JS.Promise<unit>) : unit = nativeOnly

    [<Import("beforeAll", "vitest")>]
    let private beforeAllAsync (body: unit -> JS.Promise<unit>) : unit = nativeOnly

    [<Import("afterAll", "vitest")>]
    let private afterAllAsync (body: unit -> JS.Promise<unit>) : unit = nativeOnly

    /// `expect(actual).toBe(expected)` — strict (`===`) equality; for primitives.
    let toBe (actual: 'a) (expected: 'a) : unit = (expect actual).toBe expected

    /// `expect(actual).toEqual(expected)` — deep structural equality.
    let toEqual (actual: 'a) (expected: 'a) : unit = (expect actual).toEqual expected

    let toBeTruthy (actual: 'a) : unit = (expect actual).toBeTruthy()

    let toBeFalsy (actual: 'a) : unit = (expect actual).toBeFalsy()

    /// `expect(collection).toContain(item)`.
    let toContain (collection: 'a) (item: 'b) : unit = (expect collection).toContain item

    /// `expect(actual).toHaveLength(n)`.
    let toHaveLength (actual: 'a) (length: int) : unit = (expect actual).toHaveLength length

    let toBeGreaterThan (actual: float) (expected: float) : unit =
        (expect actual).toBeGreaterThan expected

    let toBeLessThan (actual: float) (expected: float) : unit =
        (expect actual).toBeLessThan expected

    /// `expect(thunk).toThrow()` — the thunk must raise. Port of `Assert.Throws`.
    let toThrow (thunk: unit -> 'a) : unit = (expect thunk).toThrow()

    // ── Effect-aware runners (the @effect/vitest core) ──────────────────────

    /// The local analogue of upstream's test services. It intentionally exposes
    /// the concrete `TestClock` so tests can `awaitSleeps` / `adjust` directly.
    type TestEnv =
        { Clock: TestClock
          Context: Context }

    let makeTestEnv () : TestEnv =
        let clock = TestClock.make ()

        { Clock = clock
          Context = TestClock.context clock }

    /// Run an Effect test to a Promise, rejecting (failing the test) on Failure —
    /// the eff-sharp analogue of upstream's `runPromise`. `expect` assertions that
    /// throw inside the effect become defects → Failure → rejection.
    let runEffect (env: Context) (eff: Effect<unit, 'E, Context>) : JS.Promise<unit> =
        async {
            match! Effect.runExit env eff with
            | Success() -> return ()
            | Failure cause -> return failwith (Cause.render cause)
        }
        |> Async.StartAsPromise

    let private runUnit (eff: Effect<unit, 'E, unit>) : JS.Promise<unit> =
        async {
            match! Effect.runExit () eff with
            | Success() -> return ()
            | Failure cause -> return failwith (Cause.render cause)
        }
        |> Async.StartAsPromise

    /// `it.effect` — a test whose body is an Effect, run with test services.
    let itEffect (name: string) (body: unit -> Effect<unit, 'E, Context>) : unit =
        itAsync name (fun () ->
            let env = makeTestEnv ()
            runEffect env.Context (body ()))

    /// `it.effect` with direct access to this test's services, especially the
    /// fresh `TestClock`.
    let itEffectEnv (name: string) (body: TestEnv -> Effect<unit, 'E, Context>) : unit =
        itAsync name (fun () ->
            let env = makeTestEnv ()
            runEffect env.Context (body env))

    /// Upstream `it.live` — run without test services.
    let itLive (name: string) (body: unit -> Effect<unit, 'E, Context>) : unit =
        itAsync name (fun () -> runEffect Clock.live (body ()))

    /// Like `itEffect`, but with an explicit environment (e.g. a `TestClock`
    /// context from `TestClock.context`).
    let itEffectIn (env: Context) (name: string) (body: unit -> Effect<unit, 'E, Context>) : unit =
        itAsync name (fun () -> runEffect env (body ()))

    type It internal (context: unit -> Context, layerBlock: Layer<obj, Context> -> (It -> unit) -> unit) =
        /// Run a test in this block's environment.
        member _.effect (name: string) (body: unit -> Effect<unit, 'E, Context>) : unit =
            itAsync name (fun () -> runEffect (context ()) (body ()))

        /// Run a test against the live environment, ignoring this block's layer.
        member _.live (name: string) (body: unit -> Effect<unit, 'E, Context>) : unit =
            itLive name body

        /// Nest a layer under this block's environment.
        member _.layer (layer: Layer<'E, Context>) (body: It -> unit) : unit =
            layerBlock (unbox<Layer<obj, Context>> (box layer)) body

    type private LayerState =
        { mutable Context: Context option
          Scope: Scope<obj, Context> }

    let rec private layerWithContext
        (parent: unit -> Context)
        (layer: Layer<obj, Context>)
        (body: It -> unit)
        : unit =
        let state: LayerState =
            { Context = None
              Scope = Scope.make () }

        beforeAllAsync (fun () ->
            let parentContext = parent ()

            let build =
                Layer.buildWithScope state.Scope layer
                |> Effect.map (fun ctx ->
                    state.Context <- Some(Context.merge parentContext ctx))

            runEffect parentContext build)

        afterAllAsync (fun () -> runEffect (parent ()) (Scope.close state.Scope (Success(box ()))))

        let current () =
            match state.Context with
            | Some ctx -> ctx
            | None -> failwith "Effect.Vitest.layer: layer context was read before beforeAll completed"

        let nestedLayerBlock (nestedLayer: Layer<obj, Context>) (nestedBody: It -> unit) =
            layerWithContext current nestedLayer nestedBody

        body (It(current, nestedLayerBlock))

    let private eraseLayer (layer: Layer<'E, Context>) : Layer<obj, Context> =
        unbox<Layer<obj, Context>> (box layer)

    /// Share a layer across tests in this block, mirroring upstream
    /// `layer(Foo.Live)((it) => ...)`. The layer is built in `beforeAll` and its
    /// scope closes in `afterAll`.
    let layer (layer: Layer<'E, Context>) (body: It -> unit) : unit =
        let env = makeTestEnv ()
        layerWithContext (fun () -> env.Context) (eraseLayer layer) body

    /// Named layer block: `layerNamed Foo.Live "name" (fun it -> ...)`.
    let layerNamed (layer_: Layer<'E, Context>) (name: string) (body: It -> unit) : unit =
        describe name (fun () -> layer layer_ body)

    /// Upstream-style `it` object for top-level grouped specs.
    let it: It =
        let current () = (makeTestEnv ()).Context

        let layerBlock (layer: Layer<obj, Context>) (body: It -> unit) =
            layerWithContext current layer body

        It(current, layerBlock)
