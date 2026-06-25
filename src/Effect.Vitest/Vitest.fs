namespace Effect

open Fable.Core
open Fable.Core.JsInterop

/// eff-sharp port of `@effect/vitest` (Stage 1).
///
/// Port of repos/effect-smol/packages/vitest. A thin F# surface over Vitest plus
/// Effect-aware test runners, so a test is authored ONCE in F# (compiled through
/// Fable) and runs on Node against the shipped artifact — no per-test TypeScript.
///
/// Vitest's `describe` / `it` / `expect` are imported from the npm `vitest`
/// package, so generated specs are explicit ESM modules and do not rely on
/// `globals: true`.
///
/// Stage 1 runs `itEffect` against the live `Clock`. Stage 2 will provide a
/// TestClock-backed `TestEnv` by default (mirroring upstream's `it.effect`), now
/// that `TestClock` is Fable-portable; `itEffectIn` already accepts such a context.
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

    /// Run an Effect test to a Promise, rejecting (failing the test) on Failure —
    /// the eff-sharp analogue of upstream's `runPromise`. `expect` assertions that
    /// throw inside the effect become defects → Failure → rejection.
    let private runEffect (env: Context) (eff: Effect<unit, 'E, Context>) : JS.Promise<unit> =
        async {
            match! Effect.runExit env eff with
            | Success() -> return ()
            | Failure cause -> return failwith (Cause.render cause)
        }
        |> Async.StartAsPromise

    /// `it.effect` — a test whose body is an Effect, run with the live `Clock`.
    let itEffect (name: string) (body: unit -> Effect<unit, 'E, Context>) : unit =
        itAsync name (fun () -> runEffect Clock.live (body ()))

    /// Like `itEffect`, but with an explicit environment (e.g. a `TestClock`
    /// context from `TestClock.context`).
    let itEffectIn (env: Context) (name: string) (body: unit -> Effect<unit, 'E, Context>) : unit =
        itAsync name (fun () -> runEffect env (body ()))
