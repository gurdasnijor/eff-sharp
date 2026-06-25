# Future direction: synchronous interpreter-loop core (JS performance)

**Status: planned, not started.** Captured 2026-06-25 after the first fair
same-runtime benchmark (`js-bench/compare.mjs`) quantified the gap. Do **not**
start this until the current Fable-correctness work is merged and stable.

## The finding

`js-bench/compare.mjs` times eff-sharp's Fable-generated JS against stock Effect,
both on the same Node, both through their Promise runner (fair — unlike the old
`.NET`-vs-`JS` `RESULTS.md`). Representative run:

| Workload | eff-sharp (JS) | stock Effect | ratio |
|---|---:|---:|---:|
| Ref update/get (10k) | ~118 ms | ~1.2 ms | ~50–100× |
| bind throughput (10k flatMap) | ~30 ms | ~0.6 ms | ~50× |
| succeed/map (10k map) | ~22 ms | ~0.6 ms | ~35× |
| fork/join (1k) | ~11 ms | ~14–47 ms | **0.2–0.8× (we win)** |

The sync hot paths are 1–2 orders of magnitude slower; the async path is already
competitive (even faster, because stock's `forkChild` is heavier than our fork).

## Root cause

`Effect<A,E,R> = FiberRuntime -> 'R -> Async<Exit<'A,'E>>`. Every `map`/`flatMap`
threads through F# `Async`, which Fable compiles to a JS state machine with
per-step closure/continuation allocation. Worse, Fable's `Async` trampolines deep
chains with a real async hop (this is why `runSync` cannot drive a 10k-deep
program on JS — it reports "suspended"). So even a purely synchronous program pays
full async machinery on every bind.

Stock Effect is fast because its core is **not** a callback chain. It is a
synchronous **interpreter loop** (`while`) over a tagged-primitive data structure
(`OP_SUCCEED`, `OP_FLATMAP`, `OP_SYNC`, …); it allocates a continuation only at a
*true* suspension point and touches Promises only at async boundaries.

## Why NOT `Async` → `Promise`

Evaluated and rejected as the fix (see the conversation that produced this doc):

- A JS `Promise` is **spec-mandated** to run `.then` callbacks as microtasks — it
  *cannot* execute a chain synchronously. A 10k-`.then` chain = 10k microtask hops,
  which is the same or worse, and forces *everything* async (no `runSync` at all).
- Stock Effect does not build its core on Promises; it uses the interpreter loop
  above. Promise is the right tool for the **interop boundary** (our `runPromise`
  already bridges via `Async.StartAsPromise` — keep that) and for application-level
  I/O code, but not for the effect interpreter.

The Fable guidance "prefer `Fable.Promise` over `Async`" is correct for app code;
it does not apply to building a high-performance effect-system runtime.

## The actual fix (scope sketch)

Re-represent `Effect` as interpretable data + a synchronous trampoline runner — the
stock-Effect / ZIO / cats-effect design:

1. Replace the `FiberRuntime -> 'R -> Async<Exit>` function representation with a
   DU of primitives: `Succeed`, `Fail`, `Sync of (unit -> 'a)`, `FlatMap of ... `,
   `Async of (...)` (the only node that actually suspends), `WithRuntime`, etc.
2. Write a `runLoop`: a `while` loop with an explicit continuation stack that
   interprets primitives synchronously, pushing/popping continuations, and only
   yields to the host (Promise/`setImmediate`) when it hits an `Async` node or a
   fairness yield-budget boundary.
3. `runSync` = run the loop; succeed if it completes without hitting a real async
   node, else raise (the existing contract). `runPromise` = run the loop, bridging
   async nodes to Promise.
4. Keep the public combinator surface (`Effect.map`/`flatMap`/`gen`/`effect { }`)
   identical — this is a representation swap under the surface, so the ~1200 tests
   and all downstream modules should be unaffected if the surface is preserved.

Cost: a core rewrite, but localized to `Effect.fs` + `Runtime.fs` + `Fiber.fs`.
Risk: interruption/finalizer semantics and stack-safety need re-validation (the
existing `spikes/fiber` harness + the full suite are the safety net). Measure every
step with `js-bench/bench-compare.sh` — that is now the regression gauge.

## Open questions to resolve before starting

- Do we keep the `Async`-based core on **.NET** (where it is fine and proven) and
  introduce the interpreter loop only under `#if FABLE_COMPILER`, or unify on the
  loop for both targets? (Unifying is cleaner long-term but re-validates .NET.)
- Does any current consumer (`fluent-firegrid`) actually need sync throughput, or
  is it I/O-bound (where we are already competitive)? That decides priority.
