# RFC-001 — Interpreter-loop core (microtask scheduler)

**Status:** Proposed · **Area:** core runtime (stack-rank #7) · **Risk:** High ·
**Prereq:** project-identity decision = "conformant runtime"

## Problem

The core is `Effect<'A,'E,'R> = FiberRuntime -> 'R -> Async<Exit<'A,'E>>` — a
Reader over F# `Async`. Three problems trace to this substrate:

1. **Performance.** Every `flatMap` allocates an `Async` continuation and threads
   it through Fable's CPS trampoline. The cross-runtime benchmark
   (`benchmarks/cross-runtime/RESULTS.md`) quantifies the delta vs upstream's
   defunctionalized interpreter.
2. **Yield primitive is wrong for JS.** Fable's `AsyncBuilder` trampoline yields
   to the **macrotask** queue via `setTimeout(0)` every ~2000 binds
   (`maxTrampolineCallCount`). Macrotasks interleave with timers and I/O and are
   far slower than microtasks. It also breaks `runSync` for deep chains (the
   effect "suspends" once it crosses the trampoline boundary).
3. **Coarse interruption / no scheduler.** Interruption is a flag checked only at
   `flatMap`/`sleep`; there's no op-level preemption, no cooperative `yieldNow`,
   no deterministic scheduling for tests.

Effect-TS solves all three with a defunctionalized op-tree interpreted by a step
loop on a **microtask-first** scheduler (`Scheduler.ts` `MixedScheduler`:
`Promise.resolve().then` for the first ~2048 nested drains, `setTimeout(0)` only
as an anti-I/O-starvation fallback).

## Proposal

Replace the function-based representation with a **defunctionalized op tree** +
an explicit-stack step loop scheduled microtask-first. Keep the entire public
combinator surface unchanged — combinators become op constructors, so callers and
tests are unaffected.

```fsharp
// op tree — one case per primitive (closed DU, no HKT)
type internal Op<'A, 'E, 'R> =
    | Succeed of 'A
    | Fail of Cause<'E>
    | Sync of (unit -> 'A)
    | FlatMap of Op<obj, 'E, 'R> * (obj -> Op<'A, 'E, 'R>)   // boxed cont stack
    | Async of ((Exit<'A,'E> -> unit) -> unit)               // register a resume
    | Yield                                                   // cooperative yield
    | Fork of Op<obj, 'E, 'R>
    // ... WithRuntime, Provide, etc.

type Effect<'A,'E,'R> = internal { op: Op<'A,'E,'R> }
```

The fiber holds an explicit continuation stack and a step budget; the loop pops
ops, pushes continuations, and re-arms on the scheduler when the budget is spent
or an async op parks:

```fsharp
[<Emit("queueMicrotask($0)")>]
let private microtask (f: unit -> unit) : unit = nativeOnly

// after N synchronous steps, yield to the microtask queue (fallback to
// setTimeout(0) after K nested drains to avoid starving I/O), mirroring
// MixedScheduler.
```

- `Async` ops bridge to the outside world via a resume callback —
  `Async.awaitPromise` / `startAsPromise` stay as **boundary** adapters only.
- Interruption becomes an op-level check (every step), giving real granularity
  and letting finalizers run deterministically.
- A pluggable scheduler enables a deterministic test scheduler (fixes the
  `TestClock`-determinism gap for free).

## Tradeoffs / risks

- **Big, invasive change** to the one file everything depends on. Mitigate by
  keeping the public API identical and landing behind the cross-runtime benchmark
  + (eventually) the upstream conformance suite as the safety net.
- **Stack-safety is now our responsibility** (explicit stack, not `Async`'s
  trampoline) — but that's the point, and it's a well-trodden design (Effect-TS,
  ZIO, Fable's own trampoline prove it compiles/runs under Fable).
- `[<Emit>]` for `queueMicrotask` is JS-only; provide a `.NET` fallback
  (`ThreadPool.QueueUserWorkItem` or inline) under `#if !FABLE_COMPILER`.

## Migration

1. Introduce `Op` + interpreter alongside the current core; re-point one
   combinator at a time.
2. Gate with the cross-runtime benchmark (expect the `deep_bind`/chain rows to
   move the most).
3. Flip `runSync`/`runPromise`/`fork` to drive the interpreter; delete the
   `Async`-threading core once parity holds.

## Effort

High (core rewrite). Sequence after the identity decision; treat as a funded
track, not a drive-by.

## Sources

- Fable `AsyncBuilder.ts` (`maxTrampolineCallCount=2000`, `setTimeout(0)` hijack):
  https://github.com/fable-compiler/Fable/blob/main/src/fable-library-ts/AsyncBuilder.ts
- Effect-TS `Scheduler.ts` (`MixedScheduler`, microtask-first, `maxNextTickBeforeTimer=2048`):
  https://github.com/Effect-TS/effect/blob/main/packages/effect/src/Scheduler.ts
