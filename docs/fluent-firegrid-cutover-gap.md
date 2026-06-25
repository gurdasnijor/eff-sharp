# fluent-firegrid → eff-sharp cutover: API-coverage gap analysis

**Goal:** enough eff-sharp API coverage (Fable-clean) to rewrite
`/Users/gnijor/gurdasnijor/fluent-firegrid` in idiomatic eff-sharp. When that
rewrite is possible, eff-sharp is functionally feature-complete for this purpose.

Date: 2026-06-25. Snapshot after the platform cutover (FileSystem + ChildProcess +
NodeStream/NodeSink merged). eff-sharp Fable state: **7 errors / 19 warnings**.

## Target shape

fluent-firegrid's own source (~42–71 `.ts` files; ignore `repos/` + `node_modules`)
spans five packages:

| Package | Dominant Effect deps | Cutover priority |
|---|---|---|
| **verification** | FileSystem ✓, ChildProcess ✓, process orchestration ✓, OpenTelemetry (Tracer/Metric ✓) | **1 (named driver)** |
| **fluent-acp-process** | ChildProcess ✓, Queue ✓, Stream ✓ | **1 (named driver)** |
| **observability** | `effect/unstable/sql` (chdb), Reactivity | 3 |
| **effect-s2** | Schema, `effect/unstable/httpapi` (OpenAPI client codegen) | 3 |
| **effect-s2-flow** | Ref ✓, Semaphore ✓, Fiber ✓ | 2 |

## Coverage matrix

**Already covered (present + ~Fable-clean):** Effect, Stream, Layer, Context, Ref,
Option, Fiber, Queue, Semaphore, Schedule, Duration, Scope, Cause/Exit, Schema
(non-reflective surface), FileSystem, ChildProcess(Spawner), NodeStream/NodeSink,
Tracer, Metric, Console, Random.

**Gaps — Fable errors in already-ported modules (7):**
1. `Clock.fs` (3) — `SleepUnsafe: Duration -> Task` uses `Task.Delay`/`CompletedTask`.
   Fix: change the sleep representation to `Async<unit>` (Fable maps `Async.Sleep`→
   `setTimeout`; works on .NET too). **Cutover-critical** (all timing).
   Blast radius: `Clock` type, `Clock.make`, `Clock.sleep`, **`TestClock`** (its
   virtual-time `TaskCompletionSource` waiters → swap to `Cell`), `RcMap.clockSleep`
   (poll-based mid-sleep interruption → race the clock sleep against an interrupt
   check; must still go *through* the Clock so TestClock controls it). Intricate
   because of TestClock's deterministic tests — do it focused, not rushed.
2. `RcMap.fs` (2) — `Task.IsCompleted` poll. Falls out of the Clock fix (it polls
   the clock sleep handle).
3. `TxRef.fs` (2) — `Monitor.Wait`/`PulseAll` blocking STM retry. The original
   "fundamental" item: redesign retry as async suspend/resume on the `Cell` wake
   machinery, `#if FABLE_COMPILER` (keep `Monitor` on .NET). Likely NOT needed by
   the cutover (fluent-firegrid doesn't appear to use TxRef) — fix for 0-error
   cleanliness, lower priority than Clock.

**Gaps — net-new modules (not yet ported):**
| Need | Used by | Notes / effort |
|---|---|---|
| **NodeRuntime** (`runMain` entry) | every package | Small. The single `provide`+run entry that wires all Node platform layers. Do early. |
| **HttpClient** (`effect/unstable/http` + NodeHttpClient) | verification (health checks), effect-s2 | Medium. Port the client surface + a Node/`fetch` or `System.Net.Http` dual-layer. |
| **Reactivity** (`effect/unstable/reactivity`) | observability, fiber-local | Medium — fiber-local context propagation. |
| **SQL** (`effect/unstable/sql` — Statement compiler, SqlClient) | observability (chdb) | ★★★ large — dialect placeholder rendering, error classification. |
| **httpapi** (OpenAPI → client codegen) | effect-s2 | ★★★ large — REST client generation. Consider keeping TS codegen, emitting F#. |
| **cli** (`effect/unstable/cli`) | verification | Medium — arg/flag parsing. |

## Recommended autonomous sequence

**Phase A — clear the Fable tail (path to 0 errors):**
1. **Clock → Async sleep** (+ TestClock on `Cell`, + RcMap rework). Cutover-critical.
   Verify: full TestClock/Clock/RcMap suites stay green (deterministic timing).
2. **TxRef async-retry** (`#if FABLE`). Gets to 0 Fable errors. Spike harness:
   `spikes/stm`.

**Phase B — minimal cutover surface for the named drivers (verification + acp):**
3. **NodeRuntime** — unified Node platform layer + `runMain`.
4. **HttpClient** (if verification's health checks need it) — dual-layer.
5. Confirm Tracer/Metric cover the OpenTelemetry usage; add `cli` if verification's
   entry parses args.
6. **Spike the verification + fluent-acp-process rewrite** in F# against the above —
   this is the real acceptance test; gaps it surfaces drive the next work.

**Phase C — the heavy/specialized packages (effect-s2, observability):**
7. SQL, httpapi, Reactivity — large; tackle after the drivers cut over, or decide
   per-package whether to keep TS (codegen) vs port.

## Verification protocol per step
- `dotnet build` + targeted xUnit (the module's tests) green.
- `./scripts/fable-coverage.sh` error count drops (or holds for net-new).
- Fable-compile the touched package; for Node-only paths, a Node smoke test.
- CI gates the merge (build·test ‖ format·lint).

## Acceptance
Feature-complete = a `cutover` branch of fluent-firegrid's **verification** and
**fluent-acp-process** packages compiles and runs as idiomatic eff-sharp F# (via
Fable to Node). effect-s2/observability follow as Phase C.
