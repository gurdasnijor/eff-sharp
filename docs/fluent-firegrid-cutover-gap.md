# fluent-firegrid → eff-sharp cutover: API-coverage gap analysis

**Goal:** enough eff-sharp API coverage (Fable-clean) to rewrite
`/Users/gnijor/gurdasnijor/fluent-firegrid` in idiomatic eff-sharp F#. When that
rewrite is possible, eff-sharp is functionally feature-complete for this purpose.

Snapshot: 2026-06-25, after the platform cutover + Clock fix + NodeRuntime.
eff-sharp Fable state: **2 errors** (only TxRef's STM `Monitor.Wait`/`PulseAll`).

## Target

fluent-firegrid's own source (~42–71 `.ts`; ignore `repos/`, `node_modules`) spans:

| Package | Dominant Effect deps | Cutover priority |
|---|---|---|
| **verification** | FileSystem ✓, ChildProcess ✓, Tracer/Metric ✓, (cli?) | **1 (named driver)** |
| **fluent-acp-process** | ChildProcess ✓, Queue ✓, Stream ✓ | **1 (named driver)** |
| **effect-s2-flow** | Ref ✓, Semaphore ✓, Fiber ✓ | 2 |
| **observability** | `effect/unstable/sql` (chdb), Reactivity | 3 |
| **effect-s2** | Schema, `effect/unstable/httpapi` (OpenAPI codegen) | 3 |

## Coverage

**Covered + Fable-clean:** Effect, Stream, Sink, Layer, Context, Ref, Option, Fiber,
Queue, Semaphore, Schedule, Duration, Scope, Cause/Exit, Schema (non-reflective),
Clock, FileSystem, ChildProcess(Spawner), NodeStream/NodeSink, **NodeRuntime**,
Tracer, Metric, Console, Random.

**Remaining Fable errors (2):** `TxRef` — `Monitor.Wait`/`PulseAll` blocking STM
retry. Redesign as async suspend/resume on the `Cell` wake machinery,
`#if FABLE_COMPILER` (keep `Monitor` on .NET). NOT used by fluent-firegrid →
cleanliness, not cutover-blocking.

**Net-new modules still needed (by package):**
| Need | Used by | Effort |
|---|---|---|
| **HttpClient** (`effect/unstable/http` + NodeHttpClient) | verification health checks, effect-s2 | Medium — client surface + `fetch`/`System.Net.Http` dual-layer |
| **cli** (`effect/unstable/cli`) | verification entry | Medium — arg/flag parsing |
| **Reactivity** | observability, fiber-local | Medium |
| **SQL** (`effect/unstable/sql`) | observability (chdb) | ★★★ large |
| **httpapi** (OpenAPI → client) | effect-s2 | ★★★ large — consider keeping TS codegen, emitting F# |

## Roadmap (prioritized for the named drivers first)

- **A. Fable tail → 0:** TxRef async-retry (cleanliness).
- **B. Driver surface:** ✅ NodeRuntime → HttpClient → cli (if verification's entry
  parses args). Confirm Tracer/Metric cover verification's OpenTelemetry use.
- **C. Acceptance spike:** rewrite **verification** + **fluent-acp-process** in F#
  against the above (the real test); gaps it surfaces drive next work.
- **D. Heavy/specialized:** SQL, httpapi, Reactivity (effect-s2/observability) — port
  or keep-TS per package.

## Verification protocol per step
`dotnet build` + targeted xUnit green · `./scripts/fable-coverage.sh` holds/drops ·
Fable-compile the touched package (Node smoke test for Node-only paths) · CI gates merge.

## Acceptance
Feature-complete = a `cutover` branch of fluent-firegrid's **verification** and
**fluent-acp-process** packages compiles + runs as idiomatic eff-sharp (Fable→Node).
effect-s2/observability follow as Phase D.

## Note on parallel work
A parallel effort is grinding the core Fable-cleanup (merged Clock #20, Formatter,
Schema, Data/Chunk stubs — 47→2 errors). Coordinate: this track owns the platform
package + cutover surface (NodeRuntime/HttpClient/cli); that track owns core Fable
cleanliness (TxRef next). Check `gh pr list` before starting a core-touching module.
