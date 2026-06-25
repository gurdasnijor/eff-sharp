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
Feature-complete = a **POC cutover branch of fluent-firegrid, opened as a PR there**,
rewriting a package in idiomatic eff-sharp F# (Fable→Node, best practices).
effect-s2/observability follow as Phase D.

## POC execution plan (read the actual source — done 2026-06-25)

Two candidate POC modules, with their EXACT remaining eff-sharp gaps (so the POC
isn't blocked by rediscovery):

**`fluent-acp-process`** (4 src files, spawn an ACP harness → expose `acp.Stream`):
- Pure: `resolve-agent.ts` (Match on agent key) → trivial F# `match`.
- Hard: `process-owner.ts` bridges eff-sharp `Stream`↔WHATWG web streams and calls
  the external `@agentclientprotocol/sdk` `ndJsonStream(writable, readable)`. Gaps:
  `Stream.fromQueue`, `Queue.offerUnsafe`, `Effect.forkScoped`,
  **`Stream.toReadableStream`** (Effect Stream → WHATWG `ReadableStream`, Fable
  interop) + a Fable binding for `@agentclientprotocol/sdk`. Heavy JS interop.

**`verification/ProcessHost.ts`** (283 lines, spawn hosts + HTTP readiness + kill/
restart + tracing): uses ChildProcess ✓, HttpClient ✓, Ref ✓, Option ✓, Scope ✓,
`Effect.sleep` ✓, `Effect.exit` ✓ (added). Gaps: `Effect.forkScoped`,
`Effect.withSpan`/`Effect.fn` (tracing — eff-sharp has `Tracer`, needs the combinators),
`ChildProcess` `KillOptions` (`killSignal`/`forceKillAfter`), plus `cli` for the full
`CliApp.ts` entry.

**Recommended minimal POC** (proves the toolchain + platform stack without the
web-stream/SDK or tracing tail): port `resolve-agent` + a `spawn-and-capture` slice —
resolve an agent command, spawn it via `ChildProcessSpawner`/`NodeRuntime`, capture
stdout via `Stream.runCollect`, assert output — Fable-compiled, run on Node, opened as
a PR on fluent-firegrid (new package, e.g. `packages/fluent-acp-process-fs/`).
Integration steps: F# project referencing eff-sharp (project ref or built lib) →
`dotnet fable` → Node smoke test → PR.

**Combinators still to add for a fuller POC** (priority order): `Effect.forkScoped`
(Scope.fs, after Effect), `Stream.fromQueue` (on `repeatEffectOption` + `Queue.take`),
`Queue.offerUnsafe`, `Effect.withSpan`/`Effect.fn` (Tracer), `ChildProcess` `KillOptions`.

## Note on parallel work
A parallel effort is grinding the core Fable-cleanup (merged Clock #20, Formatter,
Schema, Data/Chunk stubs — 47→2 errors). Coordinate: this track owns the platform
package + cutover surface (NodeRuntime/HttpClient/cli); that track owns core Fable
cleanliness (TxRef next). Check `gh pr list` before starting a core-touching module.
