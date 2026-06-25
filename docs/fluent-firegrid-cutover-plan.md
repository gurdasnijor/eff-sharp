# fluent-firegrid → eff-sharp cutover: closing plan

Execution plan to (1) close the remaining eff-sharp gaps and (2) stand up an
`eff-sharp` cutover branch of fluent-firegrid.

**Convergence (= "solid base state"):** a clean working branch of fluent-firegrid in
which at least one real package is rewritten in **idiomatic** eff-sharp F#,
Fable-compiles to ESM, passes that package's (ported) tests on Node, and is opened as
a PR on `gurdasnijor/fluent-firegrid`.

---

## Where we are (2026-06-25)

- eff-sharp Fable state: **2 errors** (only `TxRef` STM `Monitor.Wait`/`PulseAll`).
- Driver API surface present + Fable-clean: Effect/Stream/Sink/Layer/Context/Ref/
  Option/Fiber/Queue/Semaphore/Schedule/Duration/Scope/Schema, Clock, FileSystem,
  ChildProcess(Spawner), NodeStream/NodeSink, **NodeRuntime**, **HttpClient**,
  Tracer/Metric, Console. Recent combinators: `Effect.exit`, `Scope.forkScoped`.
- fluent-firegrid: pure TS **pnpm + turbo** monorepo (ESM, `tsc`/`vitest`), no
  F#/dotnet/Fable yet, eff-sharp not referenced.

---

## Part 1 — Close the remaining eff-sharp gaps

Ordered by what unblocks the POC, then breadth. Each is its own CI-gated PR.

### 1a. Combinators the driver ports need (small, mechanical)
| Gap | Where | Approach |
|---|---|---|
| `Stream.fromQueue` | Stream.fs | emit until shutdown: `repeatEffectOption` over a `Queue.take`-or-`None` pull |
| `Queue.offerUnsafe` | Queue.fs | synchronous enqueue for unbounded queues (callback producers) |
| `Effect.withSpan` / `Effect.fn` | Effect.fs/Tracer | wrap an effect in a `Tracer` span; `fn` = named-effect helper. Tracer exists; add the combinators |
| `ChildProcess` `KillOptions` | ChildProcess/Spawner | `kill(signal, ?forceKillAfter)`; thread `killSignal` to `proc.Kill`/`child.kill(sig)` |

### 1b. Web-stream interop (for fluent-acp-process; heavier)
| Gap | Approach |
|---|---|
| `Stream.toReadableStream` | Effect `Stream<byte[]>` → WHATWG `ReadableStream` (Fable `[<Emit>]` building a `ReadableStream` whose `pull` runs the stream). Node-only. |
| `Sink`/`WritableStream` bridge | already have `NodeSink.fromWritable`; add a WHATWG `WritableStream` → `Sink`/`Queue` adapter |
| `@agentclientprotocol/sdk` binding | Fable `[<Import>]`/`[<Emit>]` of `acp.ndJsonStream`, `acp.Stream` (external JS dep) |

### 1c. Fable tail → 0 (cleanliness, NOT driver-blocking)
- `TxRef` async-retry: redesign blocking `retry` (`Monitor.Wait`/`PulseAll`) as async
  suspend/resume on the `Cell` wake machinery, `#if FABLE_COMPILER` (keep `Monitor`
  on the non-Fable fallback). Not used by fluent-firegrid.

### 1d. Larger modules (per target package; defer until needed)
- `cli` (`Argument`/`Command`/`Flag`) — verification's `CliApp.ts` entry. Scope to its
  actual usage (`string`, `withDescription`, `withDefault`, subcommands).
- `SQL`, `httpapi` (OpenAPI client) — observability/effect-s2, Phase D. Heavy; decide
  port-vs-keep-TS per package.

---

## Part 2 — The fluent-firegrid eff-sharp branch

### 2.0 Distribution: how firegrid consumes eff-sharp
eff-sharp is unpublished. The firegrid F# package references eff-sharp **source by
path** and Fable-compiles the whole graph.
- **Mechanism:** add eff-sharp as a **git submodule** (or sibling checkout) under
  firegrid; the F# package's `.fsproj` references `../<eff-sharp>/src/Effect/*.fs` +
  `Fable.Core`. Revisit publishing a Fable npm artifact once stable.

### 2.1 Branch + package layout
- Branch `eff-sharp-cutover` on fluent-firegrid.
- New sibling package per ported target, suffixed `-fs`, so the TS original stays for
  A/B until parity: e.g. `packages/fluent-acp-process-fs/`.
  ```
  packages/fluent-acp-process-fs/
    FluentAcpProcess.fsproj      # references eff-sharp src + Fable.Core
    src/*.fs                     # the idiomatic eff-sharp rewrite
    dist/*.js                    # dotnet fable output (ESM) — package main
    package.json                 # "type":"module", main -> dist/index.js, fable build script
    test/*.test.ts | *Spec.fs    # Vitest over Fable output on Node
  ```
- `package.json` `build`: `dotnet fable FluentAcpProcess.fsproj --lang javascript -o dist`.
  Wire into `turbo.json` as a build task (it shells `dotnet fable`).

### 2.2 Idiomatic / ergonomic eff-sharp (the bar for "solid")
- **`effect { }` CE** for sequencing (not raw `flatMap` chains).
- **Typed errors** via `Data`/`TaggedError`-style DUs (mirror `AcpProcessError`).
- **`Match` → native F# `match`** (cleaner than the TS `Match` pipeline).
- **Layer composition + `NodeRuntime`** at the edge; services via `Context.Tag`.
- **Schema** for any codecs; **Stream/Sink** for stdio; explicit `Scope` where TS used
  ambient scope.
- One module per file, `[<RequireQualifiedAccess>]`, doc-commented — same conventions
  as eff-sharp core (CONVENTIONS.md).

### 2.3 Phased rollout
- **Phase 1 — POC (the convergence milestone).** Port the smallest clean slice:
  `resolve-agent` (pure `Match`) + a **spawn-and-capture** demo (resolve a command,
  spawn via `ChildProcessSpawner`/`NodeRuntime`, `Stream.runCollect` stdout, assert).
  Fable-compile, run on Node, port the existing vitest as the gate. **Open the PR.**
  Proves toolchain + platform stack end-to-end. Needs nothing from Part 1.
- **Phase 2 — first full driver.** `fluent-acp-process` (needs 1b web-stream interop +
  `Stream.fromQueue`/`Queue.offerUnsafe`/already-have `forkScoped`) **or**
  `verification/ProcessHost` (needs `Effect.withSpan`/`fn` + `ChildProcess KillOptions`
  + `cli`). Pick whichever's interop is less risky; recommend **ProcessHost** (no
  external SDK, just tracing + kill-options + cli) once 1a lands.
- **Phase 3 — remaining drivers**, then effect-s2/observability (Phase D: SQL/httpapi).

### 2.4 Verification per ported package
- Fable-compiles clean; F#-authored Vitest specs are green on Node; behavior
  matches the TS original (A/B).

---

## Part 3 — Convergence criteria ("solid base state")

1. eff-sharp: 0 Fable errors (Part 1c done) OR all *firegrid-used* modules Fable-clean.
2. fluent-firegrid `eff-sharp-cutover` branch: **≥1 real package rewritten idiomatically**
   in eff-sharp, Fable→ESM, tests green on Node, opened as a PR.
3. The rewrite reads as natural F#/Effect (CE, typed errors, Layers) — not a literal
   TS transliteration.

Phase 1 satisfies the *minimum* bar (a clean working PR). Phase 2 makes it *solid* (a
real driver, not a toy).

---

## Part 4 — Risks / decisions to make

- **eff-sharp distribution** (submodule-by-source vs published Fable npm). Start
  submodule-by-source; publish later. *Biggest integration decision.*
- **Perf:** the `Async`-based core is ~35–100× slower than stock Effect on *sync*
  throughput (see `docs/future-interpreter-loop.md`). fluent-firegrid's drivers are
  **I/O-bound** (process spawn, HTTP, file I/O) where eff-sharp is competitive, so this
  is acceptable for the POC; the interpreter-loop rewrite is a separate future track.
- **Schema reflection** is limited under Fable — effect-s2's codegen path (httpapi)
  may stay TS or need non-reflective derivation.
- **External JS interop** (acp SDK, web streams, chdb) is per-package Fable-binding
  work — the main unknown for the heavy packages.
