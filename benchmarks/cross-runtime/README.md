# Cross-runtime benchmarks — upstream `effect` vs eff-sharp

Apples-to-apples Node benchmarks that measure the **performance delta** between
the upstream `effect` interpreter and eff-sharp's Reader-over-`Async` core, on
identical workloads. This is the evidence base for the "~35–100× slower on sync
throughput" claim in `docs/effect4-port-evaluation.md` and the motivation for the
interpreter-loop RFC.

## What's here

| file | role |
|------|------|
| `workloads.md` | the shared workload definitions (kept in lockstep across both sides) |
| `upstream.bench.mjs` | upstream `effect` (npm) side → `results-upstream.json` |
| `Workloads.fs` + `EffSharpBench.fsproj` | eff-sharp side (Fable → ESM) |
| `effsharp.bench.mjs` | drives the Fable output → `results-effsharp.json` |
| `run-all.mjs` | reconciles both into `RESULTS.md` (+ geo-mean slowdown) |
| `build-and-run.sh` | one-shot: build + run both + report (needs .NET SDK) |
| `results-upstream.json` | committed upstream baseline (captured numbers) |
| `RESULTS.md` | generated comparison table |

## Run it

Upstream side only (no .NET needed):

```bash
npm install
npm run bench:upstream      # writes results-upstream.json
node run-all.mjs            # regenerates RESULTS.md
```

Full cross-runtime comparison (needs the .NET SDK 10.x for Fable):

```bash
bash build-and-run.sh
```

## Methodology

- Both sides use the **async runner** (`runPromise` upstream; `runExit` bridged to
  a Promise on eff-sharp) — the realistic regime, and it avoids a `runSync`
  deep-chain suspension on the Fable side (itself a symptom of the macrotask
  trampoline; see the RFC).
- Programs are **built once** outside the timed loop, so we measure interpreter
  *run* cost, not construction.
- `tinybench` with 250 ms warmup + 1 s measurement per workload; the reported
  `±%` is relative margin of error.

## Caveat on "upstream"

The installed upstream is `effect@3.21.4` — the latest published `effect`, used as
the closest installable proxy for effect-smol / Effect 4. Both share the same
defunctionalized fiber-interpreter architecture, so the order-of-magnitude delta
vs a Reader-over-`Async` core is representative. Treat the absolute upstream
numbers as "this *class* of runtime", not a pinned effect-smol build. When
effect-smol publishes, swap the dependency and re-capture.

## Why this regime is the worst case for eff-sharp

Fully-synchronous chains are exactly where a Reader-over-`Async` core pays the
most: every `flatMap` boundary allocates an `Async` continuation and threads it
through Fable's CPS trampoline, which **yields to the macrotask queue
(`setTimeout(0)`) every 2000 binds** — whereas the upstream interpreter steps its
op-tree synchronously and yields on the **microtask** queue only when it chooses
to. The `deep_bind` workload (5000 binds) is designed to cross that boundary.
I/O-bound workloads (HTTP, fs, spawn) close the gap because the dominant cost is
the syscall, not the interpreter — which is why the fluent-firegrid cutover is
viable on the current core even before the interpreter rewrite.
