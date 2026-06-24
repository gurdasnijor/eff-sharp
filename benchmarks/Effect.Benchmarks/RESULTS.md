# Benchmark results

Infra: [BenchmarkDotNet](https://benchmarkdotnet.org) — the idiomatic .NET
micro-benchmark harness. Each benchmark pits the **current port implementation**
against the **native FSharp.Core/.NET alternative** the alignment review proposed,
so the Tier 1/Tier 2 cleanup decisions rest on measured numbers.

## Run it

```bash
dotnet run -c Release --project benchmarks/Effect.Benchmarks -- --filter '*'
# scope to one:  -- --filter '*Dijkstra*'
```

(Generated output lands in `BenchmarkDotNet.Artifacts/`, which is gitignored.)

## Snapshot (net10.0, ShortRun job — indicative; GC noise inflates the N=10000 rows)

| Benchmark | Current | Native alt | Read |
|-----------|---------|-----------|------|
| `MutableHashSet` add+lookup (N=1000) | 8.5 μs · **71 KB** | `HashSet(Structural)` 11.4 μs · **17 KB** | native ~4× less memory, ~1.3× **slower** time |
| `Graph.dijkstra` (N=200 / N=800) | 40 μs / **198 μs** | — | 4× nodes → ~5× time; sub-quadratic on sparse graphs, sub-ms at 800 nodes |
| `Chunk.dedupe` (N=10000) | 46.6 μs | `Array.distinct` **37.6 μs** | native ~20% faster |
| `HashMap.modifyAt` (N=10000) | 124 μs | `Map.change` **107 μs** | native ~14% faster (1 pass vs 2 lookups) |

## What it told the alignment decision

- **Tier 1 (apply):** native replacements are **equal-or-faster** (`Array.distinct`
  +20%, `Map.change` +14%) with no allocation regression — cleaner *and* quicker.
- **Tier 2 (skip):** perf case is weak —
  - `MutableHashSet` → native is **memory-only** (4× less alloc, no speed win) and
    costs upstream-structure fidelity. Not worth it now; available as a lever if
    memory pressure ever matters.
  - `Graph.dijkstra` linear-scan is **sub-ms at 800 nodes** and scales
    sub-quadratically on sparse graphs; a `PriorityQueue` only pays off on very
    large/dense graphs outside our target.

Numbers are indicative (ShortRun, 3 iterations). Re-run with the default job for
publication-grade precision before relying on small deltas.
