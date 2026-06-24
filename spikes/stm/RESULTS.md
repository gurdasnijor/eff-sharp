# STM spike — TxRef v1 results

Standalone F# spike (`~/gurdasnijor/eff-sharp-spike-stm`, not part of eff-sharp).
Version-based optimistic STM — the same algorithm as Effect `TxRef.ts`
(`version` + pending journal) and FSharpx `Stm.fs` (TVar + TLog + `IsValid` +
`Monitor.Wait/PulseAll`).

## What was built
- `Stm.fs` — `TVar<'a>`, the `stm { }` builder (Bind/Return/Zero/Delay/Combine),
  `newTVar`/`readTVar`/`writeTVar`, `atomically`, `retry`, `orElse`, plus the
  TxRef-parity helpers `modify`/`update`/`get`/`set`.
- `Program.fs` — 7 assertions (assert-in-main) + a manual contention probe.
- `Benchmarks.fs` — BenchmarkDotNet contention benchmark vs a `lock` baseline.

## Test outcomes — 7/7 PASS
| Test | Proves |
|------|--------|
| single-tx increment -> 1 | basic read-modify-write commits |
| modify returns result + stores new value | TxRef `modify` semantics |
| aborted tx leaves no partial writes | **all-or-nothing** (orElse rolls back a retried branch's 2 writes) |
| committed multi-write applies all | multi-TVar commit is atomic |
| concurrent increments lose nothing (8 threads × 5000 == 40000) | **conflict → validate-fail → retry → success**, no lost updates |
| waiter blocked before the write | `retry` genuinely blocks (Monitor.Wait) |
| waiter unblocked + committed after write | a committing write `PulseAll`s the waiter, which then completes |

### Correctness bug found & fixed during the spike (the main finding)
The first run **lost an update** (39999/40000). Root cause: transaction bodies
read TVars *without* the global lock (the whole point of optimism), and on a
weak-memory CPU (Apple Silicon arm64) a naive read can pair a **fresh `version`
with a stale `value`** — validation then passes against the wrong base value and
an increment vanishes. Fix: a **seqlock** — `version` is a `[<VolatileField>]`
written *after* `value` on commit, and reads re-sample until the version is
stable around the value snapshot (`TVar.ReadConsistent`). After the fix, 40000 is
hit on every run (verified ×3). **Lesson: an optimistic STM port must specify its
memory model explicitly; a direct transliteration of the JS/single-threaded
algorithm is unsafe under real .NET parallelism.**

## Benchmark — contention on ONE shared counter (worst case for STM)

### Manual probe (Stopwatch, `threads × 20000` increments, this run)
| threads | STM ms | lock ms | STM / lock |
|---------|--------|---------|------------|
| 2 | ~30 | ~0.4 | ~70–80× |
| 4 | ~30–55 | ~0.6 | ~50–70× |
| 8 | ~125 | ~1.3 | ~95× |

### BenchmarkDotNet
<!-- BDN_TABLE -->
_(filled in below once the BenchmarkDotNet run completes)_

Interpretation is unaffected by the exact figures: under **maximum contention on
a single TVar**, optimistic STM is ~1–2 orders of magnitude slower than a plain
`lock`. That is expected and not a defect — with one hotspot, every concurrent
transaction but the winner fails validation and re-runs, so the optimism is pure
wasted work, while a `lock` simply serializes cheaply. STM's payoff is the
*opposite* workload: many TVars with mostly-disjoint access, where a single lock
would needlessly serialize unrelated transactions and STM lets them commit in
parallel. This benchmark deliberately measures the regime where STM looks worst.

## 5-line read on viability
1. **Viable and correct** in F#: the version-based optimistic algorithm ports
   cleanly; all atomicity / retry / blocking semantics work in ~200 lines.
2. **Not a `lock` replacement for single-hotspot counters** — it is 50–100× slower
   there; reach for it only when transactions touch *multiple, mostly-disjoint*
   TVars and composability (`orElse`, atomic multi-ref commit) matters.
3. **Memory model is the real gotcha**: needs the seqlock/volatile fix above;
   without it you silently lose writes on arm64. This must be in the v1 design.
4. **v1 thread-blocks on `retry`** (`Monitor.Wait`) — fine for a spike, but it
   parks an OS thread per blocked tx; the real port must suspend a *fiber*
   instead once the fiber runtime exists.
5. **Other risks**: livelock/starvation under high contention (no backoff or
   fairness — a slow tx can be perpetually invalidated), and per-tx allocation
   (a `Dictionary` journal + boxed entries per `atomically`). Both are acceptable
   for v1 but should be on the v2 list (backoff, struct journal, unboxed entries).
