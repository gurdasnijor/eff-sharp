# Fiber core spike — evidence for the representation decision

Two prototypes of the fiber-capable `Effect` core, compared on **faithfulness
tests** (decisive) and a **BenchmarkDotNet** suite (cost). Run:
```
dotnet run -c Release --project spikes/fiber -- test    # faithfulness pass/fail
dotnet run -c Release --project spikes/fiber -- bench    # BenchmarkDotNet
```

## Faithfulness (the decisive axis)
| Test | Proto1 (Async + CancellationToken) | Proto2 (Async + explicit fiber state) |
|---|---|---|
| fork / join | ✅ | ✅ |
| interrupt → Interrupt | ✅ | ✅ |
| **finalizer runs on interrupt** | ❌ **FAIL** | ✅ |
| uninterruptible: region completes | ⚠️ partial | ✅ |
| uninterruptible: interrupt honored after | ❌ impossible | ✅ |

Proto1 fails because **F# `Async` cancellation unwinds via the cancellation
continuation and bypasses `try/with`/`finally`** — finalizers don't run on
interrupt (resource leak), and the masked body can't observe the outer token.

## Benchmark (BenchmarkDotNet, ShortRun, net10.0)
| Benchmark | Proto1 | Proto2 | Proto2 / Proto1 |
|---|---|---|---|
| Bind throughput, N=10,000 | 774 µs · 3.98 MB | 909 µs · 4.66 MB | 1.17× time · 1.17× alloc |
| Bind throughput, N=100,000 | 8.54 ms · 39.7 MB | 9.63 ms · 46.6 MB | 1.13× time · 1.17× alloc |
| Deep recursion, depth=50,000 | 4.22 ms | 4.41 ms | 1.05× (both stack-safe) |
| Fork/join ×1,000 | 13.6 ms | 12.5 ms | 0.91× (wash, high variance) |

## Decision: Proto2 (the hybrid)
Keep `Async` for execution / stack-safety / scheduling; add an explicit `Fiber`
state (interrupt flag + mask counter + scope) threaded through and checked at
yield points.

- Chosen **for correctness, not speed.** Proto2 is ~13–17% slower on pure-bind
  microbenchmarks and allocates ~17% more — but Proto1 is disqualified (loses
  finalizers on interrupt; can't mask). Faithful resource safety is non-negotiable.
- The overhead is **largely recoverable**: Proto2 checks the interrupt flag on
  every `bind`; moving the check to yield points only (as Effect/ZIO do) would
  close most of the gap. Tracked as a kernel tuning lever.
- It is an **evolution** of the current `Reader<'R, Async<Exit>>` core (→ thread a
  `Fiber` alongside `'R`), not a from-scratch reified interpreter — so the public
  API stays and the 44 merged modules survive (only `Deferred.await`/`Latch`/`Scope`,
  which reach into the representation, get updated).

(Earlier note: a crude Stopwatch reading wrongly suggested Proto2 was *faster*;
the BenchmarkDotNet numbers above are authoritative.)
