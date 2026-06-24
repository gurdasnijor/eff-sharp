# Wave-4 cross-runtime benchmark — eff-sharp (F#/.NET) vs stock Effect (TS/Node)

Directional performance evidence: does our native F# port land in the same
ballpark as published Effect? Four identical workloads, run on each runtime.

- **eff-sharp**: `benchmarks/Effect.Benchmarks/RuntimeBenchmarks.fs`, BenchmarkDotNet
  0.14.0, `-c Release`, .NET 10.0.9 RyuJIT Arm64 (12 iterations / 5 warmup).
- **stock Effect**: `js-bench/bench.mjs`, `effect@4.0.0-beta.87` (same version as the
  porting reference repo) + tinybench 6.0.2, Node v24.14.1 (V8).
- Hardware: Apple M4, 10 cores, macOS 26.3.1 (both runtimes, same machine).

## ⚠️ Read this caveat first
This is **.NET (RyuJIT) vs V8 — two different VMs, GCs, and JITs**. The ratios are
**directional** ("same ballpark?" / "where are we bleeding?"), **not** a like-for-like
comparison of the two effect systems in isolation. A 2× difference here could be the
runtime, not the library. Treat trends, not decimals. The **fork/join** row in
particular compares semantically different things (see notes) and should not be read
as a win.

## Workloads (identical N on both sides)
| Workload | What it does | N |
|---|---|---|
| bind throughput | left-nested chain of `flatMap`, run once | 10,000 |
| succeed/map | chain of `map` over `succeed`, run once | 10,000 |
| Ref update/get | `Ref.update` + `Ref.get` pairs in one effect | 10,000 |
| fork/join | fork a trivial fiber + join it | 1,000 |

Programs are built once and only the **run** is timed, on both sides.

## Results (per whole-program op)
| Workload | eff-sharp mean | stock Effect mean | ratio (ours ÷ stock) | verdict |
|---|---:|---:|---:|---|
| bind throughput (10k) | 913.9 µs | 448.2 µs | **2.04×** slower | competitive |
| succeed/map (10k)     | 616.8 µs | 440.3 µs | **1.40×** slower | competitive |
| Ref update/get (10k)  | 4,053.5 µs | 1,405.6 µs | **2.88×** slower | behind |
| fork/join (1k)        | 1,491.9 µs | 13,723.9 µs | **0.11× (9.2× faster)** | ⚠ unfair, see notes |

## Results (normalized per primitive operation)
| Workload | eff-sharp ns/op | stock Effect ns/op | eff-sharp ops/s | stock ops/s |
|---|---:|---:|---:|---:|
| per `flatMap` bind | 91.4 | 44.8 | 1,094 | 2,369 |
| per `map`          | 61.7 | 44.0 | 1,621 | 2,346 |
| per Ref update+get | 405.4 | 140.6 | 247 | 713 |
| per fork+join      | 1,492 | 13,724 | 670 | 73 |

eff-sharp allocations (BenchmarkDotNet MemoryDiagnoser, per whole-program op):
bind 4.74 MB · map 3.21 MB · Ref 20.88 MB · fork/join 2.54 MB.

## Reading the numbers

**Where we're competitive (bind, map): within ~1.4–2× of stock.** For a
straight-line `Async`-over-`Exit` reader competing with V8 running Effect's
hand-tuned fiber trampoline, landing inside 2× is a genuinely good result. The map
chain (1.4×) is closest because it has no yield-point bookkeeping — just an `Exit`
match per step. The bind chain (2.0×) pays our **per-`flatMap` interrupt check**
(`interruptible fib` — read two mutable fields and branch at every bind boundary,
`Effect.fs` `flatMap`) plus a fresh `async { }` state machine + `Exit` allocation per
step. Stock Effect amortizes this in one fiber loop with no allocation per bind.

**Where we're behind (Ref, 2.9×).** Each `Ref.update`/`Ref.get` is its own
`Effect.sync` thunk wrapped in its own `async { }`, and the 10k-pair program is itself
driven through 20k `flatMap` yield points — so this is really "bind throughput ×2 +
thunk overhead." The 20.88 MB allocation (4× the bind workload) confirms it: we
allocate an Async state machine + Exit per Ref op. Stock Effect's `Ref` is a direct
cell read/write inside the fiber loop with no per-op effect allocation. This is the
clearest optimization target (see below).

**fork/join — do not call this a win.** The comparison is unfair in both directions:
- *Semantics differ.* Our `Effect.fork` is `Async.StartAsTask` onto the .NET
  thread-pool; `join` awaits the `Task`. Stock `Effect.forkChild` schedules a real
  cooperative fiber on V8's microtask queue and **must** run via `runPromise` (async),
  whereas our `runSync` blocks the calling thread. We're comparing thread-pool task
  spawn vs event-loop fiber scheduling + promise round-trips.
- *Different runners.* JS pays `runPromise` + microtask latency per op (≈13.7 µs);
  ours is a synchronous `Async.RunSynchronously` over thread-pool tasks (≈1.5 µs).
  The 9× "win" is mostly the event-loop tax on the JS side, not port superiority — and
  our model will get more expensive once real fiber scheduling/interruption lands.

## Likely reasons we trail on the binding workloads
1. **Per-bind interrupt check** — every `flatMap` reads `fib.Interrupted`/`fib.Mask`
   and branches (the cooperative-interruption design). Cheap, but non-zero × 10k–20k.
2. **Allocation per step** — each combinator builds a new `async { }` state machine and
   returns a freshly allocated `Exit` (`Success`/`Failure`). Stock Effect runs a single
   fiber loop and mutates a continuation stack — far less garbage.
3. **`Async` vs a bespoke fiber loop** — F# `Async` gives us stack-safety and scheduling
   for free, but its per-bind continuation machinery is heavier than Effect's purpose-built
   evaluator.

## Suggested follow-ups (not in scope here)
- Fuse `Ref` ops into the runner (avoid an `async`+`Exit` per op) — biggest single win.
- Consider a fast-path in `flatMap` that skips the interrupt check when no fiber is
  forked (root, never-interruptible) — would directly cut the bind tax.
- Re-run fork/join with `runAsync` on the F# side too, and a thread-pool-free fiber
  scheduler, before drawing any concurrency conclusions.

## Reproduce
```bash
# F#
export DOTNET_ROOT="/opt/homebrew/opt/dotnet/libexec"; export PATH="$PATH:$DOTNET_ROOT"
dotnet run -c Release --project benchmarks/Effect.Benchmarks/Effect.Benchmarks.fsproj \
  -- --filter '*RuntimeBench*' --iterationCount 12 --warmupCount 5 --launchCount 1

# JS (stock Effect)
cd js-bench && npm install && node bench.mjs
```
