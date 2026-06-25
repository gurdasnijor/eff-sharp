# eff-sharp → Fable → Node feasibility (P0 spike)

**Verdict: GO (conditional).** The core Effect runtime — the part everyone feared
would need a single-threaded cooperative-scheduler rewrite — **compiles to JS via
Fable and runs correctly on Node today**, including `fork`/`join`. The work that
remains is a bounded, mostly-mechanical port of ~31 leaf/concurrency files, not an
architectural rewrite. There is **one genuinely fundamental** item (STM blocking
`retry` via `Monitor.Wait`) and **one hard** item (reflection-based
`Formatter`/`Schema` introspection), both of which are *leaves*, not the core.

Date: 2026-06-24 · Fable 5.4.0 · .NET SDK 10.0.301 · Node v24.14.1 ·
branch `spike/fable-node` · all work under `fable-spike/` (real `src/Effect` untouched).

---

## TL;DR for the impatient

| Question | Answer | Evidence |
|---|---|---|
| Does the `Async`-based Effect core compile to JS? | **Yes** | `fable-spike/out/` builds clean |
| Does `Effect.succeed 1 \|> map (+1) \|> runSync` print `2` on Node? | **Yes** | `node-run-output.txt` test A |
| Does `fail`/`catchAll`, `flatMap` chains, `Ref` work? | **Yes** | tests C, D, E |
| Does the **fiber model** (`fork`/`join`) run on Node's single thread? | **Yes** | tests F, G |
| Was a cooperative-scheduler rewrite of the core required? | **No** | `Async.StartChild` is the fiber primitive; core ran unmodified except 3 shims |
| Is `lock` (24 files — the brief's top worry) a blocker? | **No** | compiles as a no-op; 0 errors |
| Are `Volatile` / `CancellationToken` blockers? | **No** | compile clean; 0 errors |
| Total Fable errors across all 98 files? | **114 errors in 31 files** | `full-tree-fable-diagnostics.txt` |
| Worst blocker? | **STM blocking-retry** (`Monitor.Wait`/`PulseAll` in `TxRef.fs`) | needs async suspend/resume redesign |

The single highest-leverage refactor is **`Deferred`** (replace
`TaskCompletionSource` with a JS-native completion): it unblocks
`Latch`, `Semaphore`, `PartitionedSemaphore`, `PubSub`, `Queue`, `Cache`, `Pool`,
and more in one stroke.

---

## What was actually built & run

`fable-spike/` contains a reproducible spike:

- **`EffectSpike.fsproj`** — smallest viable slice: `Cause`, `Exit`, `Context`,
  `Effect`, `Scope`, `Fiber`, `Runtime`, `MutableRef`, `Ref`. Clean source files
  are referenced *unmodified* by path from `../src/Effect`. The three files needing
  Fable shims are spike-local copies in **`patched/`**, every change tagged `// SPIKE:`.
- **`Program.fs`** — 7 end-to-end tests, run through both `runSync` and the async
  (`runExit`) path.
- **`full/EffectFull.fsproj`** — all 98 core files, compiled to harvest the *total*
  diagnostic surface (`full-tree-fable-diagnostics.txt`).

Reproduce:
```bash
export DOTNET_ROOT=/usr/local/share/dotnet; export PATH="$PATH:$DOTNET_ROOT"
cd fable-spike && dotnet fable EffectSpike.fsproj --lang javascript -o out
node out/Program.js          # 7 passed, 0 failed
# full scope probe:
cd full && dotnet fable EffectFull.fsproj --lang javascript -o out   # 114 errors, 31 files
```

**Node output (`fable-spike/node-run-output.txt`):**
```
  PASS  A.runSync (succeed |> map (+1))  ->  2
  PASS  B.runExit (succeed |> map (+1))  ->  2
  PASS  C.fail |> catchAll  ->  4
  PASS  D.flatMap chain (10*2+1)  ->  21
  PASS  E.Ref get/update ((0+5)*2)  ->  10
  PASS  F.fork |> join (21*2)  ->  42
  PASS  G.two fibers join (1+2)  ->  3
  ==== RESULTS: 7 passed, 0 failed ====
```

### The three shims the core slice needed (all in `fable-spike/patched/`)

These are the only changes required to make the **core** compile + run. They are
small and the report classifies each:

1. **`Effect.runSync` / `Runtime.runSync`** — `Async.RunSynchronously` cannot exist
   on single-threaded JS (nothing can block the event loop waiting on a suspended
   async). Replaced with a non-blocking driver: `Async.StartImmediate` into a result
   cell; if the effect is fully synchronous the cell is populated immediately, else
   it raises. **This is exactly Effect-TS's `runSync` contract** ("dies if the effect
   suspends"). *severity: refactor (JS-specific runner).*
2. **The fiber type & `fork`/`await`/`join`/`interrupt`** — originally `Fiber.Task:
   Task<Exit>` populated by `Async.StartAsTask` and awaited with `Async.AwaitTask`.
   **Neither Task-interop function is implemented by Fable's runtime library** (it
   ships `startAsPromise`/`awaitPromise`/`startChild`, not the Task variants — verified
   by inspecting `fable-library-js/Async.js`). Switched the fiber handle to
   `Async<Exit>` produced by **`Async.StartChild`** — the idiomatic, Task-free fiber
   primitive. fork/join then run on Node's event loop unchanged. *severity: refactor
   (representation tweak, no architecture change).*
3. **`Effect.sleep` (`Stopwatch`)**, **`unwrap` (`AggregateException`)** — trivial:
   counter instead of `Stopwatch`; identity instead of the AggregateException
   type-test (no such wrapping exists on JS). *severity: trivial.* Also `Fiber.poll`
   (`Task.IsCompleted`/`Result`) stubbed — needs a completion cell on the fiber.

> **Key takeaway:** the feared rewrite did not materialize. F# `Async` *is* Fable's
> cooperative single-threaded scheduler. `Async.StartChild` *is* the fiber spawn.
> The core ported with three small shims and ran green.

---

## Incompatibility inventory (every Fable-incompatible API encountered)

From the full-tree compile (`full-tree-fable-diagnostics.txt`): **114 errors across
31 of 98 files**. 67 files compile with zero errors. Grouped by root cause:

| API family | Where (modules) | Severity | Fix |
|---|---|---|---|
| **`TaskCompletionSource`** (`.ctor`/`get_Task`/`Try?SetResult`) — 27 hits | Deferred, Semaphore, Latch, PartitionedSemaphore, PubSub | **refactor** | Replace with JS-native completion (`Async.FromContinuations` / resolve-cell). Keystone fix — see Deferred below. |
| **`Async.RunSynchronously`** — 3 | Effect, Runtime, ManagedRuntime | **refactor** | Non-blocking sync driver (proven in spike). |
| **Task interop** `StartAsTask`/`AwaitTask`/`IsCompleted`/`Result`/`Delay`/`CompletedTask` | Effect, Fiber, FiberHandle/Map/Set, Clock, Scheduler | **refactor** | `Async.StartChild` for fork (proven); `Async.Sleep`/`setTimeout` for delays; completion cell for poll. |
| **`Monitor.Wait` / `Monitor.PulseAll`** — 2 | **TxRef** | **fundamental** | STM blocking `retry` — see below. |
| **Reflection**: `System.Type.GetProperty/GetProperties/GetMethod/GetTypeCode/IsAssignableFrom`, `PropertyInfo.*`, `MemberInfo.DeclaringType`, "cannot get type info of generic param" | **Formatter** (17), Schema, Chunk, Pull, Data | **fundamental** (for the reflective parts) | Fable erases generics at runtime. Needs compile-time/`inline` derivation or explicit per-type codecs. Leaves, not core. |
| **BCL collections**: `LinkedList`, `PriorityQueue` | Semaphore, PartitionedSemaphore, Scheduler | **refactor** | Swap for arrays / a small JS-friendly queue. |
| **Platform I/O**: `System.Console.*`, `TextWriter/TextReader`, `Directory.GetCurrentDirectory` | Console, Terminal, Path | **refactor** | Provide a JS platform layer (`process.stdout`/`stdin`). Expected for any Fable app. |
| **`SemaphoreSlim`** (`.ctor`/`WaitAsync`/`Release`) | SynchronizedRef, Semaphore | **refactor** | Rebuild on the ported `Deferred`; single-threaded JS needs no real semaphore. |
| **`Interlocked.Increment`** — 1 | TxReentrantLock | **trivial** | Plain `n+1` — atomicity is free on single-threaded JS. |
| **Numeric statics**: `Double.IsInteger/IsFinite/IsNegative`, `Double.ToString(roundtrip)` | Duration, BigDecimal, JsonSchema, Schema, Cron | **trivial** | One-line shims (`Number.isInteger`, `isFinite`, …). |
| **Encoding**: `Convert.ToHexStringLower/FromHexString` | Encoding | **trivial** | Small hex helpers. |
| **Unicode**: `String.EnumerateRunes`, `Rune`, `StringRuneEnumerator` | Schema, Cron | **refactor** | JS string iteration / code-point APIs. |
| **`Random.Shared`** | Random | **trivial** | JS RNG seed source. |

### Confirmed NON-issues (the brief's top worries — all resolved ✅)

| Worried API | Files | Result |
|---|---|---|
| **`lock`** | **24** | **Compiles clean (0 errors).** Fable lowers it to a passthrough; semantically a safe no-op on single-threaded JS. This was the single biggest risk and it is a non-issue. |
| **`Volatile`** | 1 | Compiles clean. |
| **`CancellationToken`** | 1 | Compiles clean (Fable's Async ships cancellation tokens). |
| **`Async` (the whole core)** | ~10 | Compiles **and runs** on Node. |

---

## Per-subsystem verdict

| Subsystem | Compiles & runs on Node today? | Notes |
|---|---|---|
| **Core Effect** (succeed/map/flatMap/fail/catchAll/ensuring/acquireUseRelease/CE) | ✅ **Yes** (with 3 spike shims) | Proven green. `runSync` = sync-only driver, matching Effect-TS. |
| **Ref / MutableRef** | ✅ **Yes** | Clean; proven (test E). |
| **Fiber** (`fork`/`join`/`await`/`interrupt`) | ✅ **Yes** | via `Async.StartChild` (proven F, G). `poll` needs a completion cell (refactor). `FiberHandle/Map/Set` same poll fix. |
| **Scope / finalizers** | ✅ **Yes** | `lock`-based gate compiles; included in slice. |
| **Layer / Runtime** | ◐ **Mostly** | `runSync`/`runPromise` need the JS runners (done in spike); rest is plain DI. |
| **Deferred / Latch / Semaphore / PubSub / Queue** | ✗ **Needs work** | Blocked on `TaskCompletionSource`. **All unblocked by one `Deferred` refactor.** |
| **STM** (TxRef, TxChunk/HashMap/Queue/…) | ◐ **Split** | Optimistic transactions are pure F# (fine). **Blocking `retry` (`Monitor.Wait`/`PulseAll`) is fundamental** — must become async suspend/resume. `Interlocked`/`Volatile` parts are trivial. |
| **Stream / Sink / Channel / Pull** | ◐ **Likely yes after deps** | Built on Async + Effect; `Pull` has one reflection hit. Re-test once Deferred lands. |
| **Schema** | ✗ **Hard** | Reflection-based introspection + Unicode + numeric statics. The reflective derivation is fundamental on Fable. |
| **Formatter** (pretty-print/inspect) | ✗ **Hard** | 17 errors, almost all runtime reflection. Rewrite to non-reflective rendering, or drop on JS. Pure leaf. |
| **Clock / Scheduler** | ✗ **Needs work** | `Task.Delay`→`Async.Sleep`/`setTimeout`; `PriorityQueue`→array heap. Refactor. |
| **Platform** (Console, Terminal, Path) | ✗ **Needs platform layer** | Expected: provide `process`-backed implementations. |
| **Leaves** (HashMap, HashSet, Chunk, Duration, Data, Order, Equal, BigInt, Cron, Encoding, …) | ✅ **Mostly clean** | 67/98 files compile clean; the rest are trivial numeric/encoding shims (Chunk/Data each have a single reflection hit to localize). |

---

## The one fundamental blocker, stated plainly

**STM blocking `retry` in `TxRef.fs` (`Monitor.Wait` / `Monitor.PulseAll`).**
On .NET a transaction that must wait for a condition parks a thread on a monitor and
is pulsed on commit. **On single-threaded JS you cannot park — blocking the event
loop deadlocks the program.** The fix is a real (but localized) redesign: a retrying
transaction must *suspend its fiber asynchronously* (register a waiter, return control
to the event loop) and be *resumed* when a written `TxRef` it read commits — i.e. the
same async wake-up machinery the `Deferred` refactor introduces. Optimistic,
non-blocking transactions (the common case) are unaffected and port for free. Scope:
moderate, isolated to the STM engine, and reusing the Deferred primitive.

The reflection blocker (`Formatter`/`Schema`) is the other "hard" item but it is
**not on the critical path** for `fluent-firegrid`'s core effect usage — it's
pretty-printing and codec introspection on leaves. It can be stubbed/simplified for
the JS target and revisited.

---

## Recommended phased path

**Phase 0 — lock in the win (done here).** Core + Ref + Fiber compile and run on Node.
Keep `fable-spike/` as the regression harness.

**Phase 1 — make the shims real, in `src/Effect`, behind Fable conditionals.**
Apply the three proven core shims (`runSync` driver, `StartChild` fibers, `sleep`)
to the real files using `#if FABLE_COMPILER` so .NET behavior is untouched. Add the
trivial numeric/encoding/`Interlocked` shims. *Est: 1–2 days.*

**Phase 2 — the `Deferred` keystone.** Reimplement `Deferred` without
`TaskCompletionSource` (a completion cell + `Async.FromContinuations` waiters). Rebuild
`Latch`, `Semaphore`, `PartitionedSemaphore`, `PubSub`, `Queue` on it. Swap
`LinkedList`/`PriorityQueue` for arrays. This clears the largest error cluster (≈40
errors) at once. *Est: 3–5 days.*

**Phase 3 — Clock/Scheduler + platform layer.** `Async.Sleep`/`setTimeout`, array
heap; `process`-backed Console/Terminal/Path. *Est: 2–3 days.*

**Phase 4 — STM async-retry redesign.** Rework `TxRef` blocking `retry` onto the
Phase-2 async wake-up machinery. *Est: 3–5 days.*

**Phase 5 — Schema/Formatter.** Decide: non-reflective rewrite vs. JS-target stubs.
Likely the longest tail; not required for an initial `fluent-firegrid` integration if
that path doesn't lean on reflective schema derivation. *Est: variable.*

**Order of value for `fluent-firegrid`:** Phases 1–3 deliver a usable
Effect/Ref/Fiber/Layer/Stream surface on Node. That is enough to begin replacing the
`effect` npm dependency for the common case; STM and Schema follow.

---

## Honest caveats (not buried)

- The spike proves **functional correctness on small cases**, not performance,
  GC behavior, deep stack-safety under heavy recursion, or interruption/finalizer
  semantics under real concurrency. Those need their own validation once Phase 2 lands.
- `runSync` on JS is **sync-effects-only** by nature (it raises on suspension). Any
  `fluent-firegrid` code calling `runSync` on an async effect must move to
  `runPromise`. This is the same constraint Effect-TS imposes — not a regression, but
  a porting rule to enforce.
- `Async.StartChild` fiber semantics were validated for spawn + single join. Multi-join,
  interruption races, and supervision need dedicated tests before relying on them.
- Reflection-based code (`Formatter`, parts of `Schema`) will **not** be a quick port;
  budget it separately and consider JS-target stubs first.
