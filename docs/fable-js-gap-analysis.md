# eff-sharp → JS runtime (Node-first): full gap analysis, solutions & build-vs-adopt

**Status:** planning backbone for the remaining Fable port waves.
**Baseline:** commit `f8de296` · Fable 5.4.0 · .NET SDK 10 · Node v24 · branch `docs/fable-gap-analysis`.
**Companion docs:** [`fable-feasibility.md`](./fable-feasibility.md) (the P0 GO verdict + 3 core shims), `fable-spike/full-tree-fable-diagnostics.txt` (raw error inventory).

> This document does **not** re-argue feasibility — the spike settled that (GO). It synthesizes
> *everything still between us and a JS runtime we can actually ship under fluent-firegrid*, with a
> proposed solution and an external-package option for each gap, and an honest accounting of the part
> that compile-green does **not** buy us.

---

## 0. Executive summary — the two bars, and how far we are on each

There are two finish lines, and they are very far apart. Conflating them is the single biggest risk
to planning.

### Bar 1 — **Compile-green**: all of `src/Effect` compiles to JS via Fable, 0 errors.

**Where we are: ~93% of files, 81 errors left in 17 of 99 files.** (Live re-measure below; was 116 → 81
after PR #4 landed Rock 1.) This bar is **bounded and mostly mechanical** — no architectural rewrite
remains. Realistic estimate to zero: **~2–4 focused weeks**, dominated by the Deferred keystone and the
reflection tail. The spike already proved the scary part (the `Async`-based core) compiles *and runs*.

### Bar 2 — **Run-correct / "fully compatible"**: semantics actually hold on Node.

**Where we are: barely started.** We run **~6 hand-written end-to-end scenarios** on Node
(`fable-spike/Program.fs`) against **1205 `[<Fact>]` xUnit tests** on .NET. That is the real gap, and it
is the *longer, harder* bar:

- interruption + finalizer ordering under genuine concurrency (validated for spawn+single-join only),
- deep stack-safety under heavy recursion (untested on JS),
- GC / throughput / latency characteristics (untested),
- a Node test harness that mirrors the .NET suite (does not exist yet),
- and the platform/browser/Bun variants (not built).

**Do not let "compile-green" read as "done."** Compile-green is necessary and gets us a runnable
surface; it proves *nothing* about behavior under load. The honest split: **Bar 1 is ~70–80% done and
weeks away; Bar 2 is ~5% done and is the multi-month tail.** Every estimate in this doc is for Bar 1
unless it explicitly says otherwise.

### The demand twist that reorders everything (read this before the rocks)

fl-firegrid is the customer. We read its actual `effect`/`@effect` imports across all five packages
(`effect-s2`, `effect-s2-flow`, `fluent-acp-process`, `observability`, `verification`). The result
**does not match the rock ordering the spike implied:**

| What fl-firegrid actually leans on | eff-sharp status | Where it lands in this doc |
|---|---|---|
| `Effect`, `Layer`, `Context` (18 / 10 / 7 imports) | ✅ compile-green already | Done (Rock 1, PR #4) |
| **`Schema`** — 4 imports but **~530 generated calls** (`Struct`×52, `Union`×49, codecs) | ✗ **hardest rock** (reflection) | **Rock 5** |
| **`@effect/platform-node`** (7), **ChildProcess** (3), **FileSystem** (3) | ✗ **not ported even on .NET** | **§7 unstable** |
| **HttpApi*** (5), **Sql*** (3, chDB), **Cli*** (3), **NodeSdk/OpenTelemetry** (2) | ✗ **not ported even on .NET** | **§7 unstable** |
| `Stream` (3), `Ref` (3), `Fiber.join` (3), `Option`/`Duration`/`Queue` | ◐ Stream/Queue ride on Deferred | Rock 2 (transitively) |
| **Deferred, Semaphore, PubSub, STM/TxRef, Pool, Cache, Metric** | ✗ blocked / hard | **directly unused by fl-fg** |

**The hard call up front:** the Deferred keystone (Rock 2) is the spike's headline fix and is correct
for *core completeness* — but fl-firegrid **does not import Deferred, Semaphore, PubSub, STM, Pool, or
Cache at all.** Its critical path is **Schema (the hardest rock) + a platform/unstable layer that
doesn't exist yet in eff-sharp on any target.** So "finish compile-green" and "unblock fl-firegrid" are
*different projects*. Deferred is still required transitively (Queue/Stream sit on it) and for a complete
runtime — but if the goal is to replace the `effect` npm dep in fl-firegrid, the long pole is **Schema +
platform-node + the unstable subsystems**, not concurrency primitives. Sequencing in §9 reflects this.

---

## 1. Live error inventory (re-measured at `f8de296`)

```
dotnet fable fable-spike/full/EffectFull.fsproj --lang javascript -o fable-spike/full/out --noCache
→ 81 error FABLE: lines, across 17 of 99 files
```

| File | Errors | Root-cause family | Rock |
|---|--:|---|---|
| `Formatter.fs` | 17 | runtime reflection (`Type.GetProperty/GetProperties/GetTypeCode/IsAssignableFrom`, `PropertyInfo.*`, `MemberInfo.DeclaringType`) | **5** |
| `PartitionedSemaphore.fs` | 10 | `TaskCompletionSource` + `LinkedList` | **2** |
| `Terminal.fs` | 7 | `System.Console` I/O (`IsOutputRedirected`, `WindowWidth/Height`, `In`, `TextReader/Writer`) | **3** |
| `Deferred.fs` | 7 | `TaskCompletionSource` (`.ctor`/`Task`/`TrySetResult`/`IsCompleted`/`Result`) | **2** |
| `Latch.fs` | 6 | `TaskCompletionSource` + `Task.CompletedTask` | **2** |
| `PubSub.fs` | 5 | `TaskCompletionSource` | **2** |
| `JsonSchema.fs` | 5 | Unicode `String.EnumerateRunes` / `Rune` / `StringRuneEnumerator` | **5** |
| `Schema.fs` | 4 | `Double.IsInteger` (×2) + `Cannot get type info of generic parameter T` (×2) | **5** |
| `Scheduler.fs` | 4 | `PriorityQueue` (`.ctor`/`Enqueue`/`Dequeue`/`Count`) | **3** |
| `SynchronizedRef.fs` | 3 | `SemaphoreSlim` (`.ctor`/`WaitAsync`/`Release`) | **2** |
| `Semaphore.fs` | 3 | `TaskCompletionSource` | **2** |
| `Clock.fs` | 3 | `Task.Delay` (×2) + `Task.CompletedTask` | **3** |
| `TxRef.fs` | 2 | `Monitor.Wait` / `Monitor.PulseAll` | **4** |
| `RcMap.fs` | 2 | `Task.IsCompleted` (fiber-poll completion cell) | **2/3** |
| `Path.fs` | 1 | `Directory.GetCurrentDirectory` | **3** |
| `Data.fs` | 1 | `Cannot get type info of generic parameter a` | **5** |
| `Chunk.fs` | 1 | `Type.GetProperty` | **5** |
| **Total** | **81** | | |

By rock: **Rock 2 (Deferred keystone) ≈ 34**, **Rock 5 (reflection) ≈ 28**, **Rock 3 (platform+timing)
≈ 15**, **Rock 4 (STM) = 2**, plus RcMap's 2 (shared 2/3). Note PR #4 already cleared `Console.fs`,
`FiberHandle/Set/Map.fs`, and the trivial numeric/encoding/`Interlocked` leaves — what remains is the
genuinely structural work, not low-hanging fruit. The error count *understates* the remaining effort
because Rock 5's 28 errors are the *hardest* per-error, and Bar 2 (run-correctness) carries **zero** of
these errors yet represents most of the calendar time.

---

## 2. Rock 2 — the Deferred keystone (async completion)

**Gap.** ~34 errors across `Deferred`, `Latch`, `Semaphore`, `PartitionedSemaphore`, `PubSub`,
`SynchronizedRef`, all rooted in `TaskCompletionSource` (+ `SemaphoreSlim`, `LinkedList`). `RcMap` and
the fiber-poll path add `Task.IsCompleted`.

**Root cause.** `TaskCompletionSource<T>` is the .NET "promise you complete by hand." Fable's runtime
library does not implement it (nor `SemaphoreSlim`, nor `Task`-interop). On single-threaded JS you also
*cannot* block a thread on a semaphore — there is no thread to block.

**Proposed Fable solution.** Reimplement `Deferred` as a **completion cell + waiter list** behind
`Async.FromContinuations` (register continuation → return to event loop → resume on complete). This is
the spike's identified keystone: `Latch`, `Semaphore`, `PartitionedSemaphore`, `PubSub`, `Queue`, and
the `SynchronizedRef`/`Cache`/`Pool` consumers **all rebuild on it.** `SemaphoreSlim` collapses to a
counter + Deferred-waiter queue (no real semaphore needed on JS). `LinkedList` → array (or a tiny
ring buffer for the FIFO waiter queue). `Task.IsCompleted`/poll → a `completed: bool` flag on the cell.
Guard everything with `#if FABLE_COMPILER` so .NET keeps its `TaskCompletionSource` fast path.

**Effort (Bar 1):** **3–5 days** — it's one primitive, well-understood, and clears the single largest
error cluster. **Bar 2 add-on:** the interruption/cancellation races on Deferred waiters are exactly the
semantics the spike did *not* validate — budget a separate concurrency-test pass (see §8).

**fl-firegrid demand:** **Low–direct, High–transitive.** fl-fg imports **none** of Deferred/Semaphore/
PubSub directly. But it uses `Queue` and `Stream`, which sit on Deferred, and a correct
fork/join/interrupt story (it uses `Fiber.join`) depends on this machinery. So: not a *headline* feature
for the customer, but a structural prerequisite that can't be skipped.

**External-package option:** **None worth adopting.** This is the irreducible core of an Effect runtime;
binding to the real `effect` npm package's Deferred would mean running *two* Effect runtimes in one
process and marshalling at every boundary (see §6 — the cardinal "no"). **Hand-port. Verdict: BUILD.**

---

## 3. Rock 3 — platform layer + timing

**Gap (~15 errors + RcMap).** `Terminal` (7, `System.Console` raw I/O), `Clock` (3, `Task.Delay`/
`CompletedTask`), `Scheduler` (4, `PriorityQueue`), `Path` (1, `Directory.GetCurrentDirectory`).
`Console.fs` is already clean (PR #4).

**Root cause.** These are the BCL's host-OS surfaces — TTY, monotonic time, OS-timer scheduling, cwd —
none of which Fable maps, because they're inherently platform-specific.

**Proposed Fable solution (all small, all `#if FABLE_COMPILER`):**
- **Clock/Scheduler timing:** `Task.Delay` → `setTimeout`/`clearTimeout` (`Fable.Core.JS`, token is `int`);
  `Task.CompletedTask` → already-resolved cell. Drive the scheduler off a **single re-armable
  next-deadline timer** rather than N timers.
- **`PriorityQueue`** → a **~40-line mutable binary heap** in F# (deterministic, testable on both
  targets). Preferred over binding a JS heap — keeps one source of truth and is trivially unit-tested.
- **`Stopwatch`/elapsed** → a ~10-line wrapper over `performance.now()` (bind via
  `[<Emit("performance.now()")>]` or `importMember "perf_hooks"` — note `performance.now` is *not* in
  `Fable.Core.JS`, so it must be bound).
- **`Terminal`/`Path`** → `process.stdout`/`stdin`/`process.cwd()` via `Fable.Core.JsInterop`, behind
  `#if FABLE_COMPILER` so the existing `System.Console`/`System.IO` .NET implementation stays as the
  other branch. **This `#if`-dual-backing of one module is the seed of the §7 pattern** — Rock 3 shims a
  couple of leaves this way; §7 generalizes the *same* split to full services (`FileSystem`, `HttpClient`,
  `Command`, `SqlClient`) with two interchangeable `Layer`s.

**Effort (Bar 1):** **2–3 days.** **Bar 2:** TestClock determinism on JS needs validation — fl-fg's
tests use `@effect/vitest`'s TestClock heavily, and our `TestClock.fs` must behave identically under the
`setTimeout`-backed scheduler.

**fl-firegrid demand:** **Medium (timing) / High (real platform).** fl-fg uses `Clock`, `Duration`,
timeouts, and `Schedule` indirectly — but its *real* platform demand (`FileSystem`, `ChildProcess`,
`HttpClient`, stdout) lives in §7, not here. The `Terminal`/`Path` shims are necessary hygiene; the
heavy platform lift is the unbuilt unstable layer.

**External-package option:**
- **Timing:** **bind built-ins** (`Fable.Core.JS.setTimeout`, `perf_hooks`). Adopt — don't depend on
  `Fable.Node` (NuGet 1.6.0): it covers `path`/`os`/`url`/`buffer` only — **`fs`/`http`/`child_process`/
  `process` are explicitly not implemented.** Use `Fable.Core.JsInterop` directly for those.
- **Heap:** a JS package (`tinyqueue`) exists, but a hand-rolled F# heap is zero-dep, deterministic, and
  dual-targets cleanly. **Verdict: BUILD the heap, BIND the timers/TTY.**

---

## 4. Rock 4 — STM async-retry (the one fundamental item)

**Gap (2 errors).** `TxRef.fs`: `Monitor.Wait` / `Monitor.PulseAll`.

**Root cause.** STM's *blocking* `retry` parks a thread on a monitor and is pulsed on commit. **On
single-threaded JS there is no thread to park — `Monitor.Wait` would deadlock the event loop.** This is
the spike's lone "fundamental" item (small surface, but a genuine redesign, not a shim).

**Proposed Fable solution.** A retrying transaction must **suspend its fiber asynchronously** (register
the fiber as a waiter on the `TxRef`s it read, yield to the event loop) and be **resumed when any of
those `TxRef`s commits** — i.e. *exactly the Deferred wake-up machinery from Rock 2.* So Rock 4 is
**cheap once Rock 2 lands** and expensive before it. Optimistic, non-blocking transactions (the common
case) are pure F# and already compile — only blocking `retry` needs this.

**Effort (Bar 1):** **3–5 days, but strictly after Rock 2.** **Bar 2:** STM correctness under
contention is subtle; needs its own property tests.

**fl-firegrid demand:** **None.** fl-fg imports **no STM/TxRef** anywhere. This rock is for *core
completeness and parity with the .NET suite*, not for the customer. **It can be deferred behind §7
without blocking fl-firegrid** — the most defensible thing to push late.

**External-package option:** none (same reasoning as Rock 2 — it's core runtime). **Verdict: BUILD,
late.**

---

## 5. Rock 5 — reflection (Formatter, Schema, JsonSchema) — the long pole for fl-firegrid

**Gap (~28 errors).** `Formatter` (17), `JsonSchema` (5, Unicode `Rune`), `Schema` (4),
`Data`/`Chunk` (1 each). Two distinct problems hide here.

**Root cause.** Fable **erases generics at runtime** — `typeof<'T>` on a generic parameter resolves to
`obj`, and `System.Type.GetProperty/GetProperties/GetTypeCode/IsAssignableFrom`, `PropertyInfo.*`, and
`FSharpType.IsRecord/IsUnion` (second-arg) are unsupported. Any code that *introspects an arbitrary type
at runtime* cannot work as written. The Unicode `Rune` APIs are a separate, trivial sub-gap (JS strings
are UTF-16; iterate code points).

**This is the gap that matters most for fl-firegrid and is the hardest to close.** fl-fg generates
**~530 Schema calls** (`Struct`×52, `Union`×49, ~24 validators, codecs, `fromJsonString`) from its
OpenAPI codegen. Schema is *the* heavy dependency. The two pieces split sharply:

| Piece | Errors | Difficulty | fl-fg demand | Recommendation |
|---|--:|---|---|---|
| **`Formatter`** (pretty-print / inspect) | 17 | hard (pure reflection) | **none** (no Formatter usage found) | **Stub on JS** (or non-reflective rewrite later). Lowest value-per-error in the whole tree — do **not** burn the budget here. |
| **`Schema`** core | 4 (+ JsonSchema 5) | hard (codec derivation) | **critical** (~530 calls) | The real work. See below. |
| **`Data`/`Chunk`** single hits | 2 | localizable | low | `inline` the one generic site or drop the reflective branch on JS. |

**Proposed Schema solution — the key design decision.** eff-sharp's Schema must move from *runtime
reflective derivation* to **compile-time derivation**, because runtime `Type` info is gone under Fable.
Two viable routes:

1. **`inline` + per-type codecs (recommended).** Mark derivation entry points `inline` so the generic
   resolves at the call site (Fable *does* emit `typeof<'T>` metadata when resolved at compile time —
   this is precisely how `Thoth.Json`'s `Decode.Auto` works under Fable→JS). Records/unions/tuples/
   options then auto-derive from a single isomorphic source. **Caveat (not buried):** classes do **not**
   auto-derive under Fable (must register explicitly), and `int64`/`decimal`/`bigint` need explicit
   coders. Since fl-fg's schemas are codegen'd, we can also have the generator emit explicit codecs and
   sidestep derivation entirely for the generated surface.
2. **Non-reflective façade over a codec library (see external option).**

**Effort (Bar 1):** **Schema is the multi-week item** — call it **2–4 weeks** for an `inline`-based
derivation that passes the existing Schema xUnit tests on JS, and that is optimistic. Formatter-stub +
Rune-fix + Data/Chunk are **~2–3 days** combined. **Bar 2:** Schema correctness across the full codec
matrix (encode/decode round-trips, error formatting, refinements) is a large test surface.

**External-package options (this is where adopt-vs-build is liveliest):**

- **`Thoth.Json`** (`Thoth.Json.Core` + `Thoth.Json.JavaScript`, facade 10.5.1, **maintained, June
  2026**). Fable-native JSON codec. `Decode.Auto` **works under Fable** *when the wrapper is `inline`* —
  it's direct proof the `inline`-derivation route is real. **Use it for the JSON codec path** under
  Schema rather than hand-writing JSON plumbing. **Tradeoff:** Thoth is a *JSON* library, not an
  Effect-`Schema` (no refinements/annotations/transform pipeline/`ParseError` channel) — so we'd build a
  thin **Schema-shaped F# façade over Thoth**, keeping eff-sharp's API contract while Thoth does
  encode/decode. **Verdict: ADOPT Thoth for the codec engine; BUILD the Schema façade/contract on top.**
- **Bind the real `@effect/schema` npm package?** **No.** You'd lose schema→static-type derivation (Fable
  can't read TS types), re-introduce the Effect-runtime duality at every boundary, and re-depend on the
  library you're replacing — defeating the native-F# authoring goal. Borrow its *interface shapes*, not
  its code.
- **Source generators / type providers** to replace reflection? **Not available** — Fable supports
  neither Roslyn source generators nor non-erased type providers; only *erased* type providers and
  `inline` work. So `inline` is the path, full stop.
- **Formatter:** no external rescue and no demand — **stub it.**

---

## 6. Cross-cutting: can we just bridge to the real `effect` npm package?

The tempting shortcut — for Schema, platform, or anything — is to `importMember` the real `effect` /
`@effect/*` packages from Fable and skip the port. **For the Effect-shaped pieces, this is the cardinal
mistake, and it's worth stating loudly:**

- The import *mechanics* are proven (`importMember`/`importDefault`/`[<Import>]`/`[<Emit>]` — it's how
  every Fable app consumes React/Node). **Limits:** types are hand-declared compile-time fiction, no
  runtime type safety, generics erased.
- But `effect` is the **worst-case input**: HKTs (unrepresentable in F#), dual data-first/data-last
  signatures (overload explosion), phantom `Effect<A,E,R>` channels with no runtime witness, thousands
  of exports. `ts2fable`/Glutinum both choke on libraries this size.
- **The fatal semantic problem:** a Fable-compiled F# `Effect` and the TS `effect` runtime are
  **structurally different types with different fiber schedulers, `Exit`/`Cause`/`Scope`
  representations.** Binding `@effect/sql` or `@effect/schema` means **two non-interoperating Effect
  runtimes in one process** — you'd `runPromise` at every boundary and discard the exact semantics
  (interruption, scoping, typed errors) you set out to provide. And it defeats the whole point: a native
  F# authoring experience.

**Rule for the whole port: never run two Effect runtimes in one process.** Bind only *raw leaf JS*
(timers, `pg`, `fetch`, a heap). Borrow `@effect/*` *interface shapes* as design templates; consume
their *code* never. The one nuance: for **non-Effect-shaped leaf utilities** (a UUID lib, a logger,
`undici`), `importMember` is fine and encouraged.

---

## 7. The platform / unstable subsystems — the biggest fl-firegrid gap, and it's not even on .NET

**This section is the honest centerpiece.** fl-firegrid's heaviest *direct* demand after Schema is a set
of subsystems that **do not exist in eff-sharp on any target** — the `src/Effect` tree is flat (99
files) with **no** http, sql, cli, process, filesystem, or platform-node modules. So this is not a
"port to JS" gap; it's a **"hasn't been built at all"** gap, and JS-compat is a second-order concern
behind simply *existing*.

### The architectural decision: one Effect-shaped abstraction, backed twice

The framing for this layer is **not** "bind to Node vs. port the .NET implementation and transpile it" —
that dilemma is dissolved. The decision is:

> **Define ONE intermediate F# abstract service** per capability (`FileSystem`, `Path`, `HttpClient`,
> `Command`/process, `SqlClient`, …) — Effect-shaped, modeled on Effect's own platform-service interfaces
> — and **back it with two `Layer`s under a `#if` split:**
> - a **.NET Layer** over `System.IO`/`System.Net`/`System.Diagnostics.Process`, behind `#if !FABLE_COMPILER`;
> - a **Node Layer** over `fs`/`http`/`child_process`/`path` via Fable `importMember`/`importAll`,
>   behind `#if FABLE_COMPILER`.

This is **the exact dual-backing the `Terminal`/`Path`/`Console` lane already uses** (the same
`#if FABLE_COMPILER` shims this doc proposes in Rock 3) — generalized from "shim a leaf" to "provide a
service with two interchangeable `Layer` implementations." The consumer code (and fl-firegrid) depends
only on the abstract service tag; the runtime supplies whichever `Layer` matches the target.

Two consequences that change how to scope this:

- **The .NET implementation is not throwaway.** It is the **xUnit test surface** (the 1205-fact suite
  exercises these services on .NET) *and* it serves real **.NET consumers** of eff-sharp. So "build the
  abstraction + .NET Layer" is work eff-sharp wants regardless of Fable; the Node Layer is the
  incremental cost for the JS deployment target.
- **External npm packages are candidate implementations of the *Node-side Layer* — not replacements for
  the F# abstraction.** `pg`/`mysql2`/`undici`, and binding helpers like `Fable.Node`/Glutinum, are ways
  to *fill in* `SqlClient`'s or `HttpClient`'s Node `Layer`. They never substitute for the Effect-shaped
  F# interface, which is what gives consumers typed errors, scoping, and interruption. (Ref:
  <https://fable.io/docs/getting-started/javascript.html#nodejs>.)

This also keeps §6's cardinal rule intact: we bind *raw leaf drivers* into the Node `Layer`; we never
bind `@effect/platform`/`@effect/sql` (Effect-shaped TS) and never run two Effect runtimes in one process.

### Per-subsystem

| Subsystem (fl-fg usage) | Abstract F# service | .NET Layer (`#if !FABLE_COMPILER`) | Node Layer (`#if FABLE_COMPILER`) | Effort |
|---|---|---|---|---|
| **`platform-node`** (7) — runtime services | `Path`, process env, args | `System.IO`/`System.Environment` | `path`/`process` via `importMember` | Large |
| **`FileSystem`** (3) | `FileSystem` | `System.IO.File`/`Directory` | `fs`/`fs/promises` | Medium |
| **`ChildProcess`** (3) — `fluent-acp-process` core | `Command` | `System.Diagnostics.Process` | `child_process` | Medium |
| **`HttpApi*` / HttpClient** (5) | `HttpClient` (+ HttpApi builder façade) | `System.Net.Http` | global `fetch`/`undici` (`AbortController`→interruption) | Large (HttpApi is a big surface) |
| **`Sql*`** (3) — chDB embedded | `SqlClient` | ADO.NET provider | **`pg`/`mysql2`** (drivers `@effect/sql` itself binds); chDB needs its own binding | Large |
| **`Cli*`** (3) — verification tooling | `Cli` (arg/flag parser) | pure F# (target-agnostic) | same pure F# (no driver needed) | Medium–Large |
| **OpenTelemetry / NodeSdk** (2) — `observability` | `Tracer`/`Metric` exporter | OTel .NET SDK | **`@opentelemetry/*`** | Medium |
| **Reactivity** (1) | `Reactivity` | pure F# | pure F# | Small |

Node-Layer driver notes (candidate implementations, all healthy/current):
- `pg` v8 — best Fable fit, exactly what `@effect/sql-pg` binds. (`postgres`/porsager: **avoid** as a
  Node-Layer impl — its tagged-template API is unreachable from Fable; you'd be stuck on `sql.unsafe`.)
- global `fetch` (zero-dep, `AbortController` gives interruption/timeout for free); `undici` only if you
  need pooling/interceptors.
- `@opentelemetry/*` — bind behind the `Tracer`/`Metric` exporter service.
- For binding generation, `Fable.Node` (NuGet 1.6.0) covers only `path`/`os`/`url`/`buffer` — `fs`/`http`/
  `child_process`/`process` are **not** in it, so write those Node-Layer bindings directly via
  `Fable.Core.JsInterop` (`importMember`/`importAll`); Glutinum can bootstrap bindings for small concrete
  deps but isn't needed for the Node built-ins.

**Honest scope statement:** this layer is **larger than all five compile-green rocks combined**, and it's
the part fl-firegrid most needs. Compile-green (Bar 1) delivers `Effect`/`Layer`/`Stream`/`Schema` — a
real runtime — but **fl-firegrid cannot drop the `effect` npm dependency until this platform/unstable
layer exists**, because that's where `ChildProcess`, `FileSystem`, `HttpApi`, `Sql`, and OTel live.
Building it as *abstraction + .NET Layer + Node Layer* means most of it (the interface + .NET backing) is
value eff-sharp wants on .NET anyway, with the Node `Layer` as the marginal JS-target cost — but it is
still the difference between "eff-sharp runs on Node" and "eff-sharp replaces `effect` for
fluent-firegrid."

---

## 8. Bar 2 in detail — the run-correctness tail (do not skip)

Compile-green produces a runtime that *runs small cases*. It says nothing about the following, each of
which needs its own validation and **none of which shows up as a Fable error**:

| Run-correctness concern | Current state | What's needed |
|---|---|---|
| **Node test harness** mirroring the .NET suite | ~6 ad-hoc scenarios vs **1205 `[<Fact>]`** | Port the suite to run on Node — see harness note below |
| **Interruption / finalizer ordering** under concurrency | spawn + single-join only (spike) | dedicated fiber-race + scope-finalizer tests |
| **Deep stack-safety** under heavy recursion | untested on JS | trampolining check; JS has no TCO and a shallow stack |
| **GC / throughput / latency** | untested | perf harness; `Async`-on-JS allocates differently than .NET |
| **TestClock determinism** on the `setTimeout` scheduler | untested | parity tests vs .NET TestClock |
| **STM under contention** | untested | property tests (after Rock 4) |
| **Browser / Bun** platform variants | not built | separate platform layers |

**Test-harness reality (from external research):** there is **no first-class Fable↔Vitest DSL.**
`@effect/vitest` (`it.effect`/TestClock) is hard-bound to the *real* `effect` runtime and **cannot test a
Fable-compiled F# Effect** — different `Exit`/`Cause`/`Scope` types. The mature Fable runner is
**`Fable.Mocha`** (the SAFE-stack `#if FABLE_COMPILER` Expecto/Fable.Mocha dual-target pattern). But our
tests are **xUnit `[<Fact>]`**, whose attributes don't run under Fable — so closing Bar 2 means
**mechanically porting 1205 xUnit facts to a dual-targetable `testList` form plus an in-house Effect/
TestClock test shim.** That is a real, large, separate work item, and it is the true gate on "fully
compatible." **Estimate: multi-week, and it is the dominant cost of Bar 2.**

> **Bottom line for Bar 2:** plan it as its own project with its own budget. Treating it as a tail on
> compile-green will badly underestimate the path to "fully compatible."

---

## 9. Build-vs-adopt, consolidated

| Subsystem | Decision | Package(s) | Note |
|---|---|---|---|
| Effect / Fiber / Scope / Ref core | **BUILD** | — | done / compile-green |
| **Deferred + dependents** (Rock 2) | **BUILD** | — | irreducible core; never bind `effect` |
| Timing (`setTimeout`, `perf_hooks`) | **BIND** | `Fable.Core.JS`, `perf_hooks` | not `Fable.Node` |
| Terminal / Path / stdout | **BIND** | `Fable.Core.JsInterop` → `process` | thin platform layer |
| PriorityQueue / heap | **BUILD** | ~40-line F# binary heap | deterministic, dual-target |
| STM async-retry (Rock 4) | **BUILD, late** | — | cheap after Rock 2; no fl-fg demand |
| **Schema codec engine** | **ADOPT** | **Thoth.Json** (Core + JavaScript) | `inline` `Decode.Auto` works under Fable |
| **Schema contract/façade** | **BUILD** | over Thoth | keep eff-sharp's API; `inline` derivation |
| Formatter | **STUB** | — | no demand; lowest value-per-error |
| JSON `Rune`/Unicode | **BUILD** | JS code-point iteration | trivial |
| FileSystem / ChildProcess | **ABSTRACTION + 2 LAYERS** | `System.IO`/`Process` (.NET) + `fs`/`child_process` (Node) | one F# service, dual `#if` backing |
| HTTP / HttpApi | **ABSTRACTION + 2 LAYERS** | `System.Net.Http` (.NET) + global `fetch`/`undici` (Node) | + HttpApi builder façade |
| SQL | **ABSTRACTION + 2 LAYERS** | ADO.NET (.NET) + `pg`/`mysql2` (Node) | drivers fill the Node Layer; not `@effect/sql` TS; not porsager `postgres` |
| OpenTelemetry | **ABSTRACTION + 2 LAYERS** | OTel .NET SDK + `@opentelemetry/*` (Node) | behind our Tracer/Metric |
| Test harness | **BUILD** | port xUnit→`testList` + Effect shim; run on Mocha/Vitest | `@effect/vitest` is template-only |
| **Anything Effect-shaped from `effect`/`@effect/*`** | **NEVER BIND** | — | two runtimes in one process = the cardinal no |

---

## 10. Recommended sequencing (tied to fl-firegrid demand, not to error counts)

The error-count ordering (Deferred first) optimizes *core completeness*. The **fl-firegrid-value
ordering** below optimizes *time-to-drop-the-`effect`-npm-dep*, and they differ. Pick consciously.

**Wave A — finish the cheap compile-green (Bar 1, ~1 week).** Rock 3 platform/timing shims (Terminal,
Clock/Scheduler+heap, Path) + the trivial Rock 5 leaves (Rune fix, Data/Chunk `inline`/drop, **stub
Formatter**). Outcome: only Rock 2 + Schema-core remain as errors. *High leverage, low risk.*

**Wave B — Deferred keystone (Rock 2, ~1 week).** Unblocks Queue/Stream/Latch/Semaphore/PubSub/Sync-Ref
in one stroke; required transitively for fl-fg's Stream/Queue/Fiber usage. **Compile-green is essentially
reached at the end of Wave B** (modulo Schema). *Structural; do it even though fl-fg doesn't import it
directly.*

**Wave C — Schema (Rock 5 core, ~2–4 weeks).** `inline` derivation + Thoth codec engine + Schema façade,
green against the existing Schema tests. **This is fl-firegrid's #1 blocker** (~530 calls). *Longest Bar-1
item; start design early, in parallel with A/B.*

**Wave D — platform/unstable layer (§7, largest, multi-week → months).** For each capability
(`FileSystem`, `Command`, `HttpClient`, `SqlClient`, `Path`, OTel): define the Effect-shaped F# service +
its **.NET Layer** (`System.IO`/`Process`/`Net.Http`/ADO.NET — also the xUnit surface and the .NET
consumers' backing) + its **Node Layer** (`fs`/`child_process`/`fetch`/`pg`/`@opentelemetry` via
`importMember`), under `#if FABLE_COMPILER`; build the HttpApi/Cli façades on top. **This is what actually
lets fl-firegrid replace `effect`.** *Does not exist on any target yet — scope it as new construction. The
interface + .NET Layer is value eff-sharp wants regardless of Fable; the Node Layer is the marginal
JS-target cost.*

**Wave E — run-correctness (Bar 2, parallel + ongoing, multi-week).** Port the 1205-fact suite to a
dual-target `testList` + Effect/TestClock shim on Mocha/Vitest; add interruption/finalizer/stack-safety/
perf validation. **Gate "fully compatible" on this, not on compile-green.**

**Wave F — STM async-retry (Rock 4, ~1 week, deferrable).** No fl-fg demand; do it for parity after the
above. *The most defensible thing to push last.*

**One-line honest summary:** *Bar 1 (compile-green) is ~2–4 weeks of bounded work and ends around Wave C.
But fl-firegrid cannot drop `effect` until Wave D (a platform layer that doesn't exist yet) and we can't
call it "fully compatible" until Wave E (a 1205→Node test port). Compile-green is the **start** of the
real work, not the end.*

---

*Live numbers re-measured at `f8de296` (81 errors / 17 files); fl-firegrid demand read from its five
package sources; external-package assessments from web research (Fable 5.4.0 / Thoth.Json 10.5.1 / `pg`
v8 / `@effect/vitest` 0.29, all current as of June 2026). Caveats are in the body, not in footnotes —
by design.*
