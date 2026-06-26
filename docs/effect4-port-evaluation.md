# eff-sharp vs Effect 4 (effect-smol) — port fidelity evaluation

> Scope: how close eff-sharp is to a semantic port of Effect 4 (the `effect-smol`
> rewrite) under F#/Fable, where the gaps are, how to better exploit F#, and
> whether to run upstream Effect tests as a conformance gate. Findings are from a
> read of the core runtime, DI, error model, concurrency/STM, Schema,
> Stream/Channel, and the HTTP/SQL/platform layers as of this branch.

## TL;DR

eff-sharp is a **serious, large, mostly-non-stubbed port** (~25k LOC core, 141
modules, ~145 F# spec files) that faithfully reproduces Effect 4's *API surface
and authoring ergonomics* — and in a few places improves on them. But it is **not
a 1:1 semantic port of the runtime**. The core makes one foundational substitution
that ripples through everything:

```fsharp
Effect<'A,'E,'R> = FiberRuntime -> 'R -> Async<Exit<'A,'E>>
```

i.e. a **Reader-over-F#-`Async`**, not Effect's defunctionalized **fiber
interpreter + scheduler**. That choice buys Fable portability and idiomatic F#,
at the cost of the precise `R` dependency-tracking that is arguably Effect's
signature feature, deterministic scheduling, fine-grained interruption, real
chunked streaming, and ~35–100× sync throughput (the repo's own number, see the
cutover plan). Net: **excellent as an "Effect-flavored F#/Fable library";
partial as a conformant Effect 4 port.**

## Fidelity scorecard

| Subsystem | Fidelity | Notes |
|---|---|---|
| Core `Effect` combinators | 🟢 High | Real, idiomatic, behavior matches |
| `Cause` / `Exit` | 🟢 High | Flat `Reason` list matches effect-smol's flattened Cause; good render parity |
| `effect { }` CE / ergonomics | 🟢 High (better) | Native do-notation incl. `for`/`while`/`use`/`try-finally` |
| Typed errors via DUs | 🟢 High (better) | Exhaustiveness-checked `match` vs string `catchTag` |
| `Context` / `Layer` / DI | 🟡 Partial | **`R` not type-tracked** — see Gap #1 |
| Fibers / interruption | 🟡 Partial | Cooperative flag at `flatMap` boundaries only; no scheduler |
| Structured concurrency | 🟡 Partial | Not automatic; needs explicit `FiberSet`/`Scope` |
| Concurrency operators | 🔴 Low | `forEach` sequential; **no `{ concurrency }`**, no `forEachPar`/`race`/`merge` |
| STM (`Tx*`) | 🟡 Partial | Real optimistic OCC + retry; single global commit lock; **no cross-module tx composition** |
| Schema | 🟢 High (.NET) / 🟡 (Fable) | Real codecs + issue trees + JSON Schema; `derive` is reflection → **fails on Fable** |
| Stream / Channel / Sink | 🔴 Low | Element-at-a-time **push-fold**, not chunked pull Channel; no backpressure/concurrency |
| HttpApi / Client / Server | 🟢 High | Faithful declarative model, Schema-driven, real Node bindings, e2e tests |
| SQL | 🟡 Framework-only | Solid Statement/Client abstraction; **no concrete driver** |
| CLI / Multipart / SSE / OpenAPI | 🟢 Good | Real logic; argv is Fable-emit |

## The gaps (with code references)

**Gap 1 — `R` is runtime-checked, not type-tracked. (Biggest divergence.)**
In Effect, `R` is a type-level set of services and the compiler tells you exactly
what is still unprovided; `Layer<ROut, E, RIn>` tracks what it provides. Here
`Layer<'E,'RIn>` **drops `ROut`** (`Layer.fs:14`), `provideService` returns
`Effect<'A,'E,'R>` for *any* `'R`, and `Effect.service` does a runtime
`match box env with :? Context` (`Effect.fs:609`) with a missing service surfacing
as a `KeyNotFoundException` **defect at runtime** (`Effect.fs:621`). This follows
directly from F# lacking HKTs/row polymorphism. The README also mixes two DI
models (a raw record `Config` as `'R` *and* `Context`) which cannot coexist
cleanly.

**Gap 2 — No real scheduler / structured concurrency / concurrency controls.**
`fork` is `Async.StartChild` (`Effect.fs:328`); no central scheduler, no op-count
preemption, no automatic parent→child interruption (must wrap in `FiberSet`/
`Scope`), and `forEach` is strictly sequential with no `{ concurrency }` option.
Interruption is a cooperative `bool` checked only at `flatMap`/`sleep` boundaries
(`Effect.fs:234`), so a long `Effect.sync` loop is uninterruptible. Scheduling
determinism is delegated to the host event loop.

**Gap 3 — Stream is not Channel.** Upstream Stream is a chunked, pull-based,
backpressured Channel consumer. Here it is an eager push-fold
(`('A -> Effect<unit>) -> Effect<unit>`); Channel is a same-shape 61-line alias;
`Take`/`Pull` exist but are largely disconnected. Upstream stream code relying on
chunking/backpressure/merging will silently misbehave.

**Gap 4 — Schema `derive` is reflection-based and `failwith`s under Fable**
(`Schema.fs:894`) — on the actual ship target you must hand-build schemas. The
`Schema.Type<typeof S>` inference model is impossible in F# (you write the type
first; the schema points *at* it — arguably nicer, but a different model).

**Gap 5 — STM doesn't compose across modules.** Each `Tx*` op is its own
transaction; there is no `Effect.atomic`/`Effect.tx` spanning a `TxQueue` +
`TxSemaphore`. A single global commit lock also bottlenecks under contention.

**Gap 6 — `PORTING.md` is badly stale.** It lists most modules as "planned" while
the files exist and are implemented; the real status lives in the cutover doc.

## Where it is genuinely *better* than Effect (lean in)

- **`effect { }` / `stream { }` / `stm { }`** native CEs beat generator hacks.
- **Typed errors as DUs** with exhaustiveness checking; **`Cause`/`Exit` as plain
  DUs** you pattern-match directly.
- **Units of measure** in Schema — dimensional safety TS cannot express.
- **No HKT ceremony** — the concrete surface is clean.

## Actual gaps vs. things better replaced with F#/Fable

Not every divergence from Effect is a hole to fill. The dividing line:

- **Actual gap** — something Effect does that F#/Fable *can and should* do too, but
  eff-sharp hasn't built (or built thinly). Closing it makes the port more
  faithful with no downside.
- **Better replaced** — machinery that exists in Effect *only because TypeScript
  lacks something F# has natively*. Porting it faithfully is wasted effort or
  strictly worse than the F# idiom. The "gap" is illusory; the move is to
  delete/replace and **document the boundary**.

**The one-line test:** if Effect built it to *simulate an F#-native capability*
(pattern matching, DUs, structural equality, type-first modeling, the
data-structure zoo, HKT plumbing) → replace and document. If Effect built it
because *it is genuinely part of the effect system* (concurrency operators,
structured concurrency, STM composition, real interruption, chunked streams) →
actual gap, close it.

### Actual gaps (close them)

| Item | Why it's a real gap |
|---|---|
| Concurrency operators (`forEachPar`, `{ concurrency = n }`, `race`, `mergeAll`, `zipPar`) | Core Effect surface, no F# blocker; `Semaphore` primitive already exists. Pure unbuilt work. |
| Structured concurrency auto-interrupt (parent ⇒ child) | A *correctness* guarantee users assume. `FiberSet`/`Scope` exist; `fork` just doesn't wire them. |
| Schema derivation **mechanism** on Fable | The `failwith` on the ship target (`Schema.fs:894`) is a literal hole. Closeable via Fable compile-time reflection (Thoth model). |
| STM cross-module composition (`Effect.atomic`) | Headline Effect feature, no F# obstacle — just not built. (Narrow audience → low priority, but a true gap.) |
| Interruption granularity + scheduling determinism | Coarse today (flag at `flatMap` only). Genuinely missing for faithful interruption + deterministic `TestClock` tests. Only *fully* closable via the interpreter — see crossover. |
| Stale `PORTING.md` | Trivial doc gap. |
| No upstream-test conformance gate | Real measurement gap — parity is currently unprovable. |

### Better replaced with F#/Fable (don't chase Effect's design)

| Effect machinery | F#/Fable replacement | Status |
|---|---|---|
| Type-level `R` subtraction | Runtime `Context` (Fable-safe) ± Orsak-style interface constraints. F# *cannot* express set subtraction — unportable, not unbuilt. | Replace + **document as out of scope** |
| `Match` / `Option.match` / `Exit.match` / `Cause.match` combinator DSL | Native `match` + active patterns | ✅ done (good) |
| `Cause`/`Exit` as opaque combinator types | Plain DUs you pattern-match | ✅ done |
| `HKT` / `Covariant` / `Pipeable` / `Inspectable` / `Effectable` | Dropped | ✅ done (correct) |
| `Schema.Type<typeof S>` (derive a *type* from a schema) | Write the F# type first; schema points *at* it; + units of measure | Replace — the F# model is **better** |
| `Effect.gen(function*…)` generators | `effect { }` / `stream { }` / `stm { }` CEs | ✅ done (better) |
| string-tag `catchTag` | Exhaustive DU `match` | ✅ done (better) |
| `Data` / `TaggedError` / `Equal` / `Equivalence` / `Hash` / `Order` / `Combiner` / `Reducer` typeclasses | F# structural equality, `IComparable`, records/DUs | Replace — mostly redundant on F# |
| `Chunk`, `HashMap`, `HashSet`, `MutableHashSet`, … | FSharp.Core `Map`/`Set`/`list`/`array` + `System.Collections` | Replace candidates — the repo's own benchmark shows native is **equal-or-faster** (Tier 1, `RESULTS.md`) |
| `SynchronizedRef` / atomic-CAS refs | Plain `Ref` — atomicity is moot on single-threaded JS | Replace/thin |

### The crossover item (it is both)

**The core runtime** (`Reader-over-Async` → custom microtask interpreter, P7) is
*simultaneously* a gap and a replacement:
- *closing a gap* — perf, interruption granularity, structured concurrency, and
  determinism all trace back to the substrate; **and**
- *a Fable-native replacement* — F#'s `Async` yields on the **macrotask** queue
  (`setTimeout(0)` every 2000 binds), the wrong primitive; the F#/Fable-correct
  substitute is a trampolined op-tree on `queueMicrotask`, mirroring Effect-TS's
  own scheduler.

So the faithful answer *is* the F# alternative — which is why it's the
highest-ceiling, highest-risk item.

### The fence-sitter

**Stream-as-chunked-Channel** straddles the line. It's a real gap *if* you need
Channel semantics, but a strong **replace** candidate otherwise: F# has
`IAsyncEnumerable` / `TaskSeq` / `AsyncSeq` natively, so for most consumers a thin
`taskSeq`-backed stream beats porting upstream's ~8.6k-line Channel. Decide this
one by **consumer demand, not fidelity**.

## Stack rank — what to fix, by importance

Ranked by leverage = (impact on fidelity + impact on real consumers) ÷ cost.
"Impact" is how much it moves eff-sharp toward a usable, honest Effect 4 on
F#/Fable; "Cost" is rough engineering effort.

| # | Item | Impact | Cost | Why this rank |
|---|---|---|---|---|
| **1** | **Decide the project's identity** (conformant runtime vs. "Effect-flavored F#") and write it down | Very high | Low | Every other decision (R channel, interpreter, stream depth) depends on this. Without it, the port keeps half-doing both and shipping thin versions. Cheap, unblocks everything. |
| **2** | **Resolve the `R` channel story** (Gap 1) | Very high | Med–High | The single largest semantic divergence and the thing most visible to users. Pick: keep runtime `Context` (and document typed-R is out of scope) **or** adopt an SRTP-typed environment. Don't keep both half-wired. |
| **3** | **Concurrency operators** — `forEachPar`, `{ concurrency = n }`, `race`, `mergeAll`, `zipPar` (Gap 2 surface) | High | Med | Table-stakes for an Effect port; the `Semaphore` primitive to build them already exists. High user impact, contained. |
| **4** | **Structured concurrency by default** — `fork` auto-registers on the enclosing scope; parent interrupt ⇒ child interrupt (Gap 2 semantics) | High | Med | A correctness guarantee people *assume* from Effect. Today it's manual and easy to get wrong (leaked fibers). |
| **5** | **Schema derivation on Fable** via a build-time source generator (Gap 4) | High | Med | Unblocks HttpApi/codecs on the actual ship target without staying on TS. Reflection path already works on .NET; this closes the JS gap. |
| **6** | **Fix `PORTING.md` / single source of truth for status** (Gap 6) | Med | Low | Cheap honesty; it currently misrepresents the project to any evaluator. Auto-generate from the module list + a status attribute. |
| **7** | **Interpreter-loop core rewrite** (Gap 2 root; perf) — defunctionalized op tree + trampolined step loop on the microtask queue | High | High | The highest-ceiling change: fixes perf (~35–100×), interruption granularity, scheduling determinism, and structured concurrency *at the root*. Only worth it if #1 chooses "conformant runtime." Big, risky, do it deliberately. |
| **8** | **STM transaction composition** — `Effect.atomic` spanning multiple `Tx*` (Gap 5) | Med | High | Headline STM feature, but narrow audience; defer unless a consumer needs it. |
| **9** | **Stream → real (chunked, pull, backpressured) Channel** (Gap 3) | Med–High | Very High | Faithful streaming is a large subsystem (upstream Channel is ~8.6k LOC). High value only for streaming-heavy consumers; the I/O-bound cutover targets don't need it yet. Sequence after the core (#7) is settled. |

Pragmatic reading for the stated goal (the fluent-firegrid cutover, which is
I/O-bound): do **#1, #2, #3, #4, #5, #6** now; treat **#7** as a separate funded
track; **#8/#9** on demand.


## Implementation proposals (Fable/F#-research-informed)

These map to the stack rank. Each is grounded in how Fable actually compiles and
what F# can/can't express; sources at the end.

### P2 — The `R` channel: runtime `Context` floor + optional Orsak-style typed layer

**Research finding.** A "typed `R` that the compiler subtracts on `provide`" is
**not achievable in today's F#** — there is no type-level set subtraction, and
member-constraint SRTP on `^Env` is the *worst* choice for Fable (Fable runs its
own simplistic trait-call resolver and frequently can't resolve nested SRTP;
Fable issues #2083/#2468). The one production-ish F# Effect lib, **Orsak**
(`JohSand/Orsak`), deliberately avoids SRTP and instead encodes requirements as
**nominal interface subtype constraints accumulated by inference**:

```fsharp
// service + a "has-a" provider interface
type IClock = abstract Now: unit -> int64
type IClockProvider = abstract Clock: IClock

// requiring a service adds a flexible constraint 'r :> IClockProvider
let now () = Effect.create (fun (p: #IClockProvider) -> p.Clock.Now())

// binding several such effects unifies them onto ONE 'r; constraints pile up
// 'r :> IClockProvider and 'r :> ILogProvider  — inferred, not hand-written
// discharge ALL at once with a concrete env implementing every provider
```

This gives *readable* errors ("`MyEnv` does not implement `IClockProvider`") and
good inference, at the cost of provider boilerplate (Orsak mitigates with a Myriad
generator). But Orsak is `ValueTask`/resumable-code based and **not
Fable-friendly**.

**Proposal.** Keep the **runtime `Context` as the primary, Fable-safe model** (it
already exists and works on both targets), and *stop* mixing in the raw-record-as-
`'R` model from the README — pick one. Then, as an **opt-in upper layer for .NET
consumers who want compile-time DI checking**, offer the Orsak-style provider
encoding behind the same `Tag` vocabulary (a `Tag<'S>` maps to a generated
`I'SProvider`). Most importantly: **document that precise type-level `R`
subtraction is out of scope** so the gap is a stated design boundary, not a
silent surprise.
- *Effort:* Low if you just commit to `Context` + docs; Medium to add the typed
  layer.
- *Risk:* Low (Context path); the typed layer is additive.

### P7 — Core runtime: custom trampolined interpreter on a microtask scheduler

**Research finding.** Fable's `Async` is a CPS model with a trampoline that, after
**2000 synchronous binds**, yields via **`setTimeout(f, 0)` — a *macrotask***
(`AsyncBuilder.ts`, `maxTrampolineCallCount = 2000`). That is the wrong yield
primitive for an effect interpreter (timer clamp + I/O interleaving per 2000
steps), and `Async.startImmediate` is just an alias of `start`, while
`StartAsTask`/`AwaitTask` don't exist on Fable at all. By contrast Effect-TS's
default `MixedScheduler` yields on the **microtask** queue
(`Promise.resolve().then`) and only falls back to `setTimeout(0)` after ~**2048**
nested drains (`Scheduler.ts`, `maxNextTickBeforeTimer = 2048`).

**Proposal (only if P1 chooses "conformant runtime").** Replace
`FiberRuntime -> 'R -> Async<Exit>` with a **defunctionalized op tree**
(`Succeed | Sync | FlatMap | Async | Fork | Yield | …` as a DU) interpreted by an
explicit continuation-stack step loop, scheduled **microtask-first**:

```fsharp
[<Emit("queueMicrotask($0)")>]
let private queueMicrotask (f: unit -> unit) : unit = nativeOnly
// step N ops synchronously; every ~2048 steps, re-arm via queueMicrotask;
// fall back to setTimeout(0) only to avoid starving the macrotask/I-O queue.
```

Keep F# `Async`/`Promise` only at the **boundary** (`Async.awaitPromise` /
`startAsPromise` are clean and stay). This single change fixes perf (the repo's
~35–100× sync gap), interruption granularity (yield at every op, not just
`flatMap`), scheduling determinism, and gives you the substrate to make
structured concurrency real. Precedent: Fable's own trampoline and AsyncRx prove
hand-written schedulers compile fine under Fable.
- *Effort:* High (it's a core rewrite; stage behind the existing public API so
  combinators are re-pointed, not rewritten).
- *Risk:* High — do it on a branch with the conformance suite (below) as the net.

### P3/P4 — Concurrency operators + structured concurrency

**Proposal.** On the current Async core you can ship most of this *now* without
P7: build `forEachPar` / `{ concurrency = n }` / `race` / `mergeAll` on the
existing `Semaphore` + `fork`/`await`. For structured concurrency, make `fork`
**auto-register the child on the enclosing `Scope`** (the `FiberSet`/`Scope`
machinery already exists — wire it into `fork` so parent interrupt ⇒ child
interrupt is the default, not opt-in). If you later land P7, these re-point onto
real fibers unchanged.
- *Effort:* Medium. *Risk:* Low–Medium (interruption-of-suspended-`Cell.await`
  is the sharp edge — see the Semaphore note; a custom interpreter (P7) removes
  it).

### P5 — Schema derivation that works on Fable

**Research finding.** Type Providers **do not work** under Fable (confirmed,
Fable #278). The production-proven Fable path for "derive a codec from a type" is
**Fable's compile-time reflection** (the Thoth.Json model): Fable injects static
`TypeInfo` at the call site for `FSharpType.GetRecordFields/GetUnionCases`, so an
`inline` generator can walk it with **no runtime .NET reflection**.

**Proposal.** Replace the reflection `derive` that `failwith`s on Fable
(`Schema.fs:894`) with a **Thoth-style `inline` generator**:

```fsharp
let inline deriveSchema<'T> () : Schema<'T> =
    // walk FSharpType metadata Fable injects at this call site; cache by type.
```

Caveats to design around (from Thoth's scars): records/DUs/tuples/primitives
only (register classes manually via an `extra` map); `int64/bigint/decimal`
opt-in; cache generated codecs (reflection info costs bundle bytes per call
site). For bundle-size-critical or fully-static needs, offer **Myriad** as an
opt-in build step (it emits ordinary F# source *before* Fable, so it's
Fable-agnostic by construction; study `WoofWare.Myriad` for codec-gen prior art).
**Avoid Fable AST plugins** for this — wrong granularity (only decorated members)
and unstable API.
- *Effort:* Medium. *Risk:* Medium (Fable reflection has known sharp edges, e.g.
  `FSharpOption` recognition #4082 — pin your Fable version).

### Cross-cutting — interruption & interop polish

- **Interruption:** the current cooperative flag is fine but coarse (checked only
  at `flatMap`/`sleep`). On the Async core you could instead lean on F# `Async`'s
  built-in cooperative `CancellationToken` (it auto-propagates through binds and
  runs `try/finally` finalizers, and Fable *does* implement the token
  primitives). With P7 you get per-op checks for free. Either way, document the
  granularity guarantee.
- **Interop:** prefer `[<Import>]` (tree-shakeable) for Node modules; confine
  `[<Emit>]` to a thin primitives module behind a typed façade (it's opaque to
  Fable's optimizer); use `[<Erase>]` unions for `string | number`-style params;
  generate typings with **Glutinum** (current) rather than legacy ts2fable.

### Sources

- Fable `Async.ts` / `AsyncBuilder.ts` (CPS, trampoline `maxTrampolineCallCount=2000`, `setTimeout(0)` hijack, `startImmediate`=`start`, no `StartAsTask`/`AwaitTask`): https://github.com/fable-compiler/Fable/blob/main/src/fable-library-ts/Async.ts
- Effect-TS `Scheduler.ts` (MixedScheduler, microtask-first, `maxNextTickBeforeTimer=2048`): https://github.com/Effect-TS/effect/blob/main/packages/effect/src/Scheduler.ts
- Orsak (typed-R via accumulated interface constraints, Myriad generator): https://github.com/JohSand/Orsak ; encoding origin: https://www.bartoszsypytkowski.com/dealing-with-complex-dependency-injection-in-f/
- SRTP limits & Fable trait-call gaps: https://github.com/fable-compiler/Fable/issues/2083 , https://github.com/fable-compiler/Fable/issues/2468 ; FSharpPlus compile-perf: https://github.com/fsprojects/FSharpPlus/issues/24
- Type Providers unsupported on Fable: https://github.com/fable-compiler/Fable/issues/278
- Thoth.Json auto codecs (Fable compile-time reflection) + limits: https://thoth-org.github.io/Thoth.Json/documentation/auto/json-representation.html , https://github.com/fable-compiler/Fable/issues/4082
- Myriad: https://github.com/MoiraeSoftware/myriad ; WoofWare.Myriad: https://github.com/Smaug123/WoofWare.Myriad
- Interop / Glutinum: https://fable.io/docs/javascript/features.html , https://fable.io/blog/2024/2024-01-01-Glutinum_a_new_era.html
- F# Async cooperative cancellation: https://learn.microsoft.com/dotnet/fsharp/language-reference/statically-resolved-type-parameters ; Async.AwaitTask cancellation gap: https://github.com/dotnet/fsharp/issues/2127



## Compatibility layer: upstream Effect tests as a conformance gate

Recommended — but as a **tiered conformance suite**, not a drop-in. Two realities
shape the design:

- Today's tests are **hand-authored F# specs via a Vitest facade**
  (`test/support/Vitest.fs`), *not* the upstream `*.test.ts`. So there is no
  existing parity gate — every spec is a re-interpretation, which is exactly where
  silent divergence hides.
- The upstream `.test.ts` cannot run unmodified: the public API is idiomatic F#
  (curried/piped, DU errors, `'R = Context`, different names). You need a thin
  **TS adapter shim** mapping the Effect-TS surface onto the Fable-compiled
  eff-sharp exports, then run the real upstream tests through it.

**Tiering** (run order = increasing divergence):

- **Tier A — data types** (`Option`, `Result`/`Either`, `Chunk`, `HashMap`,
  `Duration`, `DateTime`, `Cause`, `Exit`, `BigDecimal`): adapter + upstream
  tests. Mostly *should* pass; cheap to wire; immediate payoff.
- **Tier B — core Effect semantics** (succeed/fail/catch, finalizer ordering,
  `acquireUseRelease`, basic interruption): run via adapter; expect some passes;
  document divergences.
- **Tier C — scheduler/concurrency/Stream/STM/precise-`R`**: keep as an
  **`xfail` manifest** with a one-line rationale per failure ("intentional: no
  chunked Channel", "intentional: `R` not type-tracked"). Failing here is
  *information*, not regression.

**Operating model:** keep the F# specs as the **primary CI gate** (they test your
semantics and the value-added layers); add the upstream suite as a **separate,
non-blocking conformance job** with a tracked allowlist you shrink over time. The
discipline that makes it worthwhile is the explicit **xfail manifest** — it turns
"we diverge somewhere, unknown" into "we diverge *here, on purpose, documented*,"
and it is the single best artifact for anyone evaluating how close the port is.

Your value-added parts (HttpApi-on-F#, units-of-measure Schema, the CEs, the
fluent-firegrid drivers) layer cleanly on top because they don't depend on the
divergent corners.

**Caveat:** a conformance suite pins you to a specific `effect-smol` commit, and
effect-smol is pre-1.0 and churns — budget for re-vendoring.

### Proposed harness layout

```
conformance/
  vendor/                     # pinned effect-smol checkout (script-managed, gitignored)
  adapter/effect-shim.ts      # maps Effect-TS API -> Fable-compiled eff-sharp exports
  xfail.json                  # { "Stream.test.ts > merge": "intentional: push-fold stream", ... }
  run.mjs                     # runs vendored tests through the shim; reconciles vs xfail.json
  README.md                   # tiers, how to shrink xfail, re-vendor steps
tools/conformance/vendor.sh   # clone/pin effect-smol at a known SHA
```

CI: `conformance` is a distinct, non-blocking job. A test that passes but is
listed in `xfail.json` is a *hard failure* ("unexpected pass — remove from
xfail"), which is how the allowlist ratchets down instead of rotting.

