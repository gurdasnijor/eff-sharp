# eff-sharp ai-docs

<<<<<<< HEAD
Runnable, type-checked F# examples that teach the eff-sharp API by example. Every
snippet lives in a module that compiles against the real library
(`../src/Effect/Effect.fsproj`), so these docs can never drift from the code.

Run the whole tour:
=======
Runnable, heavily-commented F# examples that teach the eff-sharp API — the native
F# port of [Effect](https://effect.website). Every example here **compiles against
the real library** (`EffSharp.AiDocs.fsproj` references `../src/Effect`), so the
docs can never drift from the code.

## Run them
>>>>>>> wave6/showcase2

```bash
export DOTNET_ROOT="/opt/homebrew/opt/dotnet/libexec"; export PATH="$PATH:$DOTNET_ROOT"
dotnet run --project ai-docs/EffSharp.AiDocs.fsproj
```

<<<<<<< HEAD
## Start here

- [Why eff-sharp? — F# wins over Effect (TS)](./00_why-effsharp.md) — side-by-side ergonomics.

## 01 · Effect basics

| Topic | File | What it covers |
|-------|------|----------------|
| Creating & composing | [`01_effect/01_Basics.fs`](./01_effect/01_Basics.fs) | `succeed`/`fail`/`sync`; combinators vs the `effect { }` CE; `for`/`match` in the CE |
| Services & DI | [`01_effect/02_Services.fs`](./01_effect/02_Services.fs) | `Tag` · `Context` · `Effect.service` · `Layer` · `provideService` |
| Errors | [`01_effect/03_Errors.fs`](./01_effect/03_Errors.fs) | typed DU errors, `catchAll`, native `match` on `Exit`/`Cause`, defects |
| Resources | [`01_effect/04_Resources.fs`](./01_effect/04_Resources.fs) | `Scope.scoped`, `acquireRelease`, `ensuring`, `use` for `IDisposable` |
| Running | [`01_effect/05_Running.fs`](./01_effect/05_Running.fs) | `runSync`/`runExit`/`runAsync`; matching the `Exit` |
| PubSub | [`01_effect/06_PubSub.fs`](./01_effect/06_PubSub.fs) | `bounded` hub, scoped `subscribe`, `publish`, fan-out |

## 03 · Integration

| Topic | File | What it covers |
|-------|------|----------------|
| Managed runtime | [`03_integration/10_ManagedRuntime.fs`](./03_integration/10_ManagedRuntime.fs) | build one `Runtime` from a `Layer`, call it from imperative "handlers", validate input with `Schema` |

## 09 · Testing

| Topic | File | What it covers |
|-------|------|----------------|
| Effect tests | [`09_testing/10_EffectTests.fs`](./09_testing/10_EffectTests.fs) | run + assert on `Exit`; parameterized cases; a fixed-clock service (TestClock analogue) |
| Layer tests | [`09_testing/20_LayerTests.fs`](./09_testing/20_LayerTests.fs) | swap a real dependency `Layer` for a deterministic stub |

## ⭐ Bonus

- [`bonus/01_CEPower.fs`](./bonus/01_CEPower.fs) — `stream { }`, units of measure, records/DUs + `Schema`.

> Skipped (upstream-only, those packages aren't ported): `02_stream` depth,
> `05_batching`, `06_schedule`, `07_datetime`, `08_observability`, and all
> platform topics (`http-client`, `http-server`, `child-process`, `cli`, `ai`,
> `cluster`).
=======
Each topic file exposes a `run ()` that `Program.fs` invokes, printing labelled
output you can read next to the source.

## Start here

- [**Why eff-sharp?**](./00_why-effsharp.md) — 5 side-by-side wins where F#'s
  computation expressions, pattern matching, records/DUs, and units of measure
  beat Effect's combinator-only TypeScript authoring.

## Topics

| File | Topic | Highlights |
|------|-------|------------|
| [`02_Stream.fs`](./02_Stream.fs) | **Streams** | the `stream { }` CE (`yield`/`yield!`/`for`), `map`/`filter`/`flatMap`/`take`, effectful `tap`/`mapEffect` |
| [`06_Schedule.fs`](./06_Schedule.fs) | **Schedules** | `recurs`/`spaced`/`exponential`/`jittered`, `intersect`/`union`, `collectOutputs`; driven by explicit timestamps |
| [`07_DateTime.fs`](./07_DateTime.fs) | **DateTime & Duration** | construction, ISO formatting, calendar `add`/`distance`, ordering/`clamp`, units of measure |
| [`08_Observability.fs`](./08_Observability.fs) | **Observability** | `Logger`/`LogLevel` gating, the swappable `Console` service, `Tracer` spans + trace propagation |
| [`05_Batching.fs`](./05_Batching.fs) | **Batching & caching** | `Cache` memoization/dedup/invalidate, the `Request` completion surface |

Platform-only upstream categories (`http-client`, `http-server`, `cli`, `ai`,
`cluster`, `child-process`) are intentionally skipped — those packages are not
ported.

## Mapping to upstream

These mirror the structure of Effect's own `ai-docs/src/<category>/`; each file's
top doc-comment notes the upstream topic and any behaviour deferred to a later
porting slice (e.g. `Effect.repeat` wiring for schedules, chunked streams).
>>>>>>> wave6/showcase2
