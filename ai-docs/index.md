# eff-sharp ai-docs

Runnable, heavily-commented F# examples that teach the eff-sharp API — the native
F# port of [Effect](https://effect.website). Every example here **compiles against
the real library** (`EffSharp.AiDocs.fsproj` references `../src/Effect`), so the
docs can never drift from the code. Each topic exposes an entry function that
`Program.fs` invokes, printing labelled output you can read next to the source.

Run the whole tour:

```bash
export DOTNET_ROOT="/opt/homebrew/opt/dotnet/libexec"; export PATH="$PATH:$DOTNET_ROOT"
dotnet run --project ai-docs/EffSharp.AiDocs.fsproj
```

## Start here

- [**Why eff-sharp?**](./00_why-effsharp.md) — side-by-side wins where F#'s
  computation expressions, pattern matching, records/DUs, and units of measure
  beat Effect's combinator-only TypeScript authoring.

## 01 · Effect basics

| Topic | File | What it covers |
|-------|------|----------------|
| Creating & composing | [`01_effect/01_Basics.fs`](./01_effect/01_Basics.fs) | `succeed`/`fail`/`sync`; combinators vs the `effect { }` CE; `for`/`match` in the CE |
| Services & DI | [`01_effect/02_Services.fs`](./01_effect/02_Services.fs) | `Tag` · `Context` · `Effect.service` · `Layer` · `provideService` |
| Errors | [`01_effect/03_Errors.fs`](./01_effect/03_Errors.fs) | typed DU errors, `catchAll`, native `match` on `Exit`/`Cause`, defects |
| Resources | [`01_effect/04_Resources.fs`](./01_effect/04_Resources.fs) | `Scope.scoped`, `acquireRelease`, `ensuring`, `use` for `IDisposable` |
| Running | [`01_effect/05_Running.fs`](./01_effect/05_Running.fs) | `runSync`/`runExit`/`runAsync`; matching the `Exit` |
| PubSub | [`01_effect/06_PubSub.fs`](./01_effect/06_PubSub.fs) | `bounded` hub, scoped `subscribe`, `publish`, fan-out |
| Concurrency | [`01_effect/07_Concurrency.fs`](./01_effect/07_Concurrency.fs) | `fork`/`join`, `Fiber`, structured interruption, racing |
| State | [`01_effect/08_State.fs`](./01_effect/08_State.fs) | `Ref`/`SynchronizedRef` atomic state under concurrency |
| Queues | [`01_effect/09_Queues.fs`](./01_effect/09_Queues.fs) | `Queue` back-pressure, producer/consumer |

## 02 · Streams

| Topic | File | What it covers |
|-------|------|----------------|
| Streams | [`02_Stream.fs`](./02_Stream.fs) | the `stream { }` CE (`yield`/`yield!`/`for`), `map`/`filter`/`flatMap`/`take`, effectful `tap`/`mapEffect` |

## 03 · Integration

| Topic | File | What it covers |
|-------|------|----------------|
| Managed runtime | [`03_integration/10_ManagedRuntime.fs`](./03_integration/10_ManagedRuntime.fs) | build one `Runtime` from a `Layer`, call it from imperative handlers, validate input with `Schema` |

## 04 · Data

| Topic | File | What it covers |
|-------|------|----------------|
| Data toolkit | [`04_data/01_DataToolkit.fs`](./04_data/01_DataToolkit.fs) | records/DUs as data, structural `Equal`, `Schema` decode/encode |

## 05–08 · Batching, schedules, time, observability

| Topic | File | What it covers |
|-------|------|----------------|
| Batching & caching | [`05_Batching.fs`](./05_Batching.fs) | `Cache` memoization/dedup/invalidate, the `Request` completion surface |
| Schedules | [`06_Schedule.fs`](./06_Schedule.fs) | `recurs`/`spaced`/`exponential`/`jittered`, `intersect`/`union`, `collectOutputs` |
| DateTime & Duration | [`07_DateTime.fs`](./07_DateTime.fs) | construction, ISO formatting, calendar `add`/`distance`, ordering/`clamp`, units of measure |
| Observability | [`08_Observability.fs`](./08_Observability.fs) | `Logger`/`LogLevel` gating, the swappable `Console` service, `Tracer` spans + trace propagation |

## 09 · Testing

| Topic | File | What it covers |
|-------|------|----------------|
| Effect tests | [`09_testing/10_EffectTests.fs`](./09_testing/10_EffectTests.fs) | run + assert on `Exit`; parameterized cases; a fixed-clock service |
| Layer tests | [`09_testing/20_LayerTests.fs`](./09_testing/20_LayerTests.fs) | swap a real dependency `Layer` for a deterministic stub |

## ⭐ Bonus — F# ergonomics Effect can't match

- [`bonus/01_CEPower.fs`](./bonus/01_CEPower.fs) — `stream { }`, units of measure, records/DUs + `Schema`.
- [`bonus/02_StmPower.fs`](./bonus/02_StmPower.fs) — the `stm { }` CE: atomic, composable transactions.

Platform-only upstream categories (`http-client`, `http-server`, `cli`, `ai`,
`cluster`, `child-process`) are intentionally skipped here — those packages are
ported separately.
