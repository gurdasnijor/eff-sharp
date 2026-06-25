# eff-sharp ai-docs

Runnable, type-checked F# examples that teach the eff-sharp API by example. Every
snippet lives in a module that compiles against the real library
(`../src/Effect/Effect.fsproj`), so these docs can never drift from the code.

Run the whole tour:

```bash
export DOTNET_ROOT="/opt/homebrew/opt/dotnet/libexec"; export PATH="$PATH:$DOTNET_ROOT"
dotnet run --project ai-docs/EffSharp.AiDocs.fsproj
```

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
