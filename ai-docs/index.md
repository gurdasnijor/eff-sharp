# eff-sharp ai-docs

Runnable, heavily-commented F# examples that teach the eff-sharp API — the native
F# port of [Effect](https://effect.website). Every example here **compiles against
the real library** (`EffSharp.AiDocs.fsproj` references `../src/Effect`), so the
docs can never drift from the code.

## Run them

```bash
export DOTNET_ROOT="/opt/homebrew/opt/dotnet/libexec"; export PATH="$PATH:$DOTNET_ROOT"
dotnet run --project ai-docs/EffSharp.AiDocs.fsproj
```

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
