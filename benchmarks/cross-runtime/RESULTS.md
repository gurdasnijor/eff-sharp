# Cross-runtime benchmark results

Upstream: `effect@3.21.4` · eff-sharp: Fable output · Node v22.22.2.
Runner: async (`runPromise` upstream / `runExit`+Promise eff-sharp). See `workloads.md`.

> "Δ (×slower)" = eff-sharp mean ÷ upstream mean. Higher = eff-sharp slower.
> eff-sharp column is blank until the Fable build is run on a machine with the
> .NET SDK (`bash build-and-run.sh`); upstream numbers are captured and committed.

| workload | upstream ops/s | eff-sharp ops/s | upstream ns/op | eff-sharp ns/op | Δ (×slower) |
|----------|---------------:|----------------:|---------------:|----------------:|------------:|
| `map_chain` | 10,623 | — | 104,740 | — | — |
| `flatMap_chain` | 9,307 | — | 115,064 | — | — |
| `forEach` | 12,528 | — | 87,830 | — | — |
| `deep_bind` | 1,736 | — | 594,764 | — | — |
| `error_catch` | 2,766 | — | 377,809 | — | — |
