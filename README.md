# eff-sharp

A native F# implementation of an Effect-style runtime that is compiled with
Fable and shipped to Node/TypeScript consumers.

> **Status: early.** The active build/test target is Node/TypeScript. The .NET SDK
> is used as the F# compiler host for Fable; .NET is not a runtime output target.

## Why a port (not bindings)

F# already ships native equivalents of much of Effect's "data types" layer
(`Option`, `Result` = `Either`, `List`, `Map`, `Set`, `Async`), so those are not
reimplemented. The thing F# *lacks* is a single type carrying all three of
Effect's channels at once:

- `'A` — the success value
- `'E` — the typed (expected) error
- `'R` — the required environment / dependencies (DI)

That is what `eff-sharp` provides.

The one genuine wall is higher-kinded types: Effect's generic typeclass plumbing
(`HKT`, `Covariant`, …) relies on HKTs, which F# does not have. The *concrete*
`Effect<'A,'E,'R>` type and its combinators do not need them, so the port targets
the concrete surface and skips the generic-abstraction layer.

## Example

```fsharp
open Effect

type DivError = DivByZero
type Config = { Factor: int }

let safeDiv a b : Effect<int, DivError, 'R> =
    if b = 0 then Effect.fail DivByZero else Effect.succeed (a / b)

let program : Effect<int, DivError, Config> =
    effect {
        let! cfg = Effect.environment<Config, DivError>
        let! d = safeDiv 100 cfg.Factor
        return d + 1
    }

Effect.run { Factor = 4 } program   // Ok 26
```

## Build & Test

Requires Node/npm and the .NET SDK (10.x) for Fable compilation.

```bash
npm install
npm run check
```

`npm run check` builds the TypeScript package with Fable and runs the F#-authored
Vitest specs on Node.

## Porting status

The source layout mirrors effect-smol: one `src/Effect/<Module>.fs` per upstream
`packages/effect/src/<Module>.ts`. Runtime tests live under `test/*Spec.fs`,
compile to JavaScript with Fable, and run with Vitest on Node.

## Roadmap

| Slice | Scope | Status |
|------|-------|--------|
| 1 | Core `Effect<'A,'E,'R>`: `succeed`/`fail`/`sync`/`map`/`flatMap`/`catchAll`, `effect { }` CE | ✅ done |
| 2 | `Cause` / `Exit` — defects vs. typed failures | ✅ done |
| 3 | Async core — `runPromise` analogue, real I/O | planned |
| 4 | Concurrency — fibers, `fork`, interruption | planned |
| 5 | `Context` / `Layer` — dependency injection layers | planned |
| 6 | `Schedule`, `Stream`, `Schema` | planned |

## License

MIT
