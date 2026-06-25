# eff-sharp — JS/TS consumer package (Fable → TypeScript)

This is the **packaging pipeline** that turns the F# eff-sharp library into a
TypeScript package consumable by Node/TS projects (e.g. `fluent-firegrid`).

## How it works
- `EffectJs.fsproj` — the **consumer view** of `src/Effect`: every module except the
  STM `Tx*` family (depends on `TxRef`, not yet Fable-ready) and the `.NET`-only
  `TestClock` helper (`#if !FABLE_COMPILER`).
- `dotnet fable EffectJs.fsproj --lang typescript -o dist --optimize` emits the whole
  surface to TypeScript (91 modules + the Fable runtime), `--optimize` for leaner output.
- `package.json` exposes it: `import * as Effect from "eff-sharp"` (→ `dist/.../Effect.ts`).

## Proof (runs on Node today)
```bash
dotnet tool restore
dotnet fable EffectJs.fsproj --lang typescript -o dist --optimize   # 0 errors, 91 modules
npx tsx consume.ts                                                  # PASS: ... -> 20
```
`consume.ts` imports the emitted `Effect_succeed`/`Effect_map`/`Effect_flatMap`/`Effect_runSync`
and runs a real effect on Node. Module functions are emitted as `<Module>_<fn>` (tupled args).

## Notes / next
- Consume via a real TS toolchain (`tsx`/`tsc`/bundler) — Fable's emit mixes type+value
  imports without `import type`, which Node's raw `--experimental-strip-types` rejects but
  `tsc`/esbuild handle (as fluent-firegrid's build will).
- When `TxRef` lands on Fable, add the `Tx*` family back to `EffectJs.fsproj`.
- Wire `Effect.Platform.Node` in once its Node impls are consumer-ready.
