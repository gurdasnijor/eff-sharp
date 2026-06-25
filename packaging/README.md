# eff-sharp — JS/TS consumer package (Fable → TypeScript)

This is the **packaging pipeline** that turns the F# eff-sharp library into a
TypeScript package consumable by Node/TS projects (e.g. `fluent-firegrid`).

## How it works
- `EffectJs.fsproj` — the **consumer view** of `src/Effect` plus
  `src/Effect.Platform.Node`: the canonical source lists emitted for JS/TS consumers.
- `dotnet fable EffectJs.fsproj --lang typescript -o dist --optimize` emits the whole
  surface to TypeScript, `--optimize` for leaner output.
- `package.json` exposes it: `import * as Effect from "eff-sharp"` (→ `dist/.../Effect.ts`)
  and `eff-sharp/platform-node/NodeHttpClient` for Node platform modules.

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
- The `Undici` facade uses a dynamic import, so applications that call it should
  install `undici` themselves.
