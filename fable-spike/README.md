# fable-spike — eff-sharp → Fable → Node feasibility spike

P0 go/no-go spike. **Full report: [`../docs/fable-feasibility.md`](../docs/fable-feasibility.md).**
**Verdict: GO (conditional)** — the Effect core compiles to JS and runs on Node,
including `fork`/`join`; remaining work is a bounded port (no core rewrite).

## Layout
- `EffectSpike.fsproj` — smallest viable slice. Clean files referenced **unmodified**
  from `../src/Effect`; the 3 files needing Fable shims are spike copies in `patched/`
  (every change tagged `// SPIKE:`). The real `src/Effect` tree is untouched.
- `Program.fs` — 7 end-to-end tests (runSync + async path, through fork/join).
- `patched/` — `Effect.fs`, `Fiber.fs`, `Runtime.fs` with minimal Fable-compat shims.
- `full/EffectFull.fsproj` — all 98 core files, compile-only, to harvest the total
  Fable diagnostic surface.
- `full-tree-fable-diagnostics.txt` — captured 114 errors / 57 warnings (committed evidence).
- `node-run-output.txt` — captured `7 passed, 0 failed` Node run (committed evidence).

## Reproduce
```bash
export DOTNET_ROOT=/usr/local/share/dotnet; export PATH="$PATH:$DOTNET_ROOT"
dotnet tool restore                                  # Fable 5.4.0 (pinned in dotnet-tools.json)

# 1) slice → JS → run on Node  (expect: 7 passed, 0 failed)
dotnet fable EffectSpike.fsproj --lang javascript -o out
node out/Program.js

# 2) full-tree scope probe (expect: 114 errors across 31 files)
cd full && dotnet fable EffectFull.fsproj --lang javascript -o out --noCache
```
Tested with Fable 5.4.0, .NET SDK 10.0.301, Node v24.14.1.
