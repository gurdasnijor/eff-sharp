# P0 SPIKE: Can eff-sharp (F#) compile via Fable and run on Node? Go / no-go + path

You are an autonomous agent doing a **feasibility spike**. **Work ONLY inside this
worktree** (`/Users/gnijor/gurdasnijor/eff-sharp-wt-fable`, branch `spike/fable-node`).
NEVER touch the main checkout (`~/gurdasnijor/eff-sharp`) or any other
`eff-sharp-wt-*` worktree. NEVER `git checkout`/`switch`/`fetch` other branches.
NEVER touch anything under `repos/` (vendored Fable + F# compiler source — do NOT
build it; use the PUBLISHED `fable` dotnet tool instead).

## The strategic why (P0)
eff-sharp is a native F# port of Effect-TS. Its **whole purpose** is to be consumed
by Node/TypeScript projects — specifically `fluent-firegrid`, which today depends on
the `effect` npm package. For that to happen, eff-sharp's F# must compile to
JavaScript via **Fable** and run on **Node**. Today the repo is pure .NET (`net10.0`,
xUnit) — **there is no Fable/JS pipeline at all.** This spike determines whether one
is achievable and what it costs. This is the highest-priority architectural question
in the project. Do NOT over-promise; deliver an evidence-backed go/no-go.

## The key risk to resolve
JS is **single-threaded**; eff-sharp was built for .NET. Reconnaissance of `src/Effect`:
- **Good sign:** no raw OS threads are spawned (`new Thread(...)` = 0 occurrences).
  The runtime is built on F# `Async` (~10 files), which Fable compiles to JS.
- **To resolve:** `lock` (~23 files), `Interlocked`/`Volatile` (the STM seqlock in
  `TxRef.fs`/`TxReentrantLock.fs`), `Monitor`, `SemaphoreSlim`, `Task.Delay`,
  `CancellationToken` (~12 files use `System.Threading`). In single-threaded JS these
  guards against real parallelism are *semantically* safe no-ops — BUT Fable supports
  only a **subset** of the BCL. The question is whether Fable can compile these APIs
  (even as no-ops/shims) or whether they're hard blockers needing refactors.

## What to produce (deliverables)

### 1. A working (or failing-with-evidence) Fable compile of the core
- Install the published Fable tool (`dotnet tool install fable` via a local tool
  manifest in THIS worktree). Confirm version.
- Create a spike project under `fable-spike/` that references the **smallest viable
  slice** of the eff-sharp core needed to evaluate an effect end-to-end — start with
  `Effect.fs` and only its hard dependencies (e.g. `Cause`, `Exit`, `Context`,
  `Duration` — follow the compile errors to find the closure). You may use a trimmed
  `.fsproj` that includes just those files; do NOT modify the real `src/Effect`
  fsproj/files (copy or include-by-path read-only — keep the spike additive).
- Run `dotnet fable` on it targeting JS. **Capture every Fable error / "unsupported
  API" diagnostic** verbatim.

### 2. Prove execution on Node (or document precisely where it breaks)
- Get a trivial program running through transpiled JS on Node v24:
  `Effect.succeed 1 |> Effect.map (+1) |> Effect.runSync` → prints/asserts `2`.
- Then escalate as far as you can: `fail`/`catchAll`, a `flatMap` chain, `Ref`
  get/update, and — the real test — `fork`/`join` (does the `Async`-based fiber model
  actually run on Node's single-threaded event loop?). Note exactly where it stops.

### 3. The incompatibility inventory + go/no-go report → `docs/fable-feasibility.md`
- Table: every Fable-incompatible API encountered → **module** → **severity**:
  `trivial` (Fable no-op/one-line shim) · `refactor` (small local change) ·
  `fundamental` (needs an architecture change, e.g. cooperative scheduler).
- Per-subsystem verdict: which of core Effect / Ref / Fiber / STM / Stream / Layer /
  Schema compile & run on Node today, which need work.
- **Go/no-go** with a recommended phased path. If the fiber runtime needs a
  cooperative single-threaded scheduler variant for JS (likely), say so and sketch it
  (this is exactly what Effect-TS does). Estimate scope honestly.

## Environment
- .NET SDK at `/usr/local/share/dotnet`. Always APPEND PATH (prepending drops `git`):
  `export DOTNET_ROOT=/usr/local/share/dotnet; export PATH="$PATH:$DOTNET_ROOT"`.
- Node: `node` v24.14.1 is on PATH (via nvm). npm available.
- Use the published `fable` tool; do NOT compile `repos/fable`.

## Workflow
- Commit your spike code + `docs/fable-feasibility.md`. End commit messages with
  `Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>`.
- `git push -u origin spike/fable-node`; open a PR via `gh pr create` (body =
  the go/no-go summary; end with
  `🤖 Generated with [Claude Code](https://claude.com/claude-code)`). **Do NOT merge** —
  a human reviews. The point of the PR is the feasibility report + reproducible spike.

## Reporting discipline
This is a go/no-go for the project's core strategy. Be brutally honest and surface
blockers IMMEDIATELY and prominently — never bury a caveat in a footnote. If it's
infeasible without a fiber-runtime rewrite, say that plainly with the evidence. If
it's more feasible than feared, show the working Node output. Your final message must
state: what compiles, what runs on Node, the worst blocker, and your go/no-go.
