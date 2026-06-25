#!/usr/bin/env bash
# eff-sharp CI gate — the single source of truth for "is this tree OK?".
# Runs in GitHub Actions (.github/workflows/ci.yml) AND can be run by hand from
# the repo root:  bash tools/ci/check.sh
#
# Steps run in cheapest-to-most-expensive order so failures surface fast:
#   1. conflict-marker scan      4. dotnet test  <-- THE GATE
#   2. stub scan                 5. format check (Fantomas, changed files)
#   3. build first-party projs   6. lint (FSharpLint)
#
# Every step fails the whole script (set -e) with a clearly labelled message.
# eff-sharp is an F# port of Effect-TS whose runtime target is Node/TS via Fable;
# its *build/test* toolchain is the .NET SDK, which is all this script touches.
# It NEVER restores or builds anything under repos/ (vendored Fable + F# compiler).
set -euo pipefail

# Always operate from the repo root (the dir above tools/).
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$REPO_ROOT"

# First-party projects — explicit paths only, never a recursive build that could
# wander into repos/.
LIB="src/Effect/Effect.fsproj"
TESTS="tests/Effect.Tests/Effect.Tests.fsproj"
AIDOCS="ai-docs/EffSharp.AiDocs.fsproj"
BENCH="benchmarks/Effect.Benchmarks/Effect.Benchmarks.fsproj"
BUILD_PROJECTS=("$LIB" "$TESTS" "$AIDOCS" "$BENCH")

# Dirs Fantomas / FSharpLint care about.
FS_DIRS=(src tests ai-docs benchmarks)

CONFIG="${CONFIG:-Release}"

step() { printf '\n\033[1;34m==> %s\033[0m\n' "$1"; }
fail() { printf '\n\033[1;31mFAILED: %s\033[0m\n' "$1" >&2; exit 1; }

# ---------------------------------------------------------------------------
step "0/6  Restoring local dotnet tools (Fantomas, FSharpLint)"
dotnet tool restore

# ---------------------------------------------------------------------------
step "1/6  Conflict-marker scan (tracked files, excluding repos/)"
# Match true VCS conflict markers at start of line, across all tracked files
# except the vendored repos/ tree.
if git grep -nE '^(<<<<<<<|=======|>>>>>>>)' -- ':!repos/' ; then
  fail "Merge-conflict markers found in tracked files (see matches above)."
fi
echo "ok — no conflict markers."

# ---------------------------------------------------------------------------
step "2/6  Stub scan (src/Effect/*.fs must not ship placeholders)"
# Catches the class of regression that shipped an RcRef stub onto main.
if git grep -nE 'placeholder = \(\)|failwith[[:space:]]+"not ?implemented"' -- 'src/Effect/*.fs' ; then
  fail "Stub/placeholder implementation found in src/Effect (see matches above)."
fi
echo "ok — no stubs."

# ---------------------------------------------------------------------------
step "3/6  Build first-party projects (Release)"
for proj in "${BUILD_PROJECTS[@]}"; do
  echo "--- building $proj"
  dotnet build "$proj" -c "$CONFIG" --nologo \
    || fail "Build failed: $proj"
done
echo "ok — all first-party projects build."

# ---------------------------------------------------------------------------
step "4/6  TEST — full xUnit suite (THE GATE)"
# No --filter: every test runs, including the concurrency/TestClock TTL tests.
# --no-build would risk a stale dll; rebuild of the test proj is cheap after step 3.
dotnet test "$TESTS" -c "$CONFIG" --nologo \
  --logger "console;verbosity=normal" \
  --logger "trx;LogFileName=test-results.trx" \
  --results-directory "$REPO_ROOT/TestResults" \
  || fail "Test suite is RED — see failures above. This is the gate."
echo "ok — full test suite passed."

# ---------------------------------------------------------------------------
step "5/6  Format check (Fantomas) — whole tree"
# The whole tree is Fantomas-formatted (see the one-time normalization commit).
# This enforces it on every file going forward; the style profile lives in
# .editorconfig. Fix locally with: dotnet fantomas src tests ai-docs benchmarks
dotnet fantomas --check src tests ai-docs benchmarks \
  || fail "Fantomas: files need formatting — run 'dotnet fantomas src tests ai-docs benchmarks'."
echo "ok — tree is formatted."

# ---------------------------------------------------------------------------
step "6/6  Lint (FSharpLint) — first-party projects"
lint_failed=0
for proj in "$LIB" "$TESTS" "$AIDOCS" "$BENCH"; do
  echo "--- linting $proj"
  out="$(dotnet fsharplint lint --lint-config fsharplint.json --file-type project "$proj" 2>&1)" || true
  echo "$out" | grep -E '==========' || true
  # FSharpLint exits 0 even with warnings; treat any non-zero "Summary: N warnings" as failure.
  if echo "$out" | grep -qE 'Summary: [1-9][0-9]* warnings'; then
    echo "$out"
    lint_failed=1
  fi
done
[ "$lint_failed" -eq 0 ] || fail "FSharpLint reported warnings (see above)."
echo "ok — lint clean."

# ---------------------------------------------------------------------------
printf '\n\033[1;32mAll checks passed.\033[0m\n'
