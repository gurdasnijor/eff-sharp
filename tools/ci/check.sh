#!/usr/bin/env bash
# eff-sharp CI gate — the single source of truth for "is this tree OK?".
#
# Local:  bash tools/ci/check.sh            # runs EVERYTHING (fast gate + quality)
# CI:     splits into two parallel jobs that call:
#           bash tools/ci/check.sh fast     # the gate: scans + build(lib+tests) + xUnit
#           bash tools/ci/check.sh quality  # format + lint + aux build-break check
#
# Splitting test from quality makes wall-clock = max(test, quality) instead of the
# sum, and gets the signal that matters (the xUnit suite) back fast. The fast gate
# does ONE restore + ONE build of lib+tests, then `dotnet test --no-build`, so the
# 120-file test project is compiled once per run, not 2-3×.
#
# eff-sharp is an F# port of Effect-TS whose runtime target is Node/TS via Fable;
# its build/test toolchain is the .NET SDK, which is all this script touches. It
# NEVER restores or builds anything under repos/ (vendored Fable + F# compiler).
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$REPO_ROOT"

LIB="src/Effect/Effect.fsproj"
TESTS="tests/Effect.Tests/Effect.Tests.fsproj"        # the test suite — THE GATE
AIDOCS="ai-docs/EffSharp.AiDocs.fsproj"               # Exe, build-only (not in .slnx)
BENCH="benchmarks/Effect.Benchmarks/Effect.Benchmarks.fsproj"  # build-only
CONFIG="${CONFIG:-Release}"
STAGE="${1:-all}"   # all | fast | quality

step() { printf '\n\033[1;34m==> %s\033[0m\n' "$1"; }
fail() { printf '\n\033[1;31mFAILED: %s\033[0m\n' "$1" >&2; exit 1; }

# --- fast gate -------------------------------------------------------------
scans() {
  step "Conflict-marker scan (tracked files, excluding repos/)"
  if git grep -nE '^(<<<<<<<|=======|>>>>>>>)' -- ':!repos/'; then
    fail "Merge-conflict markers found in tracked files (see matches above)."
  fi
  echo "ok — no conflict markers."

  step "Stub scan (src/Effect/*.fs must not ship placeholders)"
  if git grep -nE 'placeholder = \(\)|failwith[[:space:]]+"not ?implemented"' -- 'src/Effect/*.fs'; then
    fail "Stub/placeholder implementation found in src/Effect (see matches above)."
  fi
  echo "ok — no stubs."
}

build_gate() {
  step "Restore lib+tests graph (once)"
  dotnet restore "$TESTS" || fail "Restore failed."
  step "Build lib + tests ($CONFIG, --no-restore) — single FSC pass"
  # Building the test project transitively builds the library it references.
  dotnet build "$TESTS" -c "$CONFIG" --no-restore --nologo || fail "Build failed (lib+tests)."
  echo "ok — lib+tests build."
}

run_tests() {
  step "TEST — full xUnit suite (THE GATE) — --no-build --no-restore"
  # No --filter: every test runs, including the concurrency / TestClock TTL tests.
  dotnet test "$TESTS" -c "$CONFIG" --no-build --no-restore --nologo \
    --logger "console;verbosity=normal" \
    --logger "trx;LogFileName=test-results.trx" \
    --results-directory "$REPO_ROOT/TestResults" \
    || fail "Test suite is RED — see failures above. This is the gate."
  echo "ok — full test suite passed."
}

# --- quality ---------------------------------------------------------------
quality() {
  step "Restore local dotnet tools (Fantomas, FSharpLint)"
  dotnet tool restore

  step "Format check (Fantomas)"
  # On a PR: check only the .fs a change touches, diffed against the merge-base
  # with the PR base (PR_BASE). Anywhere else (push to main, local): whole tree.
  base=""
  if [ -n "${PR_BASE:-}" ] && git rev-parse --verify --quiet "origin/${PR_BASE}" >/dev/null; then
    base="origin/${PR_BASE}"
  fi
  if [ -n "$base" ]; then
    mapfile -t changed < <(git diff --name-only --diff-filter=ACMR --merge-base "$base" HEAD \
                             -- '*.fs' '*.fsi' '*.fsx' ':!repos/')
    if [ "${#changed[@]}" -eq 0 ]; then
      echo "ok — no F# changes vs $base."
    else
      echo "checking formatting of:"; printf '  %s\n' "${changed[@]}"
      dotnet fantomas --check "${changed[@]}" \
        || fail "Fantomas: run 'dotnet fantomas ${changed[*]}'."
      echo "ok — changed files are formatted."
    fi
  else
    dotnet fantomas --check src tests ai-docs benchmarks \
      || fail "Fantomas: run 'dotnet fantomas src tests ai-docs benchmarks'."
    echo "ok — tree is formatted."
  fi

  step "Lint (FSharpLint) — src/Effect (shipped surface)"
  # Lint's value is in the shipped library; FSharpLint is the slowest tool and
  # exits 0 even with warnings, so we parse the summary for the verdict.
  out="$(dotnet fsharplint lint --lint-config fsharplint.json --file-type project "$LIB" 2>&1)" || true
  echo "$out" | grep -E '==========' || true
  if echo "$out" | grep -qE 'Summary: [1-9][0-9]* warnings'; then
    echo "$out"
    fail "FSharpLint reported warnings (see above)."
  fi
  echo "ok — lint clean."

  step "Build-break check: ai-docs + benchmarks (off the gate's critical path)"
  dotnet build "$AIDOCS" -c "$CONFIG" --nologo || fail "Build failed: $AIDOCS"
  dotnet build "$BENCH"  -c "$CONFIG" --nologo || fail "Build failed: $BENCH"
  echo "ok — ai-docs + benchmarks build."
}

case "$STAGE" in
  fast)    scans; build_gate; run_tests ;;
  quality) quality ;;
  all)     scans; build_gate; run_tests; quality ;;
  *)       fail "unknown stage '$STAGE' (use: fast | quality | all)" ;;
esac

printf '\n\033[1;32mStage '%s' passed.\033[0m\n' "$STAGE"
