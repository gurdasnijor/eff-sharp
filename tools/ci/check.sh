#!/usr/bin/env bash
# eff-sharp CI gate.
#
# Runtime target: Node/TypeScript.
# Compiler host: .NET SDK + Fable.
#
# Local / CI:
#   bash tools/ci/check.sh
#
# There is no .NET runtime test target. `npm run check` builds the TS package and
# runs F#-authored Vitest specs against the Fable-generated JavaScript on Node.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$REPO_ROOT"

step() { printf '\n\033[1;34m==> %s\033[0m\n' "$1"; }
fail() { printf '\n\033[1;31mFAILED: %s\033[0m\n' "$1" >&2; exit 1; }

step "Conflict-marker scan (tracked files, excluding repos/)"
if git grep -nE '^(<<<<<<<|=======|>>>>>>>)' -- ':!repos/'; then
  fail "Merge-conflict markers found in tracked files."
fi
echo "ok - no conflict markers."

step "Stub scan (src/Effect/*.fs must not ship placeholders)"
if git grep -nE 'placeholder = \(\)|failwith[[:space:]]+\"not ?implemented\"' -- 'src/Effect/*.fs'; then
  fail "Stub/placeholder implementation found in src/Effect."
fi
echo "ok - no stubs."

step "Install npm workspace dependencies"
npm install --no-fund --no-audit

step "Build TS package and run Node/Vitest specs"
npm run check

printf '\n\033[1;32mNode/Fable gate passed.\033[0m\n'
