#!/usr/bin/env bash
# Fable compatibility coverage map for src/Effect.
#
# Compiles the full-tree probe (fable-spike/full/EffectFull.fsproj, which
# references every src/Effect file) through Fable and reports, per source file:
#   - true `error FABLE` count (hard blockers)
#   - `warning FABLE` count (semantic caveats, e.g. Async.Start->StartImmediate)
# plus a root-cause histogram of the unsupported APIs.
#
# Three gotchas this encodes so the numbers are trustworthy:
#   1. Fable caches aggressively -> always pass --noCache, else you get zeros.
#   2. Output is ANSI-colored -> strip escapes before grepping.
#   3. "error" and "warning" are both "FABLE:" lines -> count them separately.
#
# Usage:  ./scripts/fable-coverage.sh
# Env:    PROBE=path/to/some.fsproj   to point at a different probe project.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PROBE="${PROBE:-$ROOT/fable-spike/full/EffectFull.fsproj}"
RAW="$(mktemp)"
CLEAN="$(mktemp)"
trap 'rm -f "$RAW" "$CLEAN"' EXIT

# Make `dotnet fable` reachable even from a bare shell.
export DOTNET_ROOT="${DOTNET_ROOT:-/usr/local/share/dotnet}"
export PATH="$PATH:$DOTNET_ROOT:$HOME/.dotnet/tools"

echo "Compiling $PROBE (Fable, --noCache)..." >&2
# Don't let a non-zero Fable exit (it returns 1 when there are errors) kill the script.
( cd "$(dirname "$PROBE")" && dotnet fable "$(basename "$PROBE")" --lang javascript -o out --noCache ) >"$RAW" 2>&1 || true

# Strip ANSI, keep only file-anchored diagnostic lines.
sed -E 's/\x1b\[[0-9;]*m//g' "$RAW" | grep -E '\.fs\([0-9]+,[0-9]+\):' >"$CLEAN" || true

ERR=$(grep -c 'error FABLE'   "$CLEAN" || true)
WARN=$(grep -c 'warning FABLE' "$CLEAN" || true)

echo
echo "=== Fable coverage: $ERR errors, $WARN warnings ==="
echo
printf '%-26s %6s %8s\n' 'file' 'errors' 'warnings'
printf '%-26s %6s %8s\n' '----' '------' '--------'

# Per-file: join error and warning counts. List every file that has either.
{ grep 'error FABLE' "$CLEAN" | grep -oE 'Effect/[A-Za-z0-9]+\.fs' | sed 's#Effect/##' | sort | uniq -c \
    | awk '{print $2"\te\t"$1}'
  grep 'warning FABLE' "$CLEAN" | grep -oE 'Effect/[A-Za-z0-9]+\.fs' | sed 's#Effect/##' | sort | uniq -c \
    | awk '{print $2"\tw\t"$1}'
} | awk -F'\t' '
    { if ($2=="e") err[$1]=$3; else warn[$1]=$3; seen[$1]=1 }
    END {
      for (f in seen) printf "%s\t%d\t%d\n", f, err[f]+0, warn[f]+0
    }' \
  | sort -t$'\t' -k2,2nr -k3,3nr \
  | awk -F'\t' '{ printf "%-26s %6d %8d\n", $1, $2, $3 }'

echo
echo "=== top unsupported APIs (errors only) ==="
grep 'error FABLE' "$CLEAN" \
  | grep -oE 'FABLE: [A-Za-z0-9.`]+' \
  | sed -E 's/`[0-9]+//g; s/^FABLE: //' \
  | sort | uniq -c | sort -rn | head -15
