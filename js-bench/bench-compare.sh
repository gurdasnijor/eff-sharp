#!/usr/bin/env bash
# Fair same-runtime benchmark: eff-sharp's Fable-generated JS vs stock Effect,
# both on Node. Compiles our workloads through Fable, then runs compare.mjs.
#
# Usage:  ./bench-compare.sh
set -euo pipefail
cd "$(dirname "$0")"

export DOTNET_ROOT="${DOTNET_ROOT:-/usr/local/share/dotnet}"
export PATH="$PATH:$DOTNET_ROOT:$HOME/.dotnet/tools"

echo "==> compiling eff-sharp workloads to JS (Fable)"
( cd .. && dotnet tool restore >/dev/null 2>&1 || true )
dotnet fable port/Port.Bench.fsproj --lang javascript -o port/out --noCache

echo "==> installing JS deps (effect + tinybench)"
[ -d node_modules ] || npm install --silent

echo "==> running comparison"
node compare.mjs

echo "==> wrote js-bench/comparison.md"
