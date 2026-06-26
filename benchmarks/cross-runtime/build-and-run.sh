#!/usr/bin/env bash
# Full cross-runtime benchmark on a machine with the .NET SDK + Node.
# Builds the eff-sharp Fable output, runs both sides, writes RESULTS.md.
set -euo pipefail
cd "$(dirname "$0")"

echo "==> npm install (effect + tinybench)"
npm install --silent

echo "==> dotnet tool restore (Fable)"
( cd ../.. && dotnet tool restore )

echo "==> Fable build eff-sharp workloads -> dist/"
dotnet fable EffSharpBench.fsproj --lang javascript -o dist

echo "==> bench: upstream effect"
node upstream.bench.mjs

echo "==> bench: eff-sharp"
node effsharp.bench.mjs

echo "==> reconcile -> RESULTS.md"
node run-all.mjs
