#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$repo_root"

out_dir="$repo_root/artifacts/nuget"
rm -rf "$out_dir"
mkdir -p "$out_dir"

dotnet pack src/Effect/EffSharp.fsproj -c Release -o "$out_dir"
dotnet pack src/Effect.Platform.Node/EffSharp.Platform.Node.fsproj -c Release -o "$out_dir"

printf '\nPacked Fable packages:\n'
find "$out_dir" -maxdepth 1 -name '*.nupkg' -print | sort
