#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$repo_root"

bash tools/pack/fable-packages.sh

tmp_dir="$(mktemp -d "${TMPDIR:-/tmp}/eff-sharp-fable-consumer.XXXXXX")"
trap 'rm -rf "$tmp_dir"' EXIT
export NUGET_PACKAGES="$tmp_dir/nuget-packages"

dotnet new console -lang F# -o "$tmp_dir/Consumer" >/dev/null
cd "$tmp_dir/Consumer"

cat > nuget.config <<EOF
<?xml version="1.0" encoding="utf-8"?>
<configuration>
  <packageSources>
    <clear />
    <add key="local-eff-sharp" value="$repo_root/artifacts/nuget" />
    <add key="nuget.org" value="https://api.nuget.org/v3/index.json" />
  </packageSources>
</configuration>
EOF

dotnet add package EffSharp --version 0.0.1-alpha.1 >/dev/null
dotnet add package EffSharp.Platform.Node --version 0.0.1-alpha.1 >/dev/null

cat > Program.fs <<'EOF'
module Program

open Effect
open Effect.Platform.Node

let program () =
    effect {
        let! value = Effect.succeed 41
        return value + 1
    }

let _nodeRuntimeLayer () = NodeRuntime.layer

printfn "%A" (program ())
EOF

mkdir -p .config
cp "$repo_root/.config/dotnet-tools.json" .config/dotnet-tools.json
dotnet tool restore >/dev/null
dotnet tool run fable -- Consumer.fsproj -o "$tmp_dir/out" --noCache

printf '\nVerified EffSharp and EffSharp.Platform.Node install and compile from local NuGet packages.\n'
