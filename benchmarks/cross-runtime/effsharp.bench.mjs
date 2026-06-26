// eff-sharp side of the cross-runtime benchmark. Requires the Fable build first:
//   dotnet fable EffSharpBench.fsproj --lang javascript -o dist
// Run: npm run bench:effsharp  (writes results-effsharp.json)
import { Bench } from "tinybench";
import { writeFileSync } from "node:fs";
import * as W from "./dist/Workloads.js";

const workloads = {
  map_chain: W.mapChain,
  flatMap_chain: W.flatMapChain,
  forEach: W.forEach,
  deep_bind: W.deepBind,
  error_catch: W.errorCatch,
};

const bench = new Bench({ time: 1000, warmupTime: 250 });
for (const [id, fn] of Object.entries(workloads)) bench.add(id, fn);

await bench.run();

const results = { _meta: { runtime: "eff-sharp", node: process.version, runner: "runExit+StartAsPromise" } };
console.log(`eff-sharp (Fable) — ${process.version}`);
for (const t of bench.tasks) {
  const r = t.result;
  results[t.name] = { hz: r.hz, meanNs: r.mean * 1e6, rmePct: r.rme };
  console.log(`  ${t.name.padEnd(16)} ${Math.round(r.hz).toLocaleString().padStart(12)} ops/s   ${(r.mean * 1e6).toFixed(0).padStart(9)} ns/op  ±${r.rme.toFixed(1)}%`);
}
writeFileSync(new URL("./results-effsharp.json", import.meta.url), JSON.stringify(results, null, 2));
console.log("wrote results-effsharp.json");
