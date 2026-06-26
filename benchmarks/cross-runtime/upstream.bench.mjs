// Upstream `effect` (npm) side of the cross-runtime benchmark.
// Run: npm run bench:upstream  (writes results-upstream.json)
// See workloads.md for the shared workload definitions. Both sides run via the
// async runner (runPromise) — the realistic regime, and it avoids a runSync
// deep-chain suspension landmine on the Fable side.
import { Effect } from "effect";
import { Bench } from "tinybench";
import { writeFileSync } from "node:fs";

const N_CHAIN = 1000;
const N_FOREACH = 1000;
const N_DEEP = 5000;
const N_ERR = 1000;

// --- programs built once (measure run cost, not construction) ---

const mapChain = (() => {
  let p = Effect.succeed(0);
  for (let i = 0; i < N_CHAIN; i++) p = Effect.map(p, (x) => x + 1);
  return p;
})();

const flatMapChain = (() => {
  let p = Effect.succeed(0);
  for (let i = 0; i < N_CHAIN; i++) p = Effect.flatMap(p, (x) => Effect.succeed(x + 1));
  return p;
})();

const forEachProg = Effect.forEach(
  Array.from({ length: N_FOREACH }, (_, i) => i),
  (x) => Effect.succeed(x),
);

const deepBind = (() => {
  const count = (n) => (n === 0 ? Effect.succeed(0) : Effect.flatMap(Effect.succeed(n - 1), count));
  return count(N_DEEP);
})();

const errorCatch = (() => {
  let p = Effect.succeed(0);
  for (let i = 0; i < N_ERR; i++) {
    p = Effect.flatMap(p, () => Effect.catchAll(Effect.fail("e"), () => Effect.succeed(1)));
  }
  return p;
})();

const workloads = {
  map_chain: () => Effect.runPromise(mapChain),
  flatMap_chain: () => Effect.runPromise(flatMapChain),
  forEach: () => Effect.runPromise(forEachProg),
  deep_bind: () => Effect.runPromise(deepBind),
  error_catch: () => Effect.runPromise(errorCatch),
};

const bench = new Bench({ time: 1000, warmupTime: 250 });
for (const [id, fn] of Object.entries(workloads)) bench.add(id, fn);

await bench.run();

const results = { _meta: { runtime: "effect", version: "3.21.4", node: process.version, runner: "runPromise" } };
console.log(`upstream effect 3.21.4 — ${process.version} (runPromise)`);
for (const t of bench.tasks) {
  const r = t.result;
  results[t.name] = { hz: r.hz, meanNs: r.mean * 1e6, rmePct: r.rme };
  console.log(`  ${t.name.padEnd(16)} ${Math.round(r.hz).toLocaleString().padStart(12)} ops/s   ${(r.mean * 1e6).toFixed(0).padStart(9)} ns/op  ±${r.rme.toFixed(1)}%`);
}
writeFileSync(new URL("./results-upstream.json", import.meta.url), JSON.stringify(results, null, 2));
console.log("wrote results-upstream.json");
