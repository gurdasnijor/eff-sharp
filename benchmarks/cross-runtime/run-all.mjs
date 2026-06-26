// Reconcile both sides into a single comparison table + RESULTS.md.
// Run after results-upstream.json and results-effsharp.json exist (run-all.mjs
// reads them; it does not itself build/run the benches — see build-and-run.sh).
import { readFileSync, writeFileSync, existsSync } from "node:fs";

const here = (p) => new URL(p, import.meta.url);
const load = (p) => (existsSync(here(p)) ? JSON.parse(readFileSync(here(p), "utf8")) : null);

const up = load("./results-upstream.json");
const es = load("./results-effsharp.json");

if (!up) {
  console.error("missing results-upstream.json — run `npm run bench:upstream`");
  process.exit(1);
}

const ids = Object.keys(up).filter((k) => k !== "_meta");
const fmt = (n) => Math.round(n).toLocaleString();

const rows = ids.map((id) => {
  const u = up[id];
  const e = es?.[id];
  const ratio = e ? e.meanNs / u.meanNs : null; // how many× slower eff-sharp is
  return { id, u, e, ratio };
});

let md = `# Cross-runtime benchmark results

Upstream: \`effect@${up._meta?.version ?? "?"}\` · eff-sharp: Fable output · Node ${up._meta?.node ?? "?"}.
Runner: async (\`runPromise\` upstream / \`runExit\`+Promise eff-sharp). See \`workloads.md\`.

> "Δ (×slower)" = eff-sharp mean ÷ upstream mean. Higher = eff-sharp slower.
> eff-sharp column is blank until the Fable build is run on a machine with the
> .NET SDK (\`bash build-and-run.sh\`); upstream numbers are captured and committed.

| workload | upstream ops/s | eff-sharp ops/s | upstream ns/op | eff-sharp ns/op | Δ (×slower) |
|----------|---------------:|----------------:|---------------:|----------------:|------------:|
`;

for (const { id, u, e, ratio } of rows) {
  md += `| \`${id}\` | ${fmt(u.hz)} | ${e ? fmt(e.hz) : "—"} | ${fmt(u.meanNs)} | ${e ? fmt(e.meanNs) : "—"} | ${ratio ? ratio.toFixed(1) + "×" : "—"} |\n`;
}

if (es) {
  const ratios = rows.filter((r) => r.ratio).map((r) => r.ratio);
  const gmean = Math.exp(ratios.reduce((a, b) => a + Math.log(b), 0) / ratios.length);
  md += `\n**Geometric-mean slowdown: ${gmean.toFixed(1)}×** across ${ratios.length} workloads.\n`;
}

writeFileSync(here("./RESULTS.md"), md);
console.log(md);
console.log("wrote RESULTS.md");
