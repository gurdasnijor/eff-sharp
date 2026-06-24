// Stock (published) Effect micro-bench — the JS counterpart to the F#
// BenchmarkDotNet `RuntimeBench`. The four workloads and N values here MUST match
// benchmarks/Effect.Benchmarks/RuntimeBenchmarks.fs exactly so RESULTS.md is fair.
//
//   effect: 4.0.0-beta.87 (same version the porting reference repo uses)
//   timing: tinybench (ops/s + mean)
//
// Run: `node bench.mjs`
import { Effect, Ref, Fiber } from "effect"
import { Bench } from "tinybench"

// Keep identical to RuntimeBenchmarks.fs.
const BIND_N = 10000
const MAP_N = 10000
const REF_N = 10000
const FORK_N = 1000

// --- build programs once (matches the F# GlobalSetup) ---

// bind throughput: a left-nested chain of N flatMaps.
let bindProgram = Effect.succeed(0)
for (let i = 0; i < BIND_N; i++) {
  bindProgram = Effect.flatMap(bindProgram, (x) => Effect.succeed(x + 1))
}

// succeed/map: N maps over a succeed.
let mapProgram = Effect.succeed(0)
for (let i = 0; i < MAP_N; i++) {
  mapProgram = Effect.map(mapProgram, (x) => x + 1)
}

// Ref: N update/get pairs in one effect.
const refProgram = Effect.gen(function* () {
  const r = yield* Ref.make(0)
  for (let i = 0; i < REF_N; i++) {
    yield* Ref.update(r, (x) => x + 1)
    yield* Ref.get(r)
  }
  return yield* Ref.get(r)
})

// fork/join: fork + join a trivial fiber, N times.
// `forkChild` is beta.87's "fork a child fiber in the current scope" (the closest
// analogue to the port's `Effect.fork`). Fibers require the async runtime, so this
// workload runs via `runPromise` (see RESULTS.md caveat).
const forkProgram = Effect.gen(function* () {
  for (let i = 0; i < FORK_N; i++) {
    const fib = yield* Effect.forkChild(Effect.succeed(1))
    yield* Fiber.join(fib)
  }
})

// --- sanity check before timing ---
const bindOk = Effect.runSync(bindProgram)
const mapOk = Effect.runSync(mapProgram)
const refOk = Effect.runSync(refProgram)
const forkOk = await Effect.runPromise(forkProgram)
if (bindOk !== BIND_N || mapOk !== MAP_N || refOk !== REF_N) {
  throw new Error(`sanity failed: bind=${bindOk} map=${mapOk} ref=${refOk} fork=${forkOk}`)
}
console.log(`sanity ok: bind=${bindOk} map=${mapOk} ref=${refOk} (fork ran ${FORK_N}x)`)

// --- timing ---
const bench = new Bench({ time: 1500, warmupTime: 300 })

bench.add("bind throughput (10k flatMap)", () => {
  Effect.runSync(bindProgram)
})
bench.add("succeed/map (10k map)", () => {
  Effect.runSync(mapProgram)
})
bench.add("Ref update/get (10k)", () => {
  Effect.runSync(refProgram)
})
bench.add("fork/join (1k)", async () => {
  await Effect.runPromise(forkProgram)
})

await bench.run()

// Report ops/s + mean (ms) + mean (ns/op).
const rows = bench.tasks.map((t) => {
  const r = t.result
  return {
    workload: t.name,
    "ops/s": r.throughput.mean.toFixed(1),
    "mean (ms)": r.latency.mean.toFixed(4),
    "mean (ns/op)": (r.latency.mean * 1e6).toFixed(0),
    "rme %": r.latency.rme.toFixed(2),
    samples: r.latency.samplesCount,
  }
})
console.table(rows)
