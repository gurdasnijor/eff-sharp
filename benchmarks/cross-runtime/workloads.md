# Cross-runtime benchmark workloads

Identical logical workloads expressed once per runtime (`upstream.bench.mjs` for
`effect` npm, `effsharp.bench.mjs` for the Fable-compiled eff-sharp output). The
F# side mirrors these exactly; keep the two in lockstep when editing.

Each program is **built once** (outside the timed loop) and **run** inside the
loop, so we measure interpreter *run* cost, not construction. All workloads are
fully synchronous so they can use `runSync` on both sides — this is the regime
where a Reader-over-`Async` core is most penalised vs a defunctionalized
interpreter, which is exactly the delta we want to surface.

| id | description | param |
|----|-------------|-------|
| `map_chain` | `succeed 0 |> map (+1) ` × N | N = 1000 |
| `flatMap_chain` | left-assoc `flatMap (fun x -> succeed (x+1))` × N | N = 1000 |
| `forEach` | `forEach [0..N-1] (succeed << id)` collecting a list | N = 1000 |
| `deep_bind` | recursive `count n = if n=0 then succeed 0 else flatMap (...) (succeed (n-1))` | N = 5000 |
| `error_catch` | `fail e |> catchAll (fun _ -> succeed 1)` repeated via flatMap × N | N = 1000 |

Reported: ops/sec (higher better) and mean ns/op. The headline number is the
**ratio** upstream÷eff-sharp per workload (the "performance delta").

> Caveat: upstream is `effect@3.21.4` (the latest published `effect`), the closest
> installable proxy for effect-smol/Effect 4. The two share the same
> defunctionalized fiber-interpreter architecture, so the order-of-magnitude delta
> vs a Reader-over-`Async` core is representative; treat absolute upstream numbers
> as "this class of runtime", not a pinned effect-smol build.
