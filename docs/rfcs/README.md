# RFCs

Design proposals for the high-impact, high-cost changes identified in
[`../effect4-port-evaluation.md`](../effect4-port-evaluation.md). These are the
items too large or too architecturally significant to land as a drive-by PR — they
need a decision before code.

| RFC | Area | Stack-rank | Status |
|-----|------|-----------|--------|
| [RFC-001](RFC-001-interpreter-loop-core.md) | Interpreter-loop core (microtask scheduler) | #7 | Proposed |
| [RFC-002](RFC-002-typed-R-environment.md) | Typed-`R` environment (Context floor + Orsak layer) | #2 | Proposed |
| [RFC-003](RFC-003-stream-channel.md) | Stream: chunked Channel vs. F#-native `taskSeq` | #9 | Proposed (decision-required) |

The smaller, CI-verifiable items ship directly as code PRs (e.g. parallel
combinators); the cross-runtime performance evidence lives in
[`../../benchmarks/cross-runtime`](../../benchmarks/cross-runtime).
