# RFC-003 — Stream: chunked pull Channel vs. F#-native sequences

**Status:** Proposed (decision-required) · **Area:** streaming (stack-rank #9) ·
**Risk:** Very High (full Channel) / Low–Medium (TaskSeq route)

## Problem

Upstream Stream is a chunked, pull-based, backpressured **Channel** consumer.
eff-sharp's Stream is an eager element-at-a-time **push-fold**
(`('A -> Effect<unit>) -> Effect<unit>`); Channel is a 61-line same-shape alias;
`Take`/`Pull` exist but are largely disconnected. Consequence: no backpressure, no
chunking, no concurrency (merge/broadcast), and resource-safety only in
`SubscriptionRef`, not general combinators.

## This is a fork in the road, not a single fix

Faithfully porting Channel is ~8.6k LOC upstream — a very large subsystem. But F#
**already ships** async streaming with resource-safe iteration
(`IAsyncEnumerable`/`taskSeq`/AsyncSeq). So the real question is *which model*, and
that's a **consumer-demand** decision, not a fidelity one.

### Option A — `taskSeq`/`IAsyncEnumerable`-backed Stream (recommended default)

Re-base Stream on `IAsyncEnumerable<Chunk<'A>>` (or `taskSeq`), with operators
lowered to the native async-sequence machinery (`use`/`try-finally`/`while` give
real resource safety and demand-driven pull for free).

- **Pros:** native pull + backpressure + finalizers; far less code; idiomatic;
  composes with the rest of the F#/.NET ecosystem; Fable has async-iteration
  interop.
- **Cons:** not API-identical to upstream Stream; concurrency operators (merge,
  broadcast, parN) still need building on top; `taskSeq` cancellation doesn't
  thread into inner tasks (known caveat — wire interruption explicitly).
- **Chunking:** model elements as `Chunk<'A>` at the sequence level to recover
  upstream's batch semantics.

### Option B — full faithful Channel port

Port `Channel` (input + output + Done/Leftover typing + pipeTo/mergeAll) and
re-base Stream/Sink on it.

- **Pros:** maximum fidelity; upstream stream code ports 1:1; needed if you want
  Channel-level protocols (e.g. duplex codecs).
- **Cons:** very large; only justified by streaming-heavy consumers; depends on a
  capable fiber substrate (couples to RFC-001 for real concurrency).

## Recommendation

Default to **Option A** unless a concrete consumer needs Channel-level semantics.
The fluent-firegrid cutover targets are I/O-bound process/HTTP streams that
`taskSeq`-of-`Chunk` serves well; reserve Option B for if/when a duplex-protocol
consumer appears. Either way, first add the small, high-demand combinators the
current push-fold is missing (`Stream.fromQueue`, `toReadableStream`,
`Queue.offerUnsafe` — already on the cutover list) so consumers are unblocked
before the larger re-base.

## Decision needed

Pick A or B before investing — they share little code. This RFC recommends A and
treats B as conditional on demand.

## Sources

- `FSharp.Control.TaskSeq` (+ inner-task cancellation caveat #179):
  https://github.com/fsprojects/FSharp.Control.TaskSeq
- Upstream Stream/Channel size & model: effect-smol `packages/effect/src/Channel.ts`
