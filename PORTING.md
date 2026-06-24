# Porting manifest

Tracks every `effect` module and test from the vendored upstream
(`repos/effect-smol/packages/effect`) against its port status here.

**Source modules:** 137 &nbsp;|&nbsp; **Upstream test files:** 103

Status legend: ✅ done · 🟡 stub (compiles, not implemented) · ⬜ planned

## Source modules (`src/Effect/*.fs`)

| Module | Status | Slice | Upstream test |
|--------|--------|-------|---------------|
| `Array` | ⬜ planned | — | `Array.test.ts` |
| `BigDecimal` | ⬜ planned | — | `BigDecimal.test.ts` |
| `BigInt` | ⬜ planned | — | `BigInt.test.ts` |
| `Boolean` | ⬜ planned | — | `Boolean.test.ts` |
| `Brand` | ⬜ planned | — | `Brand.test.ts` |
| `Cache` | ⬜ planned | — | `Cache.test.ts` |
| `Cause` | 🟡 stub | 2 | `Cause.test.ts` |
| `Channel` | ⬜ planned | — | `Channel.test.ts` |
| `ChannelSchema` | ⬜ planned | — | — |
| `Chunk` | ⬜ planned | — | `Chunk.test.ts` |
| `Clock` | ⬜ planned | — | — |
| `Combiner` | ⬜ planned | — | `Combiner.test.ts` |
| `Config` | ⬜ planned | — | `Config.test.ts` |
| `ConfigProvider` | ⬜ planned | — | `ConfigProvider.test.ts` |
| `Console` | ⬜ planned | — | — |
| `Context` | 🟡 stub | 5 | — |
| `Cron` | ⬜ planned | — | `Cron.test.ts` |
| `Crypto` | ⬜ planned | — | `Crypto.test.ts` |
| `Data` | ⬜ planned | — | `Data.test.ts` |
| `DateTime` | ⬜ planned | — | `DateTime.test.ts` |
| `Deferred` | ⬜ planned | — | `Deferred.test.ts` |
| `Differ` | ⬜ planned | — | — |
| `Duration` | ⬜ planned | — | `Duration.test.ts` |
| `Effect` | ✅ done | 1 | `Effect.test.ts` |
| `Effectable` | ⬜ planned | — | — |
| `Encoding` | ⬜ planned | — | — |
| `Equal` | ⬜ planned | — | `Equal.test.ts` |
| `Equivalence` | ⬜ planned | — | `Equivalence.test.ts` |
| `ErrorReporter` | ⬜ planned | — | — |
| `ExecutionPlan` | ⬜ planned | — | `ExecutionPlan.test.ts` |
| `Exit` | 🟡 stub | 2 | `Exit.test.ts` |
| `Fiber` | 🟡 stub | 4 | `Fiber.test.ts` |
| `FiberHandle` | ⬜ planned | — | `FiberHandle.test.ts` |
| `FiberMap` | ⬜ planned | — | `FiberMap.test.ts` |
| `FiberSet` | ⬜ planned | — | `FiberSet.test.ts` |
| `FileSystem` | ⬜ planned | — | — |
| `Filter` | ⬜ planned | — | — |
| `Formatter` | ⬜ planned | — | `Formatter.test.ts` |
| `Function` | ⬜ planned | — | `Function.test.ts` |
| `Graph` | ⬜ planned | — | `Graph.test.ts` |
| `Hash` | ⬜ planned | — | — |
| `HashMap` | ⬜ planned | — | `HashMap.test.ts` |
| `HashRing` | ⬜ planned | — | — |
| `HashSet` | ⬜ planned | — | `HashSet.test.ts` |
| `HKT` | ⬜ planned | — | — |
| `index` | ⬜ planned | — | — |
| `Inspectable` | ⬜ planned | — | — |
| `Iterable` | ⬜ planned | — | `Iterable.test.ts` |
| `JsonPatch` | ⬜ planned | — | `JsonPatch.test.ts` |
| `JsonPointer` | ⬜ planned | — | `JsonPointer.test.ts` |
| `JsonSchema` | ⬜ planned | — | `JsonSchema.test.ts` |
| `Latch` | ⬜ planned | — | `Latch.test.ts` |
| `Layer` | 🟡 stub | 5 | `Layer.test.ts` |
| `LayerMap` | ⬜ planned | — | `LayerMap.test.ts` |
| `Logger` | ⬜ planned | — | `Logger.test.ts` |
| `LogLevel` | ⬜ planned | — | `LogLevel.test.ts` |
| `ManagedRuntime` | ⬜ planned | — | `ManagedRuntime.test.ts` |
| `Match` | ⬜ planned | — | `Match.test.ts` |
| `Metric` | ⬜ planned | — | `Metric.test.ts` |
| `MutableHashMap` | ⬜ planned | — | `MutableHashMap.test.ts` |
| `MutableHashSet` | ⬜ planned | — | `MutableHashSet.test.ts` |
| `MutableList` | ⬜ planned | — | `MutableList.test.ts` |
| `MutableRef` | ⬜ planned | — | — |
| `Newtype` | ⬜ planned | — | `Newtype.test.ts` |
| `NonEmptyIterable` | ⬜ planned | — | — |
| `Number` | ⬜ planned | — | `Number.test.ts` |
| `Optic` | ⬜ planned | — | `Optic.test.ts` |
| `Option` | ⬜ planned | — | `Option.test.ts` |
| `Order` | ⬜ planned | — | `Order.test.ts` |
| `Ordering` | ⬜ planned | — | `Ordering.test.ts` |
| `PartitionedSemaphore` | ⬜ planned | — | `PartitionedSemaphore.test.ts` |
| `Path` | ⬜ planned | — | — |
| `Pipeable` | ⬜ planned | — | — |
| `PlatformError` | ⬜ planned | — | — |
| `Pool` | ⬜ planned | — | `Pool.test.ts` |
| `Predicate` | ⬜ planned | — | `Predicate.test.ts` |
| `PrimaryKey` | ⬜ planned | — | — |
| `PubSub` | ⬜ planned | — | `PubSub.test.ts` |
| `Pull` | ⬜ planned | — | — |
| `Queue` | ⬜ planned | — | `Queue.test.ts` |
| `Random` | ⬜ planned | — | `Random.test.ts` |
| `RcMap` | ⬜ planned | — | `RcMap.test.ts` |
| `RcRef` | ⬜ planned | — | `RcRef.test.ts` |
| `Record` | ⬜ planned | — | `Record.test.ts` |
| `Redactable` | ⬜ planned | — | — |
| `Redacted` | ⬜ planned | — | `Redacted.test.ts` |
| `Reducer` | ⬜ planned | — | `Reducer.test.ts` |
| `Ref` | ⬜ planned | — | `Ref.test.ts` |
| `References` | ⬜ planned | — | — |
| `RegExp` | ⬜ planned | — | — |
| `Request` | ⬜ planned | — | `Request.test.ts` |
| `RequestResolver` | ⬜ planned | — | — |
| `Resource` | ⬜ planned | — | `Resource.test.ts` |
| `Result` | ⬜ planned | — | `Result.test.ts` |
| `Runtime` | 🟡 stub | 4 | — |
| `Schedule` | 🟡 stub | 6 | `Schedule.test.ts` |
| `Scheduler` | ⬜ planned | — | `Scheduler.test.ts` |
| `Schema` | 🟡 stub | 6 | — |
| `SchemaAST` | ⬜ planned | — | — |
| `SchemaGetter` | ⬜ planned | — | — |
| `SchemaIssue` | ⬜ planned | — | — |
| `SchemaParser` | ⬜ planned | — | — |
| `SchemaRepresentation` | ⬜ planned | — | — |
| `SchemaTransformation` | ⬜ planned | — | — |
| `SchemaUtils` | ⬜ planned | — | — |
| `Scope` | 🟡 stub | 3 | `Scope.test.ts` |
| `ScopedCache` | ⬜ planned | — | `ScopedCache.test.ts` |
| `ScopedRef` | ⬜ planned | — | `ScopedRef.test.ts` |
| `Semaphore` | ⬜ planned | — | `Semaphore.test.ts` |
| `Sink` | ⬜ planned | — | `Sink.test.ts` |
| `Stdio` | ⬜ planned | — | — |
| `Stream` | 🟡 stub | 6 | `Stream.test.ts` |
| `String` | ⬜ planned | — | `String.test.ts` |
| `Struct` | ⬜ planned | — | `Struct.test.ts` |
| `SubscriptionRef` | ⬜ planned | — | `SubscriptionRef.test.ts` |
| `Symbol` | ⬜ planned | — | `Symbol.test.ts` |
| `SynchronizedRef` | ⬜ planned | — | `SynchronizedRef.test.ts` |
| `Take` | ⬜ planned | — | — |
| `Terminal` | ⬜ planned | — | — |
| `Tracer` | ⬜ planned | — | `Tracer.test.ts` |
| `Trie` | ⬜ planned | — | `Trie.test.ts` |
| `Tuple` | ⬜ planned | — | `Tuple.test.ts` |
| `TxChunk` | ⬜ planned | — | `TxChunk.test.ts` |
| `TxDeferred` | ⬜ planned | — | `TxDeferred.test.ts` |
| `TxHashMap` | ⬜ planned | — | `TxHashMap.test.ts` |
| `TxHashSet` | ⬜ planned | — | `TxHashSet.test.ts` |
| `TxPriorityQueue` | ⬜ planned | — | `TxPriorityQueue.test.ts` |
| `TxPubSub` | ⬜ planned | — | `TxPubSub.test.ts` |
| `TxQueue` | ⬜ planned | — | `TxQueue.test.ts` |
| `TxReentrantLock` | ⬜ planned | — | `TxReentrantLock.test.ts` |
| `TxRef` | ⬜ planned | — | — |
| `TxSemaphore` | ⬜ planned | — | `TxSemaphore.test.ts` |
| `TxSubscriptionRef` | ⬜ planned | — | `TxSubscriptionRef.test.ts` |
| `Types` | ⬜ planned | — | — |
| `UndefinedOr` | ⬜ planned | — | `UndefinedOr.test.ts` |
| `Unify` | ⬜ planned | — | — |
| `Utils` | ⬜ planned | — | — |

## Upstream test files without a 1:1 source module

- `AtomRef.test.ts`
- `EffectEager.test.ts`
- `EffectKeepAlive.test.ts`
- `HttpClient.test.ts`
- `Migrator.test.ts`
- `Pathfinding.test.ts`
- `StackTraceLimit.test.ts`
- `TestClock.test.ts`
