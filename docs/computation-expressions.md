# Computation expressions in eff-sharp

A design note for how the port should expose Effect's authoring surface through
F# **computation expressions (CEs)** rather than pipe-heavy combinator chains.

## Why this matters

Effect (TypeScript) authors effects with generators:

```ts
Effect.gen(function* () {
  const user = yield* fetchUser(id)
  return render(user)
})
```

It uses generators *because JavaScript has no other way* to get do-notation. F#
has first-class computation expressions — a strictly more capable mechanism
(`let!`, `and!`, `use!`, `yield`, `for`, `try/finally`). So in several places the
F# port can be **more ergonomic than the original**. The guiding rule:

> Prefer a computation expression over a pipe-combinator chain wherever F# syntax
> (`yield`, `and!`, `use!`, `for`, `try/finally`) expresses the intent natively.

References:
- CE language reference: <https://learn.microsoft.com/dotnet/fsharp/language-reference/computation-expressions>
- Local mirror: `repos/fsharp/docs/`

## The builders we will ship

| Builder | Surface | Replaces (Effect) | Lands in |
|---------|---------|-------------------|----------|
| `effect { }` | `let!`/`return`/`return!`/`for`/`while`/`use!`/`try` | `Effect.gen`, `flatMap`, `acquireRelease` | slices 3–4 (extend existing) |
| `stream { }` | `yield`/`yield!`/`for`/`while` | `Stream.make`/`concat`/`fromIterable` | slice 6 |
| applicative `and!` on `effect { }` | `let! … and! …` | `Effect.all({…}, {concurrency})` | slice 4 |

We do **not** reimplement `option { }` / `result { }` — FsToolkit.ErrorHandling
already ships those. Only Effect-specific builders are in scope.

---

## 1. `stream { yield … }` — the headline win (slice 6)

`yield`/`yield!` is the natural way to author a stream; far cleaner than chained
constructors.

```fsharp
type StreamBuilder() =
    member _.Yield(x)       = Stream.succeed x          // yield value
    member _.YieldFrom(s)   = s                          // yield! anotherStream
    member _.Zero()         = Stream.empty
    member _.Combine(a, b)  = Stream.concat a b          // two statements in sequence
    member _.Delay(f)       = Stream.suspend f
    member _.For(xs, f)     = Stream.flatMap f (Stream.fromSeq xs)
    member _.While(g, body) = Stream.whileLoop g body

let stream = StreamBuilder()

stream {
    yield 1
    yield 2
    yield! Stream.range 3 5
    for x in [ 6; 7 ] do
        yield x * 10                                      // lowers to flatMap
}
```

`Combine` + `Delay` + `Zero` are what make multiple `yield` statements and `for`
loops compose. Effectful, pull-based emission stays lazy via `Delay`/`suspend`.

---

## 2. Applicative `let! … and! …` — parallelism as syntax (slice 4)

F#'s `and!` desugars through `MergeSources`/`BindReturn`. Wiring it to a parallel
combinator makes concurrency a one-keyword change:

```fsharp
type EffectBuilder with
    member _.MergeSources(a, b) = Effect.zipPar a b      // runs the two concurrently
    member _.BindReturn(x, f)   = Effect.map f x

effect {
    let! user  = fetchUser id
    and! prefs = fetchPrefs id        // evaluated in parallel with user
    return render user prefs
}
```

Sequential `let!` stays sequential (depends on `Bind`); `and!` opts into
concurrency. This mirrors Effect's `Effect.all(..., { concurrency: "unbounded" })`
without a separate API — the structure of the code expresses it.

> Depends on the fiber runtime (`Effect.zipPar` = fork both, join both, combine
> exits, propagate the first failure and interrupt the loser).

---

## 3. `use!` — deterministic resource scoping (slice 3→4)

Effect's `acquireRelease` / `Scope` becomes idiomatic `use!` once the builder
registers finalizers via `Using`/`TryFinally`:

```fsharp
effect {
    use! conn = acquireConn          // released on scope exit, success or failure
    let! rows = query conn
    return rows
}
```

`acquireConn : Effect<Resource<Conn>, 'E, 'R>` yields a scoped resource; the
builder's `Using` ensures the finalizer runs exactly once when the surrounding
scope closes (including on error/interruption).

---

## 4. Filling out `effect { }` (do this first — slices 3–4)

The current builder has only `Return`/`ReturnFrom`/`Bind`/`Zero`. The full surface
unlocks #2 and #3 and lets effectful code be written imperatively while staying
pure:

```fsharp
type EffectBuilder with
    member _.Delay(f)              = Effect.suspend f
    member _.Combine(a, b)         = Effect.zipRight b a          // a then b
    member _.For(xs, f)            = Effect.forEach xs f          // sequence over a collection
    member _.While(guard, body)    = Effect.whileLoop guard body
    member _.TryWith(body, handler)= Effect.catchAllDefect handler body
    member _.TryFinally(body, fin) = Effect.ensuring fin body
    member _.Using(res, f)         = Effect.acquireUseRelease res f
```

```fsharp
effect {
    let! items = loadItems
    for item in items do
        do! process item            // For + Zero + Combine
    try
        do! commit
    finally
        do! releaseLock             // TryFinally -> ensuring
}
```

## What we deliberately avoid

- `[<CustomOperation>]` query-style DSLs (e.g. `layer { provide … }`): possible,
  but usually less readable than plain functions. Add only if a real call site
  clearly benefits — never preemptively.
- Re-deriving FSharp.Core / FsToolkit builders (`option`, `result`, `asyncResult`).

## Implementation order (folded into slice briefs)

1. **Slices 3–4** — extend `effect { }` to the full builder (#4), then `use!`
   (#3) once `Scope` exists, then `and!` (#2) once fibers exist.
2. **Slice 6** — ship `stream { }` (#1) as the primary `Stream` authoring API,
   with the combinator functions underneath it.

Each future slice that owns one of these modules must treat the corresponding CE
as part of its definition of done, with tests that exercise the sugar (not just
the underlying combinators).
