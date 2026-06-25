# Why eff-sharp? — F# ergonomics Effect's TypeScript can't match

eff-sharp is a faithful port of [Effect](https://effect.website), but it runs on
F#, a language with first-class **computation expressions**, **pattern matching**,
**nominal records/unions**, and **units of measure**. Where Effect's TypeScript
must thread everything through combinators, eff-sharp lets you write native
syntax that lowers onto the exact same `Effect` core.

Every snippet below is taken from the runnable examples in this folder
(`dotnet run --project ai-docs`).

---

## 1. `effect { }` instead of `Effect.gen`

Effect (TS) authors sequential effects with a generator function and `yield*`:

```ts
const program = Effect.gen(function* () {
  const cache = yield* Cache.make(100, lookup)
  const a = yield* Cache.get(cache, "alpha")
  const b = yield* Cache.get(cache, "alpha")
  return [a, b]
})
```

eff-sharp uses a real F# computation expression — `let!` binds a success,
`do!` runs a unit effect, and `return` lifts a value. No generator, no `yield*`:

```fsharp
let program = effect {
    let! cache = Cache.make 100 lengthLookup
    let! a = Cache.get cache "alpha"
    let! b = Cache.get cache "alpha"
    return (a, b)
}
```

The CE also gives you `for`, `while`, `try/finally`, and `use` over effects for
free (see `EffectBuilder`). *(05_Batching.fs, 08_Observability.fs)*

---

## 2. `stream { }` instead of a combinator pipeline

This is the headline win. In Effect you build streams by chaining operators.
eff-sharp ships a `stream { }` CE so a producer reads like a generator:

```fsharp
// combinator style (the only option Effect offers)
let a = Stream.range 1 3 |> Stream.concat (Stream.fromIterable [10; 20]) |> Stream.map sq

// the SAME stream as a CE
let b = stream {
    for x in [1; 2; 3] do yield x * x
    yield! stream { for y in [10; 20] do yield y * y }
}
```

`yield` emits one element, `yield!` splices a whole stream, `for … do yield`
maps over a collection — all lowering onto the stack-safe `Effect` core.
*(02_Stream.fs)*

---

## 3. Native `match` instead of `*.match` combinators

Effect exposes `Exit.match`, `Option.match`, `Cause.match`, etc. because TS has
no pattern matching. In F#, every Effect data type is a plain DU you destructure
directly — with guards, or-patterns, and record patterns:

```fsharp
// unwrap an Exit
match Effect.runSync env eff with
| Success value -> value
| Failure cause -> failwithf "%s" (Cause.render cause)

// classify a DateTime by its UTC parts — or-patterns + guards, no combinator
match DateTime.toPartsUtc dt with
| { WeekDay = 0 } | { WeekDay = 6 } -> "weekend"
| { Hour = h } when h < 9 || h >= 17 -> "off-hours weekday"
| _ -> "business hours"
```

The same applies to `LogLevel`, `SpanStatus`, and `ScheduleDecision` — control
flow is ordinary F#. *(Demo.fs, 06_Schedule.fs, 07_DateTime.fs, 08_Observability.fs)*

---

## 4. Records & DUs for data modelling instead of object builders

Effect synthesises tagged objects at runtime (`Request.tagged`, `Data.Class`,
`Schedule` builders) because JS has no nominal types. F#'s records and unions
*are* the model:

```fsharp
// a Request is just a record implementing a marker interface
type UserById =
    { Id: int }
    interface Request<string, string, unit>

// calendar deltas are a record; schedule decisions are a union
DateTime.add { DateTimeMath.Zero with Years = 250 } independenceDay
match step now input with Continue(out, delay) -> ... | Done out -> ...
```

Construction, structural equality, and exhaustiveness checking come from the
language, not a library. *(05_Batching.fs, 06_Schedule.fs, 07_DateTime.fs)*

---

## 5. Units of measure — a guard TypeScript simply cannot express

F# can attach physical units to numbers and check them at compile time. A
seconds-vs-millis mix-up becomes a *type error*, not a production incident:

```fsharp
[<Measure>] type s
[<Measure>] type ms
let secondsToMillis (x: float<s>) : float<ms> = x * 1000.0<ms/s>

let timeout = 30.0<s>
let timeoutMs : float<ms> = secondsToMillis timeout
// secondsToMillis timeoutMs  // ← compile error: expected float<s>, got float<ms>
```

There is no TypeScript equivalent. *(07_DateTime.fs)*
