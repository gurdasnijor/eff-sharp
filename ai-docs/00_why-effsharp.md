# Why eff-sharp? — the F# expressive edge

eff-sharp keeps Effect's semantics (typed errors, fibers, scopes, layers) but is
authored in F#, whose language features remove ceremony that Effect simulates with
combinators. Five concrete wins — each is exercised by a runnable example.

---

## 1. `effect { }` is real do-notation, not a generator hack

Effect (TS) threads steps through `Effect.gen(function* () { ... })` with `yield*`.
eff-sharp uses a native computation expression, and it also lifts ordinary F#
control flow (`for`, `while`, `match`, `use`, `try/finally`).

```ts
// Effect (TypeScript)
const program = Effect.gen(function* () {
  const acc = yield* Ref.make(0)
  for (const i of [1, 2, 3]) yield* Ref.update(acc, (s) => s + i)
  return yield* Ref.get(acc)
})
```

```fsharp
// eff-sharp  —  for/let!/return are native; `for` lowers to Effect.forEach
let program =
    effect {
        let! acc = Ref.make 0
        for i in 1..3 do
            do! Ref.update acc (fun s -> s + i)
        return! Ref.get acc
    }
```

➡ `01_effect/01_Basics.fs`

---

## 2. Error handling is a native `match` on a DU — exhaustive & checked

Effect recovers by *string* tag: `Effect.catchTag("OutOfStock", …)`. eff-sharp uses
a discriminated union, so the compiler enforces that you handle every case.

```ts
program.pipe(
  Effect.catchTag("OutOfStock", (e) => Effect.succeed(`backordered ${e.sku}`))
) // a typo in "OutOfStock" is silently a no-op
```

```fsharp
program
|> Effect.catchAll (fun err ->
    match err with                               // ← exhaustiveness-checked
    | OutOfStock sku      -> Effect.succeed (sprintf "backordered %s" sku)
    | PaymentDeclined why -> Effect.succeed (sprintf "retry: %s" why)
    | InvalidQuantity q   -> Effect.succeed (sprintf "clamp %d" q))
```

➡ `01_effect/03_Errors.fs`

---

## 3. `Cause`/`Exit` are plain DUs — pattern-match straight into them

No `Exit.match`/`Cause.match` combinator family; the outcome is data.

```fsharp
match Effect.runSync () program with
| Success value                                        -> ...
| Failure { Reasons = [ Reason.Fail (OutOfStock sku) ] } -> ...   // nested match!
| Failure { Reasons = [ Reason.Die ex ] }              -> ...     // a defect
| Failure cause                                        -> Cause.render cause
```

➡ `01_effect/05_Running.fs`, `03_integration/10_ManagedRuntime.fs`

---

## 4. `stream { }` builds streams like `seq { }`

Effect composes streams from combinators; eff-sharp adds a generator CE with
`yield` / `yield!` / `for`.

```fsharp
let combined =
    stream {
        yield 1
        yield! Stream.fromIterable [ 2; 3 ]
        for x in [ 4; 5 ] do yield x
    }   // => [1; 2; 3; 4; 5]
```

➡ `bonus/01_CEPower.fs`

---

## 5. Units of measure + records/DUs model the domain — no schema→type derivation

TypeScript derives a *type* from a schema (`Schema.Type<typeof S>`). F# already has
the type: write the record/DU natively and let `Schema` validate untrusted input
*into* it. As a bonus, units of measure catch dimensional bugs at compile time —
something TS's type system cannot express.

```fsharp
[<Measure>] type m
[<Measure>] type s
let speed (d: float<m>) (t: float<s>) : float<m/s> = d / t   // d + t won't compile

type Task = { Title: string; Priority: Priority }            // the type IS the model
let taskSchema : Schema<Task> = Schema.object { ... }        // schema points AT it
```

➡ `bonus/01_CEPower.fs`, `09_testing/10_EffectTests.fs`

---

These aren't cosmetic: exhaustiveness, dimensional safety, and native control flow
move whole classes of bug from runtime to compile time, while keeping Effect's
structured-concurrency model intact.
