# Porting conventions

How we port `effect` (TypeScript) to native F#. Every porting agent must follow
this. References live in `repos/` (gitignored, local only):

- Upstream source/spec: `repos/effect-smol/packages/effect/src/<Module>.ts`
- Upstream tests:        `repos/effect-smol/packages/effect/test/<Module>.test.ts`
- F# language reference:  `repos/fsharp/docs/` (e.g. pattern matching, active patterns)
- Effect docs for intent: https://effect.website/docs

## 1. This is a *port*, not a transliteration

Write idiomatic F# that a .NET F# developer would write, that happens to match
Effect's observable behaviour. Do **not** mechanically translate JS/TS idioms.

## 2. Prefer native F# pattern matching; lower Effect primitives into it

F# has first-class pattern matching
(<https://learn.microsoft.com/dotnet/fsharp/language-reference/match-expressions>,
mirrored in `repos/fsharp/docs/`). Use it as the primary control-flow tool.

- Effect's `Match` module / `Option.match` / `Exit.match` / `Either.match` etc.
  **lower into native `match ... with` expressions or active patterns** — they are
  not reimplemented as a combinator DSL.
- Keep a thin `matchX onA onB value` helper only when it adds real ergonomic
  value over `match` (e.g. `Exit.matchExit`); it should itself be one `match`.
- Reach for **active patterns** (`(|Pattern|_|)`) when a value needs custom
  destructuring, instead of porting Effect's runtime predicates.
- No `if/else` ladders or boolean flags where a `match` is clearer.

## 3. Drop JS-runtime-only machinery

TypeScript needs runtime type guards (`isX(value: unknown)`), `TypeId` brand
strings, and structural checks because it is dynamically typed at runtime. F#'s
static types make most of these dead weight.

- **Omit** `isCause`/`isReason`-style guards that test arbitrary `unknown`/`obj`.
- **Keep** guards that genuinely narrow a known union (`isFailReason` on a
  `Reason`), expressed as a `match`.
- Keep `TypeId` constants only where a test asserts them (parity), as `[<Literal>]`.
- Note every such omission in a doc comment so it is intentional, not silent.

## 4. Higher-kinded types

F# has no HKTs. Port the **concrete** surface of a module and skip the generic
typeclass plumbing (`HKT`, `TypeLambda`, `Covariant`, `Unify`). Note what was
skipped in the module's doc comment.

## 5. Don't reimplement what F# already ships

`Option`, `Result` (= `Either`), `Array`, `List`, `Map`, `Set`, `string`,
arithmetic — use FSharp.Core. Only port the Effect-specific surface that F# lacks.

## 6. Layout & naming (mirror effect-smol)

- One module per file: `src/Effect/<Module>.fs`, `namespace Effect`,
  `[<RequireQualifiedAccess>] module <Module>`.
- One test file per module: `tests/Effect.Tests/<Module>Tests.fs`, xUnit `[<Fact>]`,
  ported from the upstream `*.test.ts`. Cite the upstream file in a header comment.
- Doc-comment the public surface; note the upstream reference and any omissions.

## 7. Definition of done (per module)

1. `src/Effect/<Module>.fs` implemented idiomatically.
2. `tests/Effect.Tests/<Module>Tests.fs` ports the upstream test cases as Facts.
3. `dotnet test` is green for the new tests (add `<Compile>` entries in your
   worktree to verify).
4. Public API doc-commented; omissions noted.
5. Touch only your assigned modules.
