# RFC-002 — Typed-`R` environment

**Status:** Proposed · **Area:** DI / `Context` / `Layer` (stack-rank #2) ·
**Risk:** Medium

## Problem

Effect's headline static-safety feature is that `R` is a type-level set of
services and the compiler tells you exactly what's unprovided; `Layer<ROut,E,RIn>`
tracks what it provides. eff-sharp **drops `ROut`** (`Layer.fs:14`), `R` is checked
at runtime via `match box env with :? Context` (`Effect.fs`), and a missing
service is a runtime `KeyNotFoundException` **defect**. Worse, two DI models are
half-wired: a raw record-as-`'R` (the README example) and `Context`.

## Constraint (from research)

A "typed `R` the compiler subtracts on `provide`" is **not achievable in F#**:

- No type-level set subtraction exists.
- Member-constraint SRTP on `^Env` is the **worst** option for Fable — Fable runs
  its own weak trait-call resolver and frequently can't resolve nested SRTP
  (Fable #2083/#2468); it also gives viral `inline`, slow compiles, and poor
  errors (the FSharpPlus experience).
- The one production-ish F# Effect lib, **Orsak**, deliberately avoids SRTP and
  encodes requirements as **nominal interface subtype constraints accumulated by
  inference** — but it's `ValueTask`/resumable-code based and **not Fable-friendly**.

## Proposal — a two-layer model, one decision made explicitly

**Layer 0 (floor, ship now): runtime `Context`, cleaned up.**
- Make `Context` the *only* environment model. Remove the raw-record-as-`'R`
  path from docs/examples so there's one story.
- Keep `Tag`-keyed lookup, but make a missing service a **typed** failure
  (`ServiceNotFound` in `E`) at the `provide`/`run` boundary rather than a bare
  defect, so it's at least observable in the error channel.
- **Document explicitly** that compile-time `R` subtraction is out of scope —
  turn the silent gap into a stated boundary.

**Layer 1 (opt-in, .NET-leaning): Orsak-style provider interfaces.**
For consumers who want compile-time DI checking and aren't targeting Fable, offer
the accumulated-interface-constraint encoding behind the same `Tag` vocabulary:

```fsharp
// a Tag<'S> projects to a generated provider interface
type IClock = abstract Now: unit -> int64
type IClockProvider = abstract Clock: IClock

let now () = Effect.serviceWith (fun (p: #IClockProvider) -> p.Clock.Now())
// requirements pile up as 'r :> IClockProvider and 'r :> ILogProvider by inference;
// discharge all at once with one concrete env implementing every provider.
```

Generate the provider-interface boilerplate from `Tag` definitions with a
**Myriad** source generator (Orsak ships exactly this) so it's not hand-written.

## Why not just SRTP everywhere

Because it breaks the primary target (Fable) and tanks ergonomics. The two-layer
split keeps Fable on the robust runtime `Context` while giving .NET-only consumers
an opt-in static-safety upgrade — without betting the core on SRTP.

## Tradeoffs

- Layer 0 is low-risk and immediately makes the project honest; most of the value
  is here.
- Layer 1 adds provider boilerplate (mitigated by Myriad) and only helps the
  non-Fable target; build it only if a consumer asks.

## Effort

Layer 0: Low–Medium (mostly removing the second DI model + typed not-found +
docs). Layer 1: Medium (encoding + Myriad generator), optional.

## Sources

- Orsak (provider-interface encoding, Myriad generator): https://github.com/JohSand/Orsak
- Encoding origin (Sypytkowski): https://www.bartoszsypytkowski.com/dealing-with-complex-dependency-injection-in-f/
- Fable SRTP trait-call gaps: https://github.com/fable-compiler/Fable/issues/2083 ,
  https://github.com/fable-compiler/Fable/issues/2468
