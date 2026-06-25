# Node Runtime Tests

Tests in this directory are authored in F#, compiled by Fable, and executed by
Vitest on Node.

```bash
npm test
```

Add one `*Spec.fs` file per module. Specs should use `Effect.Vitest`:

```fsharp
module DurationSpec

open Effect
open Effect.Vitest

describe "Duration" (fun () ->
    test "millis" (fun () ->
        toBe (Duration.toMillis (Duration.millis 10.0)) 10.0))
```

Specs validate the shipped JavaScript runtime behavior.
