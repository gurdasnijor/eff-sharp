# EffSharp.S2

Strawman Fable-facing S2 binding for eff-sharp.

The package keeps `@s2-dev/streamstore` behind an internal Fable binding and
exposes an eff-sharp-shaped API:

```fsharp
open Effect

let program =
    effect {
        let! ack =
            S2.Stream.append
                { Target = { Basin = "orders"; Stream = "events" }
                  Records = [ S2.AppendRecord.string "created" ]
                  Options = Some { MatchSeqNum = Some 0.; FencingToken = None }
                  RequestOptions = None }

        return ack.Tail.SeqNum
    }

let runnable =
    program
    |> Layer.provide (S2.layer (S2Config.Create "s2-access-token"))
```

Consumers must install the JS SDK alongside the Fable output:

```sh
npm install @s2-dev/streamstore
```

This first pass covers client/layer setup, basin and stream list/create/delete,
append, read, read sessions, and tail checks. Append sessions, producers,
pattern serialization, access tokens, locations, metrics, endpoint overrides,
and retry configuration are intentionally left for follow-up design.
