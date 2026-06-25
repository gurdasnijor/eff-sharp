/// @title ⭐ Bonus — F# expressive power Effect (TS) can't match
///
/// eff-sharp keeps Effect's semantics but rides on F#'s language features. Three
/// wins shown here:
///   1. `stream { }` — generator-style streams with `yield` / `yield!` / `for`.
///   2. Units of measure — dimensional types checked at COMPILE time.
///   3. Records/DUs as the data model that Schema validates into.
/// See also `00_why-effsharp.md` for side-by-side comparisons.
module EffSharp.AiDocs.CEPower

open Effect
open EffSharp.AiDocs.Shared

// ---------------------------------------------------------------------------
// 1. `stream { }` — build a Stream like a sequence expression.  ⭐
//    In Effect (TS) you assemble streams from combinators (Stream.range,
//    Stream.filter, Stream.map, Stream.concat). Here the CE reads like `seq { }`.
// ---------------------------------------------------------------------------

let evensSquared: Stream<int, string, unit> =
    stream {
        for x in 1..10 do
            if x % 2 = 0 then
                yield x * x // 4, 16, 36, 64, 100
    }

let combined: Stream<int, string, unit> =
    stream {
        yield 1 // a single element
        yield! Stream.fromIterable [ 2; 3 ] // splice another stream
        for x in [ 4; 5 ] do
            yield x // a loop
    }

// ---------------------------------------------------------------------------
// 2. Units of measure — the compiler rejects mixing dimensions.  ⭐
//    There is simply no equivalent in TypeScript's type system.
// ---------------------------------------------------------------------------

[<Measure>]
type m // metres

[<Measure>]
type s // seconds

/// Returns metres-per-second; `dist + time` would be a *compile error*.
let speed (dist: float<m>) (time: float<s>) : float<m / s> = dist / time

// ---------------------------------------------------------------------------
// 3. Records/DUs model data natively; Schema validates untrusted input INTO them
//    (no separate "type derived from schema" dance — the type is the type).  ⭐
// ---------------------------------------------------------------------------

type Priority =
    | Low
    | High

type Task = { Title: string; Priority: Priority }

let taskSchema: Schema<Task> =
    Schema.object {
        let! title = Schema.field "title" (Schema.string |> Schema.minLength 1) (fun (t: Task) -> t.Title)

        and! priority =
            Schema.field "priority" (Schema.literalMap [ JString "low", Low; JString "high", High ]) (fun t -> t.Priority)

        return { Title = title; Priority = priority }
    }

let demo () =
    banner "Bonus · CE power — stream / units / Schema"
    printfn "  evensSquared => %A" (run (Stream.runCollect evensSquared))
    printfn "  combined     => %A" (run (Stream.runCollect combined))
    printfn "  speed 100m/8s => %.2f m/s" (float (speed 100.0<m> 8.0<s>))

    match Schema.decode taskSchema (JObject(Map [ "title", JString "ship docs"; "priority", JString "high" ])) with
    | Ok task -> printfn "  decoded task => %A" task
    | Error e -> printfn "  decode failed => %s" (Schema.format e)
