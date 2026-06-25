module Effect.Tests.TerminalTests

open Xunit
open Effect

// Upstream ships no Terminal.test.ts; these Facts exercise the ported core
// surface (columns/rows/readLine/display, QuitError, the default layer) against
// a capturing test `Terminal`. Interactive `readInput` (key streaming) is
// deferred — see Terminal.fs.

/// A test `Terminal`: `readLine` drains `lines` (then quits); `display` appends
/// to `output`; dimensions are fixed.
let private testTerminal (lines: string list) (output: System.Text.StringBuilder) (cols: int) : Terminal =
    let queue = System.Collections.Generic.Queue<string>(lines)

    Terminal.make
        (fun () -> cols)
        (fun () -> 24)
        (fun () -> if queue.Count > 0 then Some(queue.Dequeue()) else None)
        (fun text -> output.Append(text: string) |> ignore)

let private runExit (ctx: Context) (eff: Effect<'A, 'E, Context>) : Exit<'A, 'E> = Effect.runSync ctx eff

let private run (ctx: Context) (eff: Effect<'A, 'E, Context>) : 'A =
    match runExit ctx eff with
    | Success a -> a
    | Failure c -> failwithf "unexpected failure: %s" (Cause.render c)

// ----------------------------------------------------------------------------

[<Fact>]
let ``columns reads the provided terminal dimensions`` () =
    let ctx =
        Context.make Terminal.tag (testTerminal [] (System.Text.StringBuilder()) 132)

    Assert.Equal(132, run ctx Terminal.columns)

[<Fact>]
let ``rows reads the provided terminal dimensions`` () =
    let ctx =
        Context.make Terminal.tag (testTerminal [] (System.Text.StringBuilder()) 80)

    Assert.Equal(24, run ctx Terminal.rows)

[<Fact>]
let ``display writes text to the terminal`` () =
    let output = System.Text.StringBuilder()
    let ctx = Context.make Terminal.tag (testTerminal [] output 80)

    let program =
        effect {
            do! Terminal.display "Hello, "
            do! Terminal.display "world!"
        }

    run ctx program
    Assert.Equal("Hello, world!", output.ToString())

[<Fact>]
let ``readLine returns queued lines in order`` () =
    let ctx =
        Context.make Terminal.tag (testTerminal [ "first"; "second" ] (System.Text.StringBuilder()) 80)

    let program =
        effect {
            let! a = Terminal.readLine
            let! b = Terminal.readLine
            return (a, b)
        }

    Assert.Equal(("first", "second"), run ctx program)

[<Fact>]
let ``readLine fails with QuitError at end of input`` () =
    let ctx =
        Context.make Terminal.tag (testTerminal [] (System.Text.StringBuilder()) 80)

    Assert.Equal<Exit<string, QuitError>>(Exit.fail QuitError, runExit ctx Terminal.readLine)

[<Fact>]
let ``isQuitError narrows a QuitError`` () =
    Assert.True(Terminal.isQuitError (box QuitError))
    Assert.False(Terminal.isQuitError (box "nope"))

[<Fact>]
let ``readLine then quit surfaces QuitError through a program`` () =
    let ctx =
        Context.make Terminal.tag (testTerminal [ "only" ] (System.Text.StringBuilder()) 80)

    let program =
        effect {
            let! _first = Terminal.readLine
            return! Terminal.readLine // input exhausted -> QuitError
        }

    match runExit ctx program with
    | Failure c ->
        Assert.True(Cause.failures c |> List.exists (fun e -> Terminal.isQuitError (box e)), "expected a QuitError")
    | Success v -> Assert.Fail(sprintf "expected QuitError, got %s" v)

[<Fact>]
let ``accessors fall back to the live terminal when none is provided`` () =
    // No Terminal in context: columns falls back to `live` (a non-negative int).
    let cols = run Context.empty Terminal.columns
    Assert.True(cols >= 0)

[<Fact>]
let ``the default layer provides the live terminal`` () =
    let program = Terminal.columns |> Effect.map (fun c -> c >= 0)
    let provided: Effect<bool, exn, unit> = Layer.provide Terminal.layer program
    Assert.Equal<Exit<bool, exn>>(Exit.succeed true, Effect.runSync () provided)
