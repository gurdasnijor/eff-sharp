module TerminalSpec

open Effect
open Effect.Vitest

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

describe "Terminal" (fun () ->
    test "columns reads the provided terminal dimensions" (fun () ->
        let ctx =
            Context.make Terminal.tag (testTerminal [] (System.Text.StringBuilder()) 132)

        toBe (run ctx Terminal.columns) 132)

    test "rows reads the provided terminal dimensions" (fun () ->
        let ctx =
            Context.make Terminal.tag (testTerminal [] (System.Text.StringBuilder()) 80)

        toBe (run ctx Terminal.rows) 24)

    test "display writes text to the terminal" (fun () ->
        let output = System.Text.StringBuilder()
        let ctx = Context.make Terminal.tag (testTerminal [] output 80)

        let program =
            effect {
                do! Terminal.display "Hello, "
                do! Terminal.display "world!"
            }

        run ctx program
        toBe (output.ToString()) "Hello, world!")

    test "readLine returns queued lines in order" (fun () ->
        let ctx =
            Context.make Terminal.tag (testTerminal [ "first"; "second" ] (System.Text.StringBuilder()) 80)

        let program =
            effect {
                let! a = Terminal.readLine
                let! b = Terminal.readLine
                return (a, b)
            }

        toBe (run ctx program = ("first", "second")) true)

    test "readLine fails with QuitError at end of input" (fun () ->
        let ctx =
            Context.make Terminal.tag (testTerminal [] (System.Text.StringBuilder()) 80)

        toBe (runExit ctx Terminal.readLine = Exit.fail QuitError) true)

    test "isQuitError narrows a QuitError" (fun () ->
        toBe (Terminal.isQuitError (box QuitError)) true
        toBe (Terminal.isQuitError (box "nope")) false)

    test "readLine then quit surfaces QuitError through a program" (fun () ->
        let ctx =
            Context.make Terminal.tag (testTerminal [ "only" ] (System.Text.StringBuilder()) 80)

        let program =
            effect {
                let! _first = Terminal.readLine
                return! Terminal.readLine
            }

        match runExit ctx program with
        | Failure c ->
            toBe (Cause.failures c |> List.exists (fun e -> Terminal.isQuitError (box e))) true
        | Success v -> failwithf "expected QuitError, got %s" v)

    test "accessors fall back to the live terminal when none is provided" (fun () ->
        let cols = run Context.empty Terminal.columns
        toBe (cols >= 0) true)

    test "the default layer provides the live terminal" (fun () ->
        let program = Terminal.columns |> Effect.map (fun c -> c >= 0)
        let provided: Effect<bool, exn, unit> = Layer.provide Terminal.layer program
        toBe (Effect.runSync () provided = Exit.succeed true) true))
