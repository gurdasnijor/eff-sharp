module CliSpec

open Effect
open Effect.Cli
open Effect.Vitest

let private runExit ctx eff = Effect.runSync ctx eff

let private run ctx eff =
    match runExit ctx eff with
    | Success value -> value
    | Failure cause -> failwithf "unexpected failure: %s" (Cause.render cause)

let private consoleWithBuffers (stdout: System.Collections.Generic.List<string>) (stderr: System.Collections.Generic.List<string>) =
    { Console.live with
        Log = fun args -> stdout.Add(args |> Array.map string |> String.concat " ")
        Error = fun args -> stderr.Add(args |> Array.map string |> String.concat " ") }

let private get<'T> key (values: Map<string, obj>) : 'T = values.[key] :?> 'T

describe "Effect.Cli" (fun () ->
    test "primitive parsers cover scalar command inputs" (fun () ->
        toBe (run Context.empty (Primitive.string.Parse "hello")) "hello"
        toBe (run Context.empty (Primitive.integer.Parse "42")) 42
        toBe (run Context.empty (Primitive.float.Parse "3.5")) 3.5
        toBe (run Context.empty (Primitive.boolean.Parse "yes")) true
        toBe (run Context.empty ((Primitive.choice [ "text"; "json" ]).Parse "json")) "json")

    test "primitive parser failures stay in the typed error channel" (fun () ->
        match runExit Context.empty (Primitive.integer.Parse "3.14") with
        | Failure cause -> toBe (Cause.failures cause |> List.head) "an integer, got 3.14"
        | Success value -> failwithf "expected parse failure, got %d" value)

    test "runs nested commands with shared flags, defaults, and optional flags" (fun () ->
        let captured = System.Collections.Generic.List<Map<string, obj>>()

        let outputFlag =
            Flag.choice "output" [ "text"; "json" ]
            |> Flag.withDescription "Output format"
            |> Flag.withDefault "text"

        let listCommand =
            Command.makeWith "list" [] (fun values ->
                Effect.sync (fun () -> captured.Add values))
            |> Command.withDescription "List available verification proofs"

        let runCommand =
            Command.makeWith
                "run"
                [ Command.param "name" (Argument.string "name" |> Argument.withDefault "all")
                  Command.flag "reportDir" (Flag.directory "report-dir" |> Flag.optional)
                  Command.flag "trialId" (Flag.string "trial-id" |> Flag.optional) ]
                (fun values -> Effect.sync (fun () -> captured.Add values))
            |> Command.withDescription "Run one proof or all proofs"

        let command =
            Command.make "verification"
            |> Command.withSharedFlags [ Command.flag "output" outputFlag ]
            |> Command.withSubcommands
                [ Command.make "proof"
                  |> Command.withSubcommands [ listCommand; runCommand ] ]

        run
            Context.empty
            (Command.runWith
                command
                { Version = "1.0.0" }
                [ "--output"; "json"; "proof"; "run"; "all"; "--report-dir"; "reports"; "--trial-id=trial-1" ])

        toBe captured.Count 1
        let values = captured.[0]
        toBe (get<string> "output" values) "json"
        toBe (get<string> "name" values) "all"
        toBe (get<string option> "reportDir" values) (Some "reports")
        toBe (get<string option> "trialId" values) (Some "trial-1"))

    test "uses defaults and None when optional flags are absent" (fun () ->
        let captured = System.Collections.Generic.List<Map<string, obj>>()

        let command =
            Command.makeWith
                "run"
                [ Command.param "name" (Argument.string "name" |> Argument.withDefault "all")
                  Command.flag "reportDir" (Flag.directory "report-dir" |> Flag.optional) ]
                (fun values -> Effect.sync (fun () -> captured.Add values))

        run Context.empty (Command.runWith command { Version = "1.0.0" } [])

        let values = captured.[0]
        toBe (get<string> "name" values) "all"
        toBe (get<string option> "reportDir" values) None)

    test "fails on unknown flags before invoking the handler" (fun () ->
        let command =
            Command.makeWith "run" [ Command.param "name" (Argument.string "name") ] (fun _ -> Effect.succeed ())

        match Command.runWith command { Version = "1.0.0" } [ "--wat"; "x" ] |> runExit Context.empty with
        | Failure cause ->
            match Cause.failures cause |> List.head with
            | UnknownFlag name -> toBe name "wat"
            | other -> failwithf "expected UnknownFlag, got %A" other
        | Success() -> failwith "expected unknown flag failure")

    test "prints help and version through the Console service" (fun () ->
        let stdout = System.Collections.Generic.List<string>()
        let stderr = System.Collections.Generic.List<string>()
        let ctx = Context.make Console.tag (consoleWithBuffers stdout stderr)

        let command =
            Command.makeWith
                "run"
                [ Command.param "name" (Argument.string "name" |> Argument.withDescription "Proof name")
                  Command.flag "output" (Flag.choice "output" [ "text"; "json" ]) ]
                (fun _ -> Effect.succeed ())
            |> Command.withDescription "Run proofs"

        run ctx (Command.runWith command { Version = "2.0.0" } [ "--help" ])
        run ctx (Command.runWith command { Version = "2.0.0" } [ "--version" ])

        toBe (stdout.[0].Contains("USAGE")) true
        toBe (stdout.[0].Contains("ARGUMENTS")) true
        toBe (stdout.[0].Contains("FLAGS")) true
        toBe stdout.[1] "2.0.0"))
