namespace Effect.Cli

open System
open Effect

type CliError =
    | MissingArgument of name: string
    | MissingFlag of name: string
    | InvalidValue of target: string * value: string * expected: string
    | UnknownFlag of name: string
    | UnknownCommand of path: string list
    | MissingHandler of command: string

[<RequireQualifiedAccess>]
module CliError =

    let render (error: CliError) : string =
        match error with
        | MissingArgument name -> sprintf "Missing argument <%s>" name
        | MissingFlag name -> sprintf "Missing flag --%s" name
        | InvalidValue(target, value, expected) -> sprintf "Invalid value for %s: \"%s\". Expected: %s" target value expected
        | UnknownFlag name -> sprintf "Unknown flag --%s" name
        | UnknownCommand path -> sprintf "Unknown command: %s" (String.concat " " path)
        | MissingHandler command -> sprintf "Command has no handler: %s" command

type Primitive<'A> =
    { Tag: string
      Parse: string -> Effect<'A, string, Context>
      Metavar: string }

[<RequireQualifiedAccess>]
module Primitive =

    let make tag metavar parse : Primitive<'A> =
        { Tag = tag
          Metavar = metavar
          Parse = parse }

    let private result tag metavar (parse: string -> Result<'A, string>) : Primitive<'A> =
        make tag metavar (fun value -> parse value |> Effect.fromResult)

    let string: Primitive<string> = result "String" "string" Ok

    let boolean: Primitive<bool> =
        let trueValues = Set.ofList [ "true"; "1"; "y"; "yes"; "on" ]
        let falseValues = Set.ofList [ "false"; "0"; "n"; "no"; "off" ]

        result "Boolean" "boolean" (fun value ->
            let lower = value.ToLowerInvariant()

            if Set.contains lower trueValues then
                Ok true
            elif Set.contains lower falseValues then
                Ok false
            else
                Error "one of true, yes, on, 1, y, false, no, off, 0, n")

    let float: Primitive<float> =
        result "Float" "number" (fun value ->
            match Double.TryParse(value, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
            | true, n when not (Double.IsNaN n) && not (Double.IsInfinity n) -> Ok n
            | _ -> Error "a finite number")

    let integer: Primitive<int> =
        result "Integer" "integer" (fun value ->
            match Double.TryParse(value, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
            | true, n when not (Double.IsNaN n) && not (Double.IsInfinity n) && Math.Truncate n = n -> Ok(int n)
            | true, n -> Error(sprintf "an integer, got %O" n)
            | _ -> Error "an integer")

    let date: Primitive<DateTimeOffset> =
        result "Date" "date" (fun value ->
            match DateTimeOffset.TryParse(value, Globalization.CultureInfo.InvariantCulture, Globalization.DateTimeStyles.RoundtripKind) with
            | true, d -> Ok d
            | _ -> Error "a valid date")

    let choiceWithValue (choices: (string * 'A) list) : Primitive<'A> =
        let expected = choices |> List.map fst |> String.concat " | "

        result "Choice" "choice" (fun value ->
            match choices |> List.tryFind (fun (name, _) -> name = value) with
            | Some(_, parsed) -> Ok parsed
            | None -> Error(sprintf "one of %s" expected))

    let choice (choices: string list) : Primitive<string> = choiceWithValue (choices |> List.map (fun value -> value, value))

type ParamKind =
    | ArgumentKind
    | FlagKind

type ParsedArgs =
    { Flags: Map<string, string list>
      Arguments: string list }

type ParamMeta =
    { Kind: ParamKind
      Name: string
      Description: string option
      Aliases: string list
      Metavar: string
      IsBoolean: bool }

type Param<'A> =
    internal
        { Meta: ParamMeta
          Parse: ParsedArgs -> Effect<string list * 'A, CliError, Context> }

[<RequireQualifiedAccess>]
module Param =

    let private missing meta =
        match meta.Kind with
        | ArgumentKind -> MissingArgument meta.Name
        | FlagKind -> MissingFlag meta.Name

    let private invalid meta value expected =
        let target =
            match meta.Kind with
            | ArgumentKind -> sprintf "argument <%s>" meta.Name
            | FlagKind -> sprintf "flag --%s" meta.Name

        InvalidValue(target, value, expected)

    let make (kind: ParamKind) (name: string) (primitive: Primitive<'A>) : Param<'A> =
        let meta =
            { Kind = kind
              Name = name
              Description = None
              Aliases = []
              Metavar = primitive.Metavar
              IsBoolean = primitive.Tag = "Boolean" && kind = FlagKind }

        let parsePrimitive value =
            primitive.Parse value |> Effect.mapError (invalid meta value)

        let parse args =
            match kind with
            | ArgumentKind ->
                match args.Arguments with
                | value :: rest -> parsePrimitive value |> Effect.map (fun parsed -> rest, parsed)
                | [] -> Effect.fail (MissingArgument name)
            | FlagKind ->
                match Map.tryFind name args.Flags with
                | Some values when values.Length > 0 ->
                    values
                    |> List.rev
                    |> List.head
                    |> parsePrimitive
                    |> Effect.map (fun parsed -> args.Arguments, parsed)
                | _ when meta.IsBoolean -> Effect.succeed (args.Arguments, unbox<'A> (box false))
                | _ -> Effect.fail (MissingFlag name)

        { Meta = meta; Parse = parse }

    let erase (param: Param<'A>) : Param<obj> =
        { Meta = param.Meta
          Parse = fun args -> param.Parse args |> Effect.map (fun (rest, value) -> rest, box value) }

    let withDescription (description: string) (param: Param<'A>) : Param<'A> =
        { param with
            Meta = { param.Meta with Description = Some description } }

    let withAlias (alias: string) (param: Param<'A>) : Param<'A> =
        { param with
            Meta = { param.Meta with Aliases = param.Meta.Aliases @ [ alias ] } }

    let withDefault (value: 'A) (param: Param<'A>) : Param<'A> =
        { param with
            Parse =
                fun args ->
                    param.Parse args
                    |> Effect.catchAll (function
                        | MissingArgument _
                        | MissingFlag _ -> Effect.succeed (args.Arguments, value)
                        | other -> Effect.fail other) }

    let optional (param: Param<'A>) : Param<'A option> =
        { Meta = param.Meta
          Parse =
            fun args ->
                param.Parse args
                |> Effect.map (fun (rest, value) -> rest, Some value)
                |> Effect.catchAll (function
                    | MissingArgument _
                    | MissingFlag _ -> Effect.succeed (args.Arguments, None)
                    | other -> Effect.fail other) }

[<RequireQualifiedAccess>]
module Argument =

    let string name = Param.make ArgumentKind name Primitive.string
    let integer name = Param.make ArgumentKind name Primitive.integer
    let float name = Param.make ArgumentKind name Primitive.float
    let date name = Param.make ArgumentKind name Primitive.date
    let choice name choices = Param.make ArgumentKind name (Primitive.choice choices)
    let choiceWithValue name choices = Param.make ArgumentKind name (Primitive.choiceWithValue choices)
    let file name = string name
    let directory name = string name

    let withDescription description param = Param.withDescription description param
    let withDefault value param = Param.withDefault value param
    let optional param = Param.optional param

[<RequireQualifiedAccess>]
module Flag =

    let string name = Param.make FlagKind name Primitive.string
    let integer name = Param.make FlagKind name Primitive.integer
    let float name = Param.make FlagKind name Primitive.float
    let boolean name = Param.make FlagKind name Primitive.boolean
    let date name = Param.make FlagKind name Primitive.date
    let choice name choices = Param.make FlagKind name (Primitive.choice choices)
    let choiceWithValue name choices = Param.make FlagKind name (Primitive.choiceWithValue choices)
    let file name = string name
    let directory name = string name

    let optional param = Param.optional param
    let withDescription description param = Param.withDescription description param
    let withDefault value param = Param.withDefault value param
    let withAlias alias param = Param.withAlias alias param

type Binding =
    { Key: string
      Param: Param<obj> }

type RunOptions = { Version: string }

type Command =
    internal
        { Name: string
          Description: string option
          Config: Binding list
          SharedFlags: Binding list
          Handler: (Map<string, obj> -> Effect<unit, CliError, Context>) option
          Subcommands: Command list }

[<RequireQualifiedAccess>]
module Command =

    let make name : Command =
        { Name = name
          Description = None
          Config = []
          SharedFlags = []
          Handler = None
          Subcommands = [] }

    let param key (param: Param<'A>) : Binding = { Key = key; Param = Param.erase param }

    let flag key (flag: Param<'A>) : Binding = param key flag

    let withDescription (description: string) (command: Command) : Command =
        { command with Description = Some description }

    let withConfig (config: Binding list) (command: Command) : Command = { command with Config = config }

    let withHandler (handler: Map<string, obj> -> Effect<unit, CliError, Context>) (command: Command) : Command =
        { command with Handler = Some handler }

    let makeWith name config handler =
        make name |> withConfig config |> withHandler handler

    let withSharedFlags flags command =
        { command with SharedFlags = flags }

    let withSubcommands subcommands command =
        { command with Subcommands = subcommands }

    let private isLongFlag (token: string) = token.StartsWith("--") && token.Length > 2
    let private isShortFlag (token: string) = token.StartsWith("-") && not (token.StartsWith("--")) && token.Length > 1
    let private isFlagToken token = isLongFlag token || isShortFlag token

    let private allFlagBindings command inherited =
        inherited @ command.SharedFlags @ (command.Config |> List.filter (fun b -> b.Param.Meta.Kind = FlagKind))

    let private flagIndex bindings =
        bindings
        |> List.collect (fun binding ->
            let meta = binding.Param.Meta
            (meta.Name, meta) :: (meta.Aliases |> List.map (fun alias -> alias, meta)))
        |> Map.ofList

    let private flagName (token: string) =
        if isLongFlag token then
            token.Substring(2).Split('=').[0]
        elif isShortFlag token then
            token.Substring(1)
        else
            token

    let private skipFlag (index: Map<string, ParamMeta>) (tokens: string list) =
        match tokens with
        | token :: value :: rest when isLongFlag token && token.Contains("=") -> rest
        | token :: rest when isLongFlag token || isShortFlag token ->
            match Map.tryFind (flagName token) index with
            | Some meta when meta.IsBoolean -> rest
            | _ ->
                match rest with
                | next :: tail when not (isFlagToken next) -> tail
                | _ -> rest
        | _ :: rest -> rest
        | [] -> []

    let private selectCommand command args =
        let rec splitFlagForScan (index: Map<string, ParamMeta>) tokens =
            match tokens with
            | token :: rest when isLongFlag token && token.Contains("=") -> [ token ], rest
            | token :: rest when isLongFlag token || isShortFlag token ->
                match Map.tryFind (flagName token) index with
                | Some meta when meta.IsBoolean -> [ token ], rest
                | _ ->
                    match rest with
                    | value :: tail when not (isFlagToken value) -> [ token; value ], tail
                    | _ -> [ token ], rest
            | token :: rest -> [ token ], rest
            | [] -> [], []

        let rec loop current inherited path kept remaining =
            let index = flagIndex (allFlagBindings current inherited)

            match remaining with
            | [] -> current, inherited, List.rev path, List.rev kept
            | token :: _ when isFlagToken token ->
                let consumed, rest = splitFlagForScan index remaining
                loop current inherited path (List.rev consumed @ kept) rest
            | token :: rest ->
                match current.Subcommands |> List.tryFind (fun child -> child.Name = token) with
                | Some child -> loop child (inherited @ current.SharedFlags) (child.Name :: path) kept rest
                | None -> loop current inherited path (token :: kept) rest

        let args =
            match args with
            | head :: tail when head = command.Name -> tail
            | _ -> args

        loop command [] [ command.Name ] [] args

    let private collectFlags bindings tokens =
        let index = flagIndex bindings

        let rec loop flags positionals remaining =
            match remaining with
            | [] -> Ok(flags, List.rev positionals)
            | "--" :: rest -> Ok(flags, List.rev positionals @ rest)
            | token :: rest when isLongFlag token || isShortFlag token ->
                let rawName = flagName token

                match Map.tryFind rawName index with
                | None -> Error(UnknownFlag rawName)
                | Some meta ->
                    let canonical = meta.Name

                    let add value =
                        flags
                        |> Map.change canonical (function
                            | Some values -> Some(values @ [ value ])
                            | None -> Some [ value ])

                    if isLongFlag token && token.StartsWith("--no-") && meta.IsBoolean then
                        loop (add "false") positionals rest
                    elif token.Contains("=") then
                        let value = token.Substring(token.IndexOf("=") + 1)
                        loop (add value) positionals rest
                    elif meta.IsBoolean then
                        loop (add "true") positionals rest
                    else
                        match rest with
                        | value :: tail when not (isFlagToken value) -> loop (add value) positionals tail
                        | _ -> Error(MissingFlag canonical)
            | token :: rest -> loop flags (token :: positionals) rest

        loop Map.empty [] tokens

    let private parseConfig bindings parsed =
        let rec loop remaining values args =
            match remaining with
            | [] -> Effect.succeed values
            | binding :: rest ->
                binding.Param.Parse { parsed with Arguments = args }
                |> Effect.flatMap (fun (leftover, value) ->
                    loop rest (Map.add binding.Key value values) leftover)

        loop bindings Map.empty parsed.Arguments

    let private usage command path inherited =
        let bindings = inherited @ command.SharedFlags @ command.Config

        let flags =
            bindings
            |> List.choose (fun b ->
                if b.Param.Meta.Kind = FlagKind then
                    Some(sprintf "[--%s]" b.Param.Meta.Name)
                else
                    None)

        let args =
            bindings
            |> List.choose (fun b ->
                if b.Param.Meta.Kind = ArgumentKind then
                    Some(sprintf "<%s>" b.Param.Meta.Name)
                else
                    None)

        let head = String.concat " " path
        ("USAGE\n  " + String.concat " " ([ head ] @ flags @ args)).TrimEnd()

    let private help command path inherited =
        let bindings = inherited @ command.SharedFlags @ command.Config

        let renderParam prefix (b: Binding) =
            let meta = b.Param.Meta
            let desc = defaultArg meta.Description ""
            sprintf "  %s%s %s    %s" prefix meta.Name meta.Metavar desc

        let args =
            bindings
            |> List.filter (fun b -> b.Param.Meta.Kind = ArgumentKind)
            |> List.map (renderParam "")

        let flags =
            bindings
            |> List.filter (fun b -> b.Param.Meta.Kind = FlagKind)
            |> List.map (renderParam "--")

        let commands =
            command.Subcommands
            |> List.map (fun c -> sprintf "  %s    %s" c.Name (defaultArg c.Description ""))

        [ yield usage command path inherited
          match command.Description with
          | Some description ->
              yield ""
              yield description
          | None -> ()
          if args.Length > 0 then
              yield ""
              yield "ARGUMENTS"
              yield! args
          if flags.Length > 0 then
              yield ""
              yield "FLAGS"
              yield! flags
          if commands.Length > 0 then
              yield ""
              yield "COMMANDS"
              yield! commands ]
        |> String.concat "\n"

    let private has token args = args |> List.exists ((=) token)

    let runWith (command: Command) (options: RunOptions) (args: string list) : Effect<unit, CliError, Context> =
        let selected, inherited, path, remaining = selectCommand command args

        effect {
            if has "--help" remaining || has "-h" remaining then
                do! Console.log [| box (help selected path inherited) |] |> Effect.mapError (fun _ -> MissingHandler selected.Name)
            elif has "--version" remaining || has "-v" remaining then
                do! Console.log [| box options.Version |] |> Effect.mapError (fun _ -> MissingHandler selected.Name)
            else
                let bindings = inherited @ selected.SharedFlags @ selected.Config

                match collectFlags (allFlagBindings selected inherited) remaining with
                | Error error -> return! Effect.fail error
                | Ok(flags, positionals) ->
                    let parsed = { Flags = flags; Arguments = positionals }
                    let! values = parseConfig bindings parsed

                    match selected.Handler with
                    | Some handler -> return! handler values
                    | None ->
                        if selected.Subcommands.Length > 0 then
                            do! Console.log [| box (help selected path inherited) |] |> Effect.mapError (fun _ -> MissingHandler selected.Name)
                        else
                            return! Effect.fail (MissingHandler selected.Name)
        }

#if FABLE_COMPILER
    [<Fable.Core.Emit("process.argv.slice(2)")>]
    let private argv () : string array = failwith "Fable emit only"
#else
    let private argv () : string array = Environment.GetCommandLineArgs() |> Array.skip 1
#endif

    let run command options : Effect<unit, CliError, Context> =
        argv () |> Array.toList |> runWith command options
