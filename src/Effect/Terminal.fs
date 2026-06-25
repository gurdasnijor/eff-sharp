#if FABLE_COMPILER
namespace Fable.Core

open System

/// Fable-only stand-ins for `Fable.Core.EmitAttribute`/`ImportAttribute`, letting the
/// platform layer use Fable's `[<Emit>]`/`[<Import>]` JS interop WITHOUT taking a
/// `Fable.Core` NuGet dependency — which keeps the JS shims confined to the two
/// platform files (`Terminal.fs`, `Path.fs`) per the porting brief, with zero change
/// to the .NET build or project files. Fable resolves emit/import by the attribute's
/// full name, so these local declarations are honored. The whole block compiles only
/// under Fable and is invisible to the .NET path. It lives here in `Terminal.fs`
/// (which precedes `Path.fs` in the assembly's compile order) and is reused by
/// `Path.fs`. (severity: refactor — JS platform layer)
type internal EmitAttribute(macro: string) =
    inherit Attribute()

/// Generates a top-level ESM `import { selector } from path` (ESM-safe; works under
/// Node's default module mode where `require` is undefined).
type internal ImportAttribute(selector: string, path: string) =
    inherit Attribute()
#endif

namespace Effect

/// `Terminal` — the command-line interface service.
///
/// Port of repos/effect-smol/packages/effect/src/Terminal.ts. A `Terminal` reads
/// input from and displays output to a user, without programs depending on a
/// concrete platform. It exposes terminal dimensions (`columns`/`rows`), reads a
/// line of input (`readLine`, failing with `QuitError` when the user cancels /
/// the stream ends), and displays text (`display`, failing with `PlatformError`).
/// Accessed as a `Context` service so tests can swap in a custom implementation.
///
/// **Adaptation to this port (NOT a transliteration).** As with `Console`, the
/// service record stores *unsafe* primitives and the accessors wrap them into
/// effects; the env is `Context`, and accessors fall back to the `live`
/// (`System.Console`-backed) terminal when none is provided.
///
/// Deferred (interactive parts, per the brief): `readInput` — the low-level key
/// event stream (`Queue.Dequeue<UserInput, Cause.Done>`) needs raw-TTY mode and
/// host keypress events that are not portable through .NET's `System.Console`.
/// The `UserInput`/`Key` shapes are still defined here for parity so a platform
/// adapter can supply `readInput` later. The HKT/`Schema.ErrorClass`/`TypeId`
/// machinery and the `isQuitError`-over-`unknown` guard are simplified per
/// CONVENTIONS.
/// Keyboard key metadata for a terminal input event. (Terminal.Key)
type Key =
    { Name: string
      Ctrl: bool
      Meta: bool
      Shift: bool }

/// A terminal input event: an optional raw character plus the parsed key.
/// (Terminal.UserInput)
type UserInput = { Input: string option; Key: Key }

/// Raised through the typed error channel when a user quits a `Terminal` prompt
/// (e.g. Ctrl+C / Ctrl+D) or the input stream ends. (Terminal.QuitError)
type QuitError = | QuitError

/// The command-line interface service. The fields are *unsafe* primitives the
/// accessors lift into effects (mirrors this port's `Console`). (Terminal.Terminal)
type Terminal =
    {
        ColumnsUnsafe: unit -> int
        RowsUnsafe: unit -> int
        /// `None` signals a quit / end-of-input (lifted to `QuitError`).
        ReadLineUnsafe: unit -> string option
        /// May throw; a throw is lifted to a `PlatformError`.
        DisplayUnsafe: string -> unit
    }

[<RequireQualifiedAccess>]
module Terminal =

    /// The `Tag` under which the `Terminal` service is stored. (Terminal.Terminal)
    let tag: Tag<Terminal> = Tag.make<Terminal> "effect/platform/Terminal"

    /// Construct a `Terminal` from its capabilities. (Terminal.make)
    let make
        (columns: unit -> int)
        (rows: unit -> int)
        (readLine: unit -> string option)
        (display: string -> unit)
        : Terminal =
        { ColumnsUnsafe = columns
          RowsUnsafe = rows
          ReadLineUnsafe = readLine
          DisplayUnsafe = display }

    /// Whether a value is a `QuitError`. (Terminal.isQuitError)
    let isQuitError (u: obj) : bool =
        match u with
        | :? QuitError -> true
        | _ -> false

    // The live terminal's primitives. On .NET they are `System.Console`-backed; on
    // the Fable/JS target none of `System.Console`'s `TextWriter`/`TextReader`,
    // `WindowWidth`/`WindowHeight` are supported, so we back the *same* primitives
    // with Node's `process` stdio. Both paths keep identical semantics: dimensions
    // fall back to 80×25 when stdout is redirected / not a TTY, and `readLine`
    // returns `None` at end-of-input. (severity: refactor — JS platform layer)
#if FABLE_COMPILER
    // `process.stdout.columns`/`.rows` are `undefined` when stdout is not a TTY
    // (piped/redirected); fall back to 80×25, matching the .NET redirected case.
    [<Fable.Core.Emit("(typeof process.stdout.columns === 'number' ? process.stdout.columns : 80)")>]
    let private columnsUnsafe () : int = failwith "Fable emit only"

    [<Fable.Core.Emit("(typeof process.stdout.rows === 'number' ? process.stdout.rows : 25)")>]
    let private rowsUnsafe () : int = failwith "Fable emit only"

    // `Console.Out.Write` writes to stdout with no trailing newline — `process.stdout.write`
    // is the exact analogue. A thrown JS error (e.g. EPIPE) propagates to `display`'s
    // `try/with`, which lifts it to a `PlatformError`.
    [<Fable.Core.Emit("process.stdout.write($0)")>]
    let private displayUnsafe (text: string) : unit = failwith "Fable emit only"

    // `node:fs`'s synchronous `readSync`, imported (ESM-safe) and threaded into the
    // read loop below — `require` is undefined under Node's default ESM module mode.
    [<Fable.Core.Import("readSync", "node:fs")>]
    let private fsReadSync: obj = failwith "Fable emit only"

    // Synchronous line read from fd 0, mirroring .NET's blocking `Console.In.ReadLine`.
    // Raw bytes are accumulated and UTF-8-decoded once, so multibyte input stays intact.
    // Returns `null` at end-of-input with nothing read (lifted to `None`); an empty line
    // returns "". Best-effort for interactive raw-TTY (the rich keypress `readInput`
    // stream stays deferred, per the module header).
    [<Fable.Core.Emit("""(function (readSync) {
  const buf = Buffer.alloc(1);
  const bytes = [];
  let got = false;
  while (true) {
    let n;
    try { n = readSync(0, buf, 0, 1, null); }
    catch (e) {
      if (e.code === 'EAGAIN') { continue; }
      else if (e.code === 'EOF') { n = 0; }
      else { throw e; }
    }
    if (n === 0) { break; }
    got = true;
    const b = buf[0];
    if (b === 10) { break; }     // '\n'
    if (b === 13) { continue; }  // '\r'
    bytes.push(b);
  }
  if (!got) { return null; }
  return Buffer.from(bytes).toString('utf8');
})($0)""")>]
    let private readLineRaw (readSync: obj) : string = failwith "Fable emit only"

    let private readLineUnsafe () : string option =
        match readLineRaw fsReadSync with
        | null -> None
        | s -> Some s
#else
    let private columnsUnsafe () : int =
        try
            if System.Console.IsOutputRedirected then
                80
            else
                System.Console.WindowWidth
        with _ ->
            80

    let private rowsUnsafe () : int =
        try
            if System.Console.IsOutputRedirected then
                25
            else
                System.Console.WindowHeight
        with _ ->
            25

    let private displayUnsafe (text: string) : unit = System.Console.Out.Write(text: string)

    let private readLineUnsafe () : string option =
        match System.Console.In.ReadLine() with
        | null -> None
        | s -> Some s
#endif

    /// The live terminal, backed by `System.Console` on .NET and by Node `process`
    /// stdio under Fable. Dimensions fall back to 80×25 when the console is
    /// redirected / unavailable; `readLine` returns `None` at end-of-input.
    let live: Terminal =
        { ColumnsUnsafe = columnsUnsafe
          RowsUnsafe = rowsUnsafe
          ReadLineUnsafe = readLineUnsafe
          DisplayUnsafe = displayUnsafe }

    /// A `Context` carrying the live terminal. (Terminal layer context)
    let liveContext: Context = Context.make tag live

    /// The default `System.Console`-backed `Terminal` layer. (NodeTerminal.layer)
    let layer<'E, 'RIn> : Layer<'E, 'RIn> = Layer.succeed tag live

    /// Build an effect from the active `Terminal` service, falling back to `live`
    /// when none is provided. (Terminal.Terminal accessor root)
    let terminalWith (f: Terminal -> Effect<'A, 'E, Context>) : Effect<'A, 'E, Context> =
        Effect.environment<Context, 'E>
        |> Effect.flatMap (fun ctx ->
            match Context.tryGet tag ctx with
            | Some t -> f t
            | None -> f live)

    /// The number of columns available on the terminal. (Terminal.columns)
    let columns<'E> : Effect<int, 'E, Context> =
        terminalWith (fun t -> Effect.sync (fun () -> t.ColumnsUnsafe()))

    /// The number of rows available on the terminal. (Terminal.rows)
    let rows<'E> : Effect<int, 'E, Context> =
        terminalWith (fun t -> Effect.sync (fun () -> t.RowsUnsafe()))

    /// Read a single line of input; fails with `QuitError` at end-of-input or on
    /// user cancellation. (Terminal.readLine)
    let readLine: Effect<string, QuitError, Context> =
        terminalWith (fun t ->
            Effect(fun _ _ ->
                async {
                    match t.ReadLineUnsafe() with
                    | Some line -> return Success line
                    | None -> return Exit.fail QuitError
                }))

    /// Display text to the terminal; a write failure becomes a `PlatformError`.
    /// (Terminal.display)
    let display (text: string) : Effect<unit, PlatformError, Context> =
        terminalWith (fun t ->
            Effect(fun _ _ ->
                async {
                    try
                        t.DisplayUnsafe text
                        return Success()
                    with ex ->
                        return
                            Exit.fail (
                                PlatformError.badArgument
                                    { Module = "Terminal"
                                      Method = "display"
                                      Description = Some "Failed to write to stdout"
                                      Cause = Some(box ex) }
                            )
                }))
