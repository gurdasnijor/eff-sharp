namespace Effect

/// `Console` — console output as a `Context` service.
///
/// Port of repos/effect-smol/packages/effect/src/Console.ts. The `Console`
/// service exposes the familiar console surface (log/info/warn/error/debug,
/// counters, groups, timers, tables); routing through a service lets tests
/// capture output or supply a custom console. Scoped helpers (`group`/`time`)
/// and effect-wrapping helpers (`withGroup`/`withTime`) open and close groups or
/// timers automatically.
///
/// Decoupling / omissions (per CONVENTIONS):
///   * The active service is a `Context.Reference` with `live` as its default,
///     matching Effect v4's replacement for FiberRef-backed services.
///   * JS variadic `...args: any[]` is modelled as `obj[]`.
///   * The scoped helpers take an explicit `Scope` (the port's `Scope` is
///     passed, not an environment requirement) rather than requiring `Scope` in
///     `R`.
///   * The `live` console is best-effort: log/info/debug → stdout,
///     warn/error/trace → stderr, group prints its label; count/time/table/dir
///     are no-ops.
/// The console service. Mirrors upstream's `Console` interface (variadic args
/// are `obj[]`).
type Console =
    { Assert: bool -> obj[] -> unit
      Clear: unit -> unit
      Count: string option -> unit
      CountReset: string option -> unit
      Debug: obj[] -> unit
      Dir: obj -> obj option -> unit
      Dirxml: obj[] -> unit
      Error: obj[] -> unit
      Group: string option -> unit
      GroupCollapsed: string option -> unit
      GroupEnd: unit -> unit
      Info: obj[] -> unit
      Log: obj[] -> unit
      Table: obj -> string[] option -> unit
      Time: string option -> unit
      TimeEnd: string option -> unit
      TimeLog: string option -> obj[] -> unit
      Trace: obj[] -> unit
      Warn: obj[] -> unit }

[<RequireQualifiedAccess>]
module Console =

    let private render (args: obj[]) : string =
        args |> Array.map string |> String.concat " "

    /// The live, best-effort console over Node stdout/stderr.
    let live: Console =
        let writeOut (s: string) = printfn "%s" s
        let writeErr (s: string) = eprintfn "%s" s

        let toOut (args: obj[]) = writeOut (render args)
        let toErr (args: obj[]) = writeErr (render args)

        { Assert =
            fun condition args ->
                if not condition then
                    toErr (Array.append [| box "Assertion failed:" |] args)
          Clear = fun () -> ()
          Count = fun _ -> ()
          CountReset = fun _ -> ()
          Debug = toOut
          Dir = fun item _ -> writeOut (string item)
          Dirxml = toOut
          Error = toErr
          Group = fun label -> label |> Option.iter writeOut
          GroupCollapsed = fun label -> label |> Option.iter writeOut
          GroupEnd = fun () -> ()
          Info = toOut
          Log = toOut
          Table = fun item _ -> writeOut (string item)
          Time = fun _ -> ()
          TimeEnd = fun _ -> ()
          TimeLog = fun _ _ -> ()
          Trace = toErr
          Warn = toErr }

    /// The defaulted `Context.Reference` under which the active console service is stored.
    let reference: Reference<Console> = Reference.make "effect/Console" live

    /// Compatibility tag for Layer/Context APIs. Values provided here override `reference`.
    let tag: Tag<Console> = Reference.toTag reference

    /// A `Context` carrying the live console.
    let liveContext: Context = Context.make tag live

    /// Build an effect from the active `Console` service (falling back to `live`
    /// when none is provided). (Console.consoleWith)
    let consoleWith (f: Console -> Effect<'A, 'E, Context>) : Effect<'A, 'E, Context> =
        Effect.serviceReference reference |> Effect.flatMap f

    // --- simple accessors ---

    /// Logs an assertion failure as an error when `condition` is false. (Console.assert)
    let assertLog (condition: bool) (args: obj[]) : Effect<unit, 'E, Context> =
        consoleWith (fun c -> Effect.sync (fun () -> c.Assert condition args))

    let clear<'E> : Effect<unit, 'E, Context> =
        consoleWith (fun c -> Effect.sync c.Clear)

    let count (label: string option) : Effect<unit, 'E, Context> =
        consoleWith (fun c -> Effect.sync (fun () -> c.Count label))

    let countReset (label: string option) : Effect<unit, 'E, Context> =
        consoleWith (fun c -> Effect.sync (fun () -> c.CountReset label))

    let debug (args: obj[]) : Effect<unit, 'E, Context> =
        consoleWith (fun c -> Effect.sync (fun () -> c.Debug args))

    let dir (item: obj) (options: obj option) : Effect<unit, 'E, Context> =
        consoleWith (fun c -> Effect.sync (fun () -> c.Dir item options))

    let dirxml (args: obj[]) : Effect<unit, 'E, Context> =
        consoleWith (fun c -> Effect.sync (fun () -> c.Dirxml args))

    let error (args: obj[]) : Effect<unit, 'E, Context> =
        consoleWith (fun c -> Effect.sync (fun () -> c.Error args))

    let info (args: obj[]) : Effect<unit, 'E, Context> =
        consoleWith (fun c -> Effect.sync (fun () -> c.Info args))

    let log (args: obj[]) : Effect<unit, 'E, Context> =
        consoleWith (fun c -> Effect.sync (fun () -> c.Log args))

    let table (data: obj) (properties: string[] option) : Effect<unit, 'E, Context> =
        consoleWith (fun c -> Effect.sync (fun () -> c.Table data properties))

    let timeLog (label: string option) (args: obj[]) : Effect<unit, 'E, Context> =
        consoleWith (fun c -> Effect.sync (fun () -> c.TimeLog label args))

    let trace (args: obj[]) : Effect<unit, 'E, Context> =
        consoleWith (fun c -> Effect.sync (fun () -> c.Trace args))

    let warn (args: obj[]) : Effect<unit, 'E, Context> =
        consoleWith (fun c -> Effect.sync (fun () -> c.Warn args))

    // --- scoped helpers (explicit Scope) ---

    /// Open a console group, closing it when `scope` closes. (Console.group)
    let group (scope: Scope<'E, Context>) (label: string option) (collapsed: bool) : Effect<unit, 'E, Context> =
        consoleWith (fun c ->
            Scope.acquireRelease
                scope
                (Effect.sync (fun () -> if collapsed then c.GroupCollapsed label else c.Group label))
                (fun () _ -> Effect.sync (fun () -> c.GroupEnd())))

    /// Start a console timer, ending it when `scope` closes. (Console.time)
    let time (scope: Scope<'E, Context>) (label: string option) : Effect<unit, 'E, Context> =
        consoleWith (fun c ->
            Scope.acquireRelease scope (Effect.sync (fun () -> c.Time label)) (fun () _ ->
                Effect.sync (fun () -> c.TimeEnd label)))

    // --- effect-wrapping helpers ---

    /// Run `self` inside a console group. (Console.withGroup)
    let withGroup (label: string option) (collapsed: bool) (self: Effect<'A, 'E, Context>) : Effect<'A, 'E, Context> =
        consoleWith (fun c ->
            Effect.acquireUseRelease
                (Effect.sync (fun () -> if collapsed then c.GroupCollapsed label else c.Group label))
                (fun () -> self)
                (fun () _ -> Effect.sync (fun () -> c.GroupEnd())))

    /// Run `self` inside a console timer. (Console.withTime)
    let withTime (label: string option) (self: Effect<'A, 'E, Context>) : Effect<'A, 'E, Context> =
        consoleWith (fun c ->
            Effect.acquireUseRelease (Effect.sync (fun () -> c.Time label)) (fun () -> self) (fun () _ ->
                Effect.sync (fun () -> c.TimeEnd label)))
