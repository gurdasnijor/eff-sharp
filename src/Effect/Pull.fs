namespace Effect

/// Port of repos/effect-smol/packages/effect/src/Pull.ts.
///
/// A `Pull` is one low-level pull step: an `Effect` that produces a value, fails
/// with an ordinary error, or signals normal end-of-input with a `Done`
/// completion carrying a leftover. The separate `Done` signal lets stream
/// consumers tell normal completion apart from failure.
///
/// Modeling note: upstream puts `Cause.Done<Done>` in the effect's error channel
/// alongside the ordinary error (`E | Cause.Done<Done>`). The port's `Cause` has
/// no `Done` reason, and F# has no `E | Done` union type, so a Pull's error
/// channel is modeled as **`obj`** (boxed): ordinary errors and `Done<'L>`
/// completion markers coexist there, and a completion is detected by a runtime
/// test on the boxed value — exactly what upstream's `Cause.isDone` does.
///
/// The Effect-kernel-coupled helpers (`matchEffect`, `catchDone`) are
/// implemented by destructuring the effect's `Async<Exit>` directly; they need
/// no fiber/runtime scheduler.
/// Normal-completion marker carrying a leftover value. Upstream `Cause.Done<L>`.
type Done<'L> = { Leftover: 'L }

[<RequireQualifiedAccess>]
module Pull =

    /// A pull step: an effect whose error channel (`obj`) carries either an
    /// ordinary boxed error or a `Done<'L>` completion marker.
    type Pull<'A, 'R> = Effect<'A, obj, 'R>

    // --- Done detection (runtime, mirrors upstream Cause.isDone) ---

    /// Whether a boxed error value is a `Done<_>` completion marker (any
    /// leftover type), via reflection — the house-style runtime narrowing.
#if FABLE_COMPILER
    // Fable shim: runtime generics are erased, so reflection (typedefof / GetProperty)
    // cannot identify a `Done<_>`. A direct type test / downcast works precisely
    // because erasure makes every `Done<_>` the same runtime type. (severity: refactor)
    let isDone (error: obj) : bool =
        match error with
        | null -> false
        | :? Done<obj> -> true
        | _ -> false

    let private leftoverOf (doneMarker: obj) : obj = (doneMarker :?> Done<obj>).Leftover
#else
    let isDone (error: obj) : bool =
        match error with
        | null -> false
        | _ ->
            let t = error.GetType()
            t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Done<obj>>

    let private leftoverOf (doneMarker: obj) : obj =
        doneMarker.GetType().GetProperty("Leftover").GetValue(doneMarker)
#endif

    /// Whether a `Reason` is a `Fail` whose error is a `Done` signal.
    let isDoneFailure (reason: Reason<obj>) : bool =
        match reason with
        | Reason.Fail e -> isDone e
        | _ -> false

    /// Whether a `Cause` contains any `Done` failure.
    let isDoneCause (cause: Cause<obj>) : bool =
        cause.Reasons |> List.exists isDoneFailure

    // --- Cause-level filters (built from Filter, as upstream) ---

    /// Finds the first ordinary error in a cause (upstream `Cause.findError`):
    /// passes the error, fails with the whole cause when none is present.
    let findError: Filter<Cause<obj>, obj, Cause<obj>> =
        fun cause ->
            match
                cause.Reasons
                |> List.tryPick (function
                    | Reason.Fail e -> Some e
                    | _ -> None)
            with
            | Some e -> Ok e
            | None -> Error cause

    /// Extracts the `Done` marker from a cause when its first error is a `Done`;
    /// otherwise fails with the original cause.
    let filterDone: Filter<Cause<obj>, obj, Cause<obj>> =
        findError
        |> Filter.composePassthrough (fun e -> if isDone e then Ok e else Error e)

    /// Like `filterDone` (the done payload is not used).
    let filterDoneVoid: Filter<Cause<obj>, obj, Cause<obj>> = filterDone

    /// Extracts the leftover value carried by a `Done` completion.
    let filterDoneLeftover: Filter<Cause<obj>, obj, Cause<obj>> =
        findError
        |> Filter.composePassthrough (fun e -> if isDone e then Ok(leftoverOf e) else Error e)

    /// Keeps a cause only when it contains no `Done` failures.
    let filterNoDone: Filter<Cause<obj>, Cause<obj>, Cause<obj>> =
        Filter.fromPredicate (fun (cause: Cause<obj>) -> cause.Reasons |> List.forall (isDoneFailure >> not))

    /// Converts a cause into an `Exit`, treating a `Done` completion as success
    /// (its leftover becomes the value) and any other cause as failure.
    let doneExitFromCause (cause: Cause<obj>) : Exit<obj, obj> =
        match filterDoneLeftover cause with
        | Ok leftover -> Exit.succeed leftover
        | Error c -> Exit.failCause c

    // --- constructors ---

    /// A pull step that signals normal completion with `leftover`
    /// (upstream `Cause.done`).
    let halt (leftover: 'L) : Pull<'A, 'R> =
        Effect.failCause { Reasons = [ Reason.Fail(box { Leftover = leftover }) ] }

    /// A pull step that signals completion with no leftover.
    let haltVoid<'A, 'R> : Pull<'A, 'R> = halt ()

    // --- pattern matching (Effect-coupled, kernel-free) ---

    /// Handles all three pull outcomes: success, ordinary failure (non-done
    /// cause), and done completion (leftover). Subject-last.
    let matchEffect
        (onSuccess: 'A -> Effect<'B, 'E2, 'R>)
        (onFailure: Cause<obj> -> Effect<'B, 'E2, 'R>)
        (onDone: obj -> Effect<'B, 'E2, 'R>)
        (self: Pull<'A, 'R>)
        : Effect<'B, 'E2, 'R> =
        Effect(fun fib r ->
            async {
                let (Effect run) = self
                let! exit = run fib r

                match exit with
                | Success a ->
                    let (Effect run2) = onSuccess a
                    return! run2 fib r
                | Failure cause ->
                    match filterDoneLeftover cause with
                    | Ok leftover ->
                        let (Effect run2) = onDone leftover
                        return! run2 fib r
                    | Error c ->
                        let (Effect run2) = onFailure c
                        return! run2 fib r
            })

    /// Recovers from a `Done` completion with `f`, leaving successes and
    /// ordinary failures untouched. Subject-last.
    let catchDone (f: obj -> Pull<'A, 'R>) (self: Pull<'A, 'R>) : Pull<'A, 'R> =
        self |> matchEffect Effect.succeed Effect.failCause f
