namespace EffSharp.AiDocs

/// Shared helpers used across the examples. Nothing here is part of the public
/// eff-sharp API — these just keep the example bodies focused on the concept.
module Shared =

    open Effect

    /// Run an effect that needs no environment and unwrap its success value,
    /// turning any failure into a thrown exception. In real code you would match
    /// the `Exit` (see `05_Running`) instead of throwing — this is sugar for docs.
    let run (eff: Effect<'A, 'E, unit>) : 'A =
        match Effect.runSync () eff with
        | Success a -> a
        | Failure cause -> failwithf "effect failed: %s" (Cause.render cause)

    /// Run an effect that needs a `Context` (e.g. anything using `Console` or a
    /// service) against the supplied context.
    let runWith (ctx: Context) (eff: Effect<'A, 'E, Context>) : 'A =
        match Effect.runSync ctx eff with
        | Success a -> a
        | Failure cause -> failwithf "effect failed: %s" (Cause.render cause)

    /// A doc-only assertion. Stands in for `Assert.Equal` / vitest `assert` so the
    /// testing examples don't drag a test framework into the docs project.
    let check (label: string) (condition: bool) : unit =
        if condition then printfn "    PASS  %s" label
        else failwithf "    FAIL  %s" label

    /// Print a section banner.
    let banner (title: string) : unit =
        printfn ""
        printfn "== %s ==" title
