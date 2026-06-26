namespace Effect

/// `ExecutionPlan` — ordered fallback steps for running effects.
///
/// Port of repos/effect-smol/packages/effect/src/ExecutionPlan.ts. A plan is a
/// non-empty list of steps; each step provides a `Layer` and may retry up to
/// `attempts` times. The executor tries steps in order until one succeeds or the
/// plan is exhausted.
///
/// SCOPE (per CONVENTIONS "core surface + note deferrals"): upstream's *execution*
/// lives on `Stream.withExecutionPlan` / `Effect.withExecutionPlan` (in
/// `internal/effect`), not in this module. This port provides the plan **data
/// model** (`make`/`merge`/`Metadata`) plus an **Effect-level** executor
/// (`withExecutionPlan`) that falls back through failing layers with per-step
/// `attempts` retries.
///
/// Deferred (noted): the `Stream` integration and partial-stream fallback rules;
/// per-step `Schedule` and `while` predicates; input/error row-types (the single
/// error channel uses one `'E`).
/// One fallback step: a layer to provide, with a retry budget.
type ExecutionStep<'E> =
    { Provide: Layer<'E, unit>
      Attempts: int }

/// An ordered, non-empty list of fallback steps.
type ExecutionPlan<'E> =
    internal
        { Steps: ExecutionStep<'E> list }

/// Metadata describing the active step/attempt. (ExecutionPlan.Metadata)
type Metadata = { Attempt: int; StepIndex: int }

[<RequireQualifiedAccess>]
module ExecutionPlan =

    /// Metadata for the currently running execution-plan attempt.
    let CurrentMetadata: Reference<Metadata> =
        Reference.make "effect/ExecutionPlan/CurrentMetadata" { Attempt = 0; StepIndex = 0 }

    /// A single step that provides `layer` and is attempted once.
    let step (layer: Layer<'E, unit>) : ExecutionStep<'E> = { Provide = layer; Attempts = 1 }

    /// A single step that provides `layer`, retrying up to `attempts` times.
    let stepWith (layer: Layer<'E, unit>) (attempts: int) : ExecutionStep<'E> =
        { Provide = layer; Attempts = attempts }

    /// Build a plan from steps, rejecting any `attempts < 1`. (ExecutionPlan.make)
    let make (steps: ExecutionStep<'E> list) : ExecutionPlan<'E> =
        steps
        |> List.iteri (fun i s ->
            if s.Attempts < 1 then
                raise (System.Exception(sprintf "ExecutionPlan.make: step[%d].attempts must be greater than 0" i)))

        { Steps = steps }

    /// The plan's steps.
    let steps (plan: ExecutionPlan<'E>) : ExecutionStep<'E> list = plan.Steps

    /// Concatenate plans' steps in order. (ExecutionPlan.merge)
    let merge (plans: ExecutionPlan<'E> list) : ExecutionPlan<'E> =
        { Steps = plans |> List.collect (fun p -> p.Steps) }

    /// Run `effect`, trying each step's layer in order (with per-step retries),
    /// falling back on failure until one succeeds or the plan is exhausted. The
    /// last step's failure propagates. (Effect.withExecutionPlan)
    let withExecutionPlan (plan: ExecutionPlan<'E>) (effect: Effect<'A, 'E, Context>) : Effect<'A, 'E, unit> =
        let runStep (stepIndex: int) (s: ExecutionStep<'E>) : Effect<'A, 'E, unit> =
            let rec withRetries (attempt: int) (remaining: int) : Effect<'A, 'E, unit> =
                let metadata = { Attempt = attempt; StepIndex = stepIndex }

                let once =
                    effect
                    |> Effect.provideServiceReference CurrentMetadata metadata
                    |> Layer.provide s.Provide

                if remaining <= 1 then
                    once
                else
                    once |> Effect.catchAll (fun _ -> withRetries (attempt + 1) (remaining - 1))

            withRetries 1 s.Attempts

        let rec tryFrom (stepIndex: int) (remaining: ExecutionStep<'E> list) : Effect<'A, 'E, unit> =
            match remaining with
            | [] -> Effect.failCause (Cause.die (box "ExecutionPlan: empty plan"))
            | [ last ] -> runStep stepIndex last
            | s :: rest -> runStep stepIndex s |> Effect.catchAll (fun _ -> tryFrom (stepIndex + 1) rest)

        tryFrom 0 plan.Steps
