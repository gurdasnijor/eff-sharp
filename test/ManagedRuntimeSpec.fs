module ManagedRuntimeSpec

open Effect
open Effect.Vitest

let private hasInterrupt (cause: Cause<'E>) : bool =
    cause.Reasons
    |> List.exists (function
        | Reason.Interrupt _ -> true
        | _ -> false)

describe "ManagedRuntime" (fun () ->
    test "memoizes the layer build" (fun () ->
        let count = ref 0

        let layer: Layer<string, unit> =
            Layer.effectContext (
                Effect.sync (fun () ->
                    count.Value <- count.Value + 1
                    Context.empty)
            )

        let runtime = ManagedRuntime.make layer
        ManagedRuntime.runSync runtime (Effect.succeed ()) |> ignore
        ManagedRuntime.runSync runtime (Effect.succeed ()) |> ignore
        Effect.runSync () (ManagedRuntime.disposeEffect runtime) |> ignore
        toBe count.Value 1)

    test "provides context" (fun () ->
        let tag = Tag.make<string> "string"
        let runtime = ManagedRuntime.make (Layer.succeed tag "test")
        let result = ManagedRuntime.runSync runtime (Effect.service tag)
        Effect.runSync () (ManagedRuntime.disposeEffect runtime) |> ignore
        toBe result "test")

    test "can be built synchronously" (fun () ->
        let tag = Tag.make<string> "string"
        let runtime = ManagedRuntime.make (Layer.succeed tag "test")

        match Effect.runSync () (ManagedRuntime.contextEffect runtime) with
        | Success services -> toBe (Context.get tag services) "test"
        | Failure c -> failwith (Cause.render c))

    test "allows sharing a MemoMap" (fun () ->
        let count = ref 0

        let layer: Layer<string, unit> =
            Layer.effectContext (
                Effect.sync (fun () ->
                    count.Value <- count.Value + 1
                    Context.empty)
            )

        let runtimeA = ManagedRuntime.make layer
        let runtimeB = ManagedRuntime.makeWith (ManagedRuntime.memoMap runtimeA) layer
        ManagedRuntime.runSync runtimeA (Effect.succeed ()) |> ignore
        ManagedRuntime.runSync runtimeB (Effect.succeed ()) |> ignore
        Effect.runSync () (ManagedRuntime.disposeEffect runtimeA) |> ignore
        Effect.runSync () (ManagedRuntime.disposeEffect runtimeB) |> ignore
        toBe count.Value 1)

    itEffect "runPromise resolves to the value" (fun () ->
        let tag = Tag.make<string> "string"
        let runtime = ManagedRuntime.make (Layer.succeed tag "done")

        Effect(fun _ _ ->
            async {
                let! result = ManagedRuntime.runPromise runtime (Effect.service tag)
                Effect.runSync () (ManagedRuntime.disposeEffect runtime) |> ignore
                toBe result "done"
                return Success()
            }))

    itEffect "fibers are interrupted on dispose" (fun () ->
        let runtime = ManagedRuntime.make (Layer.empty: Layer<string, unit>)
        let never: Effect<unit, string, Context> = Effect.sleep 1_000_000
        let fiber = ManagedRuntime.runFork runtime never

        Effect(fun _ _ ->
            async {
                let! exit =
                    ManagedRuntime.disposeEffect runtime
                    |> Effect.zipRight (Effect.await fiber)
                    |> Effect.runExit ()

                match exit with
                | Success(Failure c) -> toBe (hasInterrupt c) true
                | Success(Success _) -> failwith "expected the forked fiber to be interrupted"
                | Failure c -> failwith (Cause.render c)

                return Success()
            })))
