module Effect.Tests.ManagedRuntimeTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/ManagedRuntime.test.ts,
// adapted to the port (single error channel, explicit-scope Layer; see
// ManagedRuntime.fs). Promise runners are exercised synchronously.

let private hasInterrupt (cause: Cause<'E>) : bool =
    cause.Reasons
    |> List.exists (function
        | Reason.Interrupt _ -> true
        | _ -> false)

[<Fact>]
let ``memoizes the layer build`` () =
    let count = ref 0

    let layer: Layer<string, unit> =
        Layer.effectContext (
            Effect.sync (fun () ->
                incr count
                Context.empty)
        )

    let runtime = ManagedRuntime.make layer
    ManagedRuntime.runSync runtime (Effect.succeed ()) |> ignore
    ManagedRuntime.runSync runtime (Effect.succeed ()) |> ignore
    Effect.runSync () (ManagedRuntime.disposeEffect runtime) |> ignore
    Assert.Equal(1, count.Value)

[<Fact>]
let ``provides context`` () =
    let tag = Tag.make<string> "string"
    let runtime = ManagedRuntime.make (Layer.succeed tag "test")
    let result = ManagedRuntime.runSync runtime (Effect.service tag)
    Effect.runSync () (ManagedRuntime.disposeEffect runtime) |> ignore
    Assert.Equal("test", result)

[<Fact>]
let ``can be built synchronously`` () =
    let tag = Tag.make<string> "string"
    let runtime = ManagedRuntime.make (Layer.succeed tag "test")

    match Effect.runSync () (ManagedRuntime.contextEffect runtime) with
    | Success services -> Assert.Equal("test", Context.get tag services)
    | Failure c -> Assert.Fail(Cause.render c)

[<Fact>]
let ``allows sharing a MemoMap`` () =
    let count = ref 0

    let layer: Layer<string, unit> =
        Layer.effectContext (
            Effect.sync (fun () ->
                incr count
                Context.empty)
        )

    let runtimeA = ManagedRuntime.make layer
    let runtimeB = ManagedRuntime.makeWith (ManagedRuntime.memoMap runtimeA) layer
    ManagedRuntime.runSync runtimeA (Effect.succeed ()) |> ignore
    ManagedRuntime.runSync runtimeB (Effect.succeed ()) |> ignore
    Effect.runSync () (ManagedRuntime.disposeEffect runtimeA) |> ignore
    Effect.runSync () (ManagedRuntime.disposeEffect runtimeB) |> ignore
    Assert.Equal(1, count.Value)

[<Fact>]
let ``runPromise resolves to the value`` () =
    let tag = Tag.make<string> "string"
    let runtime = ManagedRuntime.make (Layer.succeed tag "done")

    let result =
        ManagedRuntime.runPromise runtime (Effect.service tag)
        |> Async.AwaitTask
        |> Async.RunSynchronously

    Effect.runSync () (ManagedRuntime.disposeEffect runtime) |> ignore
    Assert.Equal("done", result)

[<Fact>]
let ``fibers are interrupted on dispose`` () =
    let runtime = ManagedRuntime.make (Layer.empty: Layer<string, unit>)
    let never: Effect<unit, string, Context> = Effect.sleep 1_000_000
    let fiber = ManagedRuntime.runFork runtime never
    Effect.runSync () (ManagedRuntime.disposeEffect runtime) |> ignore

    match Effect.runSync () (Effect.await fiber) with
    | Success exit ->
        match exit with
        | Failure c -> Assert.True(hasInterrupt c, "expected an interrupt cause")
        | Success _ -> Assert.Fail "expected the forked fiber to be interrupted"
    | Failure c -> Assert.Fail(Cause.render c)
