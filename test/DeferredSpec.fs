module DeferredSpec

open Effect
open Effect.Vitest

let private same actual expected = toBe (actual = expected) true

let private runBool (eff: Effect<bool, string, unit>) : bool =
    match Effect.runSync () eff with
    | Success b -> b
    | Failure c -> failwithf "unexpected failure: %s" (Cause.render c)

let private runExit (eff: Effect<'A, 'E, unit>) : Exit<'A, 'E> = Effect.runSync () eff

describe "Deferred" (fun () ->
    test "succeed propagates the value once" (fun () ->
        let deferred = Deferred.makeUnsafe<int, string> ()
        toBe (runBool (Deferred.succeed deferred 1)) true
        toBe (runBool (Deferred.succeed deferred 1)) false
        same (runExit (Deferred.await deferred)) (Exit.succeed 1))

    test "complete memoizes the result of the provided effect" (fun () ->
        let value = ref 0

        let complete: Effect<int, string, unit> =
            Effect.sync (fun () ->
                value.Value <- value.Value + 1
                value.Value)

        let deferred = Deferred.makeUnsafe<int, string> ()
        toBe (runBool (Deferred.complete deferred complete)) true
        toBe (runBool (Deferred.complete deferred complete)) false
        toBe value.Value 1
        same (runExit (Deferred.await deferred)) (Exit.succeed 1)
        same (runExit (Deferred.await deferred)) (Exit.succeed 1)
        toBe value.Value 1)

    test "completeWith stores the effect without memoizing its result" (fun () ->
        let value = ref 0

        let complete: Effect<int, string, unit> =
            Effect.sync (fun () ->
                value.Value <- value.Value + 1
                value.Value)

        let complete2: Effect<int, string, unit> =
            Effect.sync (fun () ->
                value.Value <- value.Value + 10
                value.Value)

        let deferred = Deferred.makeUnsafe<int, string> ()
        toBe (runBool (Deferred.completeWith deferred complete)) true
        toBe (runBool (Deferred.completeWith deferred complete2)) false
        toBe value.Value 0
        same (runExit (Deferred.await deferred)) (Exit.succeed 1)
        same (runExit (Deferred.await deferred)) (Exit.succeed 2)
        toBe value.Value 2)

    test "fail propagates the failure" (fun () ->
        let deferred = Deferred.makeUnsafe<int, string> ()
        toBe (runBool (Deferred.fail deferred "boom")) true
        toBe (runBool (Deferred.succeed deferred 1)) false
        same (runExit (Deferred.await deferred)) (Exit.fail "boom"))

    test "complete memoizes the failure of the provided effect" (fun () ->
        let deferred = Deferred.makeUnsafe<int, string> ()
        toBe (runBool (Deferred.complete deferred (Effect.fail "boom"))) true
        toBe (runBool (Deferred.complete deferred (Effect.fail "boom2"))) false
        same (runExit (Deferred.await deferred)) (Exit.fail "boom")
        same (runExit (Deferred.await deferred)) (Exit.fail "boom"))

    test "completeWith stores a failing effect without memoizing its result" (fun () ->
        let i = ref 0
        let failures = [| "boom"; "boom2" |]

        let complete: Effect<int, string, unit> =
            Effect.sync (fun () ->
                let v = failures.[i.Value]
                i.Value <- i.Value + 1
                v)
            |> Effect.flatMap Effect.fail

        let complete2: Effect<int, string, unit> = Effect.fail "boom3"

        let deferred = Deferred.makeUnsafe<int, string> ()
        toBe (runBool (Deferred.completeWith deferred complete)) true
        toBe (runBool (Deferred.completeWith deferred complete2)) false
        same (runExit (Deferred.await deferred)) (Exit.fail "boom")
        same (runExit (Deferred.await deferred)) (Exit.fail "boom2"))

    test "failCauseSync propagates a lazily computed cause" (fun () ->
        let deferred = Deferred.makeUnsafe<int, string> ()
        toBe (runBool (Deferred.failCauseSync deferred (fun () -> Cause.fail "boom"))) true
        toBe (runBool (Deferred.failCauseSync deferred (fun () -> Cause.fail "boom2"))) false
        same (runExit (Deferred.await deferred)) (Exit.fail "boom"))

    test "failCauseSync does not evaluate the thunk until the effect runs" (fun () ->
        let evaluated = ref false
        let deferred = Deferred.makeUnsafe<int, string> ()

        let completion =
            Deferred.failCauseSync deferred (fun () ->
                evaluated.Value <- true
                Cause.fail "boom")

        toBe evaluated.Value false
        toBe (runBool completion) true
        toBe evaluated.Value true)

    test "dieSync propagates a lazily computed defect" (fun () ->
        let deferred = Deferred.makeUnsafe<int, string> ()
        toBe (runBool (Deferred.dieSync deferred (fun () -> box "boom"))) true
        toBe (runBool (Deferred.dieSync deferred (fun () -> box "boom2"))) false

        match runExit (Deferred.await deferred) with
        | Failure cause ->
            match Cause.defects cause with
            | [ defect ] -> toBe (string defect) "boom"
            | defects -> failwithf "expected one defect, got %d" defects.Length
        | Success value -> failwithf "expected defect, got %d" value)

    test "dieSync does not evaluate the thunk until the effect runs" (fun () ->
        let evaluated = ref false
        let deferred = Deferred.makeUnsafe<int, string> ()

        let completion =
            Deferred.dieSync deferred (fun () ->
                evaluated.Value <- true
                box "boom")

        toBe evaluated.Value false
        toBe (runBool completion) true
        toBe evaluated.Value true)

    test "interrupt propagates interruption" (fun () ->
        let deferred = Deferred.makeUnsafe<int, string> ()
        toBe (runBool (Deferred.interruptWith deferred -1)) true
        toBe (runBool (Deferred.interrupt deferred)) false
        same (runExit (Deferred.await deferred)) (Exit.failCause (Cause.interrupt (Some -1))))

    test "poll returns None when incomplete" (fun () ->
        let deferred = Deferred.makeUnsafe<int, string> ()
        match runExit (Deferred.poll deferred) with
        | Success None -> ()
        | Success(Some _) -> failwith "expected None, got Some"
        | Failure cause -> failwithf "unexpected failure: %s" (Cause.render cause))

    test "poll returns Some when succeeded" (fun () ->
        let deferred = Deferred.makeUnsafe<int, string> ()
        runBool (Deferred.succeed deferred 1) |> ignore

        match runExit (Deferred.poll deferred) with
        | Success(Some inner) -> same (runExit inner) (Exit.succeed 1)
        | Success None -> failwith "expected Some, got None"
        | Failure cause -> failwithf "unexpected failure: %s" (Cause.render cause))

    test "poll returns Some when failed" (fun () ->
        let deferred = Deferred.makeUnsafe<int, string> ()
        runBool (Deferred.fail deferred "boom") |> ignore

        match runExit (Deferred.poll deferred) with
        | Success(Some inner) -> same (runExit inner) (Exit.fail "boom")
        | Success None -> failwith "expected Some, got None"
        | Failure cause -> failwithf "unexpected failure: %s" (Cause.render cause))

    test "poll returns Some when interrupted" (fun () ->
        let deferred = Deferred.makeUnsafe<int, string> ()
        runBool (Deferred.interruptWith deferred -1) |> ignore

        match runExit (Deferred.poll deferred) with
        | Success(Some inner) -> same (runExit inner) (Exit.failCause (Cause.interrupt (Some -1)))
        | Success None -> failwith "expected Some, got None"
        | Failure cause -> failwithf "unexpected failure: %s" (Cause.render cause))

    test "isDone returns true when succeeded or failed" (fun () ->
        let succeeded = Deferred.makeUnsafe<int, string> ()
        runBool (Deferred.succeed succeeded 1) |> ignore
        toBe (runBool (Deferred.isDone succeeded)) true

        let failed = Deferred.makeUnsafe<int, string> ()
        runBool (Deferred.fail failed "boom") |> ignore
        toBe (runBool (Deferred.isDone failed)) true))
