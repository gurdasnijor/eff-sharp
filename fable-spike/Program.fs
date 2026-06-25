module Program

open Effect

// ---- tiny test harness (Fable-friendly: just console output) ----

let mutable passed = 0
let mutable failed = 0

let pass (name: string) (detail: string) =
    passed <- passed + 1
    printfn "  PASS  %s  ->  %s" name detail

let fail (name: string) (detail: string) =
    failed <- failed + 1
    printfn "  FAIL  %s  ->  %s" name detail

let checkEq (name: string) (expected: 'a) (actual: 'a) =
    if expected = actual then pass name (sprintf "%A" actual)
    else fail name (sprintf "expected %A, got %A" expected actual)

// ---------------------------------------------------------------
// SECTION A — runSync (the brief's canonical first test)
// Async.RunSynchronously is the suspected hard blocker on JS.
// Isolated in its own try so its failure does not mask the rest.
// ---------------------------------------------------------------
let testRunSync () =
    try
        let eff = Effect.succeed 1 |> Effect.map (fun x -> x + 1)
        match Effect.runSync () eff with
        | Success v -> checkEq "A.runSync (succeed |> map (+1))" 2 v
        | Failure c -> fail "A.runSync" (sprintf "Failure %s" (Cause.render c))
    with ex ->
        fail "A.runSync" (sprintf "THREW: %s" ex.Message)

// ---------------------------------------------------------------
// SECTION B..F — run through the *async* path (Effect.runExit),
// which is what a JS host would actually use. We assemble one
// async workflow and bridge it to JS at the end.
// ---------------------------------------------------------------

let asyncTests : Async<unit> =
    async {
        // B. succeed |> map  (async path)
        let! e1 = Effect.runExit () (Effect.succeed 1 |> Effect.map (fun x -> x + 1))
        match e1 with
        | Success v -> checkEq "B.runExit (succeed |> map (+1))" 2 v
        | Failure c -> fail "B.runExit map" (Cause.render c)

        // C. fail |> catchAll
        let recovered =
            Effect.fail "boom"
            |> Effect.catchAll (fun (msg: string) -> Effect.succeed (msg.Length))
        let! e2 = Effect.runExit () recovered
        match e2 with
        | Success v -> checkEq "C.fail |> catchAll" 4 v
        | Failure c -> fail "C.catchAll" (Cause.render c)

        // D. flatMap chain
        let chain =
            Effect.succeed 10
            |> Effect.flatMap (fun a -> Effect.succeed (a * 2))
            |> Effect.flatMap (fun b -> Effect.succeed (b + 1))
        let! e3 = Effect.runExit () chain
        match e3 with
        | Success v -> checkEq "D.flatMap chain (10*2+1)" 21 v
        | Failure c -> fail "D.flatMap" (Cause.render c)

        // E. Ref make / get / update
        let refProg =
            Ref.make 0
            |> Effect.flatMap (fun r ->
                Ref.update r (fun n -> n + 5)
                |> Effect.flatMap (fun () -> Ref.update r (fun n -> n * 2))
                |> Effect.flatMap (fun () -> Ref.get r))
        let! e4 = Effect.runExit () refProg
        match e4 with
        | Success v -> checkEq "E.Ref get/update ((0+5)*2)" 10 v
        | Failure c -> fail "E.Ref" (Cause.render c)

        // F. fork / join  — THE real test: does the Async/Task fiber model
        //    run on Node's single-threaded event loop?
        let forkProg =
            Effect.succeed 21
            |> Effect.map (fun x -> x * 2)
            |> Effect.fork
            |> Effect.flatMap (fun fib -> Effect.join fib)
        let! e5 = Effect.runExit () forkProg
        match e5 with
        | Success v -> checkEq "F.fork |> join (21*2)" 42 v
        | Failure c -> fail "F.fork/join" (Cause.render c)

        // G. fork two fibers, join both, combine
        let twoFibers =
            Effect.fork (Effect.succeed 1)
            |> Effect.flatMap (fun f1 ->
                Effect.fork (Effect.succeed 2)
                |> Effect.flatMap (fun f2 ->
                    Effect.join f1
                    |> Effect.flatMap (fun a -> Effect.join f2 |> Effect.map (fun b -> a + b))))
        let! e6 = Effect.runExit () twoFibers
        match e6 with
        | Success v -> checkEq "G.two fibers join (1+2)" 3 v
        | Failure c -> fail "G.twoFibers" (Cause.render c)

        printfn ""
        printfn "==== RESULTS: %d passed, %d failed ====" passed failed
    }

[<EntryPoint>]
let main _ =
    printfn "==== eff-sharp Fable spike ===="
    printfn "-- Section A: runSync (Async.RunSynchronously) --"
    testRunSync ()
    printfn "-- Sections B..G: async path (Effect.runExit) --"
    // Bridge the async workflow to the JS event loop without blocking.
    Async.StartImmediate asyncTests
    0
