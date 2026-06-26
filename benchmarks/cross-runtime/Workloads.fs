module Workloads

// eff-sharp side of the cross-runtime benchmark. Mirrors workloads.md / the
// upstream `effect` programs exactly. Compiled to JS by Fable; driven by
// effsharp.bench.mjs with the same tinybench config as the upstream side.
//
// Programs are built once at module init (top-level `let`), so each exported
// function measures interpreter *run* cost, not construction — matching the
// upstream side which prebuilds its programs too.

open Effect

#if FABLE_COMPILER
open Fable.Core
#endif

[<Literal>]
let private nChain = 1000

[<Literal>]
let private nForeach = 1000

[<Literal>]
let private nDeep = 5000

[<Literal>]
let private nErr = 1000

let private mapChainProgram: Effect<int, string, unit> =
    let mutable p: Effect<int, string, unit> = Effect.succeed 0
    for _ in 1..nChain do
        p <- p |> Effect.map (fun x -> x + 1)
    p

let private flatMapChainProgram: Effect<int, string, unit> =
    let mutable p: Effect<int, string, unit> = Effect.succeed 0
    for _ in 1..nChain do
        p <- p |> Effect.flatMap (fun x -> Effect.succeed (x + 1))
    p

let private forEachProgram: Effect<int list, string, unit> =
    Effect.forEach [ 0 .. nForeach - 1 ] (fun x -> Effect.succeed x)

let rec private count (n: int) : Effect<int, string, unit> =
    if n = 0 then
        Effect.succeed 0
    else
        Effect.succeed (n - 1) |> Effect.flatMap count

let private deepBindProgram: Effect<int, string, unit> = count nDeep

let private errorCatchProgram: Effect<int, string, unit> =
    let mutable p: Effect<int, string, unit> = Effect.succeed 0
    for _ in 1..nErr do
        p <-
            p
            |> Effect.flatMap (fun _ -> Effect.fail "e" |> Effect.catchAll (fun _ -> Effect.succeed 1))
    p

// Run via the async runner (runExit) so deep chains don't trip the runSync
// "effect suspended" guard, and bridge to a JS Promise so tinybench can await —
// mirroring upstream's runPromise.
#if FABLE_COMPILER
let inline private runP (eff: Effect<'a, 'e, unit>) : JS.Promise<unit> =
    async {
        let! _ = Effect.runExit () eff
        return ()
    }
    |> Async.StartAsPromise

let mapChain () : JS.Promise<unit> = runP mapChainProgram
let flatMapChain () : JS.Promise<unit> = runP flatMapChainProgram
let forEach () : JS.Promise<unit> = runP forEachProgram
let deepBind () : JS.Promise<unit> = runP deepBindProgram
let errorCatch () : JS.Promise<unit> = runP errorCatchProgram
#else
let private runP (eff: Effect<'a, 'e, unit>) : Async<unit> =
    async {
        let! _ = Effect.runExit () eff
        return ()
    }

let mapChain () = runP mapChainProgram |> Async.RunSynchronously
let flatMapChain () = runP flatMapChainProgram |> Async.RunSynchronously
let forEach () = runP forEachProgram |> Async.RunSynchronously
let deepBind () = runP deepBindProgram |> Async.RunSynchronously
let errorCatch () = runP errorCatchProgram |> Async.RunSynchronously
#endif
