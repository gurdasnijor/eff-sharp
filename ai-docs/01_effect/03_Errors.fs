/// @title Typed errors, recovery, and matching on Cause/Exit
///
/// eff-sharp keeps three failure notions distinct (just like Effect):
///   - **typed failures** (`'E`) — expected, recoverable, in the type signature
///   - **defects** (`Die`)        — unexpected bugs/exceptions
///   - **interruption**           — a fiber was cancelled
/// All three live in a `Cause`, which an `Exit` carries on failure.
///
/// Mirrors upstream `01_effect/03_errors`. ⭐ The F# edge: instead of a family of
/// `Effect.catchTag` / `Effect.match*` combinators, you model errors as a normal
/// discriminated union and recover with **native `match`** — exhaustive, and the
/// compiler tells you when you miss a case.
module EffSharp.AiDocs.Errors

open Effect
open EffSharp.AiDocs.Shared

// ---------------------------------------------------------------------------
// Model the error domain as a DU. Each case is a "tag" — but it's just data.
// ---------------------------------------------------------------------------

type OrderError =
    | OutOfStock of sku: string
    | PaymentDeclined of reason: string
    | InvalidQuantity of qty: int

/// A computation whose failures are fully described by `OrderError`.
let placeOrder (sku: string) (qty: int) : Effect<string, OrderError, unit> =
    effect {
        if qty <= 0 then
            return! Effect.fail (InvalidQuantity qty)
        elif sku = "ghost" then
            return! Effect.fail (OutOfStock sku)
        else
            return sprintf "shipped %d x %s" qty sku
    }

// ---------------------------------------------------------------------------
// Recover with `catchAll` + native `match` — exhaustive, no stringly-typed tags.
// ---------------------------------------------------------------------------

let placeOrderSafe (sku: string) (qty: int) : Effect<string, string, unit> =
    placeOrder sku qty
    |> Effect.catchAll (fun err ->
        // `err : OrderError` — a real union, so this `match` is checked for
        // exhaustiveness. Effect's `catchTag("OutOfStock", ...)` is stringly typed;
        // this is the same idea with compiler guarantees.
        match err with
        | OutOfStock sku -> Effect.succeed (sprintf "backordered %s" sku)
        | PaymentDeclined reason -> Effect.succeed (sprintf "retry payment: %s" reason)
        | InvalidQuantity q -> Effect.succeed (sprintf "clamped qty %d -> 1" q))

// ---------------------------------------------------------------------------
// Map the error channel (transform 'E without touching success).
// ---------------------------------------------------------------------------

let asMessage (sku: string) (qty: int) : Effect<string, string, unit> =
    placeOrder sku qty
    |> Effect.mapError (fun err ->
        match err with
        | OutOfStock s -> sprintf "%s is unavailable" s
        | PaymentDeclined r -> r
        | InvalidQuantity q -> sprintf "quantity %d is invalid" q)

// ---------------------------------------------------------------------------
// Inspect the FULL outcome by matching on `Exit` and `Cause` directly. ⭐
// Upstream reaches for `Exit.match` / `Cause.match`; here it's just `match`.
// ---------------------------------------------------------------------------

let describe (exit: Exit<string, OrderError>) : string =
    match exit with
    | Success value -> sprintf "ok: %s" value
    // A `Cause` is a list of `Reason`s — pattern-match straight into them.
    | Failure { Reasons = [ Reason.Fail(OutOfStock sku) ] } -> sprintf "out of stock: %s" sku
    | Failure { Reasons = [ Reason.Fail e ] } -> sprintf "failed: %A" e
    | Failure { Reasons = [ Reason.Die ex ] } -> sprintf "defect: %A" ex
    | Failure cause -> sprintf "other: %s" (Cause.render cause)

/// Defects (thrown exceptions) bypass the typed channel; recover them with
/// `catchDefect` when you must.
let guarded: Effect<int, string, unit> =
    Effect.sync (fun () -> failwith "kaboom": int)
    |> Effect.catchDefect (fun (ex: exn) -> Effect.succeed -1)

let demo () =
    banner "03 · Errors — typed failures & native matching"
    printfn "  safe ghost x2   = %s" (run (placeOrderSafe "ghost" 2))
    printfn "  safe widget x0  = %s" (run (placeOrderSafe "widget" 0))
    printfn "  asMessage ghost = %A" (Effect.runSync () (asMessage "ghost" 1))
    printfn "  describe ok     = %s" (describe (Effect.runSync () (placeOrder "widget" 3)))
    printfn "  describe fail   = %s" (describe (Effect.runSync () (placeOrder "ghost" 1)))
    printfn "  recovered defect= %d" (run guarded)
