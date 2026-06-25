/// @title ⭐⭐ Bonus — Software Transactional Memory with `stm { }`
///
/// eff-sharp ships a THIRD computation expression (after `effect { }` and
/// `stream { }`): `stm { }`, for composing `TxRef` operations into an atomic
/// transaction. `TxRef.atomically` runs the whole block as one all-or-nothing
/// unit — if it conflicts with a concurrent commit it transparently retries, and
/// if a guard fails nothing is written. This is something Effect's TS API models
/// with combinators; in F# it reads like ordinary sequential code.
///
/// The classic example: transferring money between accounts must touch two cells
/// atomically — no observer can ever see money "in flight" or doubled.
module EffSharp.AiDocs.StmPower

open Effect
open EffSharp.AiDocs.Shared

/// A transaction over two accounts. Note: a plain `if`, `let!`, `do!` — but the
/// WHOLE thing commits atomically (or not at all).
let transfer (from: TxRef<int>) (into: TxRef<int>) (amount: int) : Stm<bool> =
    stm {
        let! balance = TxRef.get from

        if balance < amount then
            return false // insufficient funds -> commit nothing
        else
            do! TxRef.set from (balance - amount)
            let! dest = TxRef.get into
            do! TxRef.set into (dest + amount)
            return true
    }

// ---------------------------------------------------------------------------
// 1. A single transaction with a guard that fails on the second attempt.
// ---------------------------------------------------------------------------

let basic: Effect<bool * bool * int * int, string, unit> =
    effect {
        let! alice = TxRef.make 100
        let! bob = TxRef.make 0

        let! ok1 = TxRef.atomically (transfer alice bob 70) // succeeds (100 -> 30 / 0 -> 70)
        let! ok2 = TxRef.atomically (transfer alice bob 70) // fails: only 30 left, no-op

        let! a = TxRef.atomically (TxRef.get alice)
        let! b = TxRef.atomically (TxRef.get bob)
        return ok1, ok2, a, b // (true, false, 30, 70)
    }

// ---------------------------------------------------------------------------
// 2. Contention: 100 fibers each transfer 1 unit. STM serializes the commits
//    with automatic retry, so the invariant (alice + bob = 100) is preserved and
//    NO update is lost — atomicity under real concurrency.  ⭐
// ---------------------------------------------------------------------------

let underContention: Effect<int * int, string, unit> =
    effect {
        let! alice = TxRef.make 100
        let! bob = TxRef.make 0

        let! fibers = Effect.forEach [ 1..100 ] (fun _ -> Effect.fork (TxRef.atomically (transfer alice bob 1)))

        do! Effect.forEach fibers Effect.join |> Effect.map ignore

        let! a = TxRef.atomically (TxRef.get alice)
        let! b = TxRef.atomically (TxRef.get bob)
        return a, b // (0, 100) — exactly, every time
    }

let demo () =
    banner "Bonus · STM — atomic transactions with stm { }"
    let ok1, ok2, a, b = run basic
    printfn "  transfer #1 ok=%b, #2 ok=%b => alice=%d bob=%d" ok1 ok2 a b
    let a2, b2 = run underContention
    printfn "  100 concurrent transfers => alice=%d bob=%d (sum=%d)" a2 b2 (a2 + b2)
