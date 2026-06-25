/// @title Batching & caching — dedup expensive work
///
/// Upstream's `05_batching` is about not doing the same I/O twice: caching and
/// request batching. eff-sharp ports the **caching** half in full — `Cache`
/// memoizes by key, shares concurrent misses, and can expire entries — plus the
/// `Request` *completion* surface a resolver uses to finish pending work. (The
/// `RequestResolver` that auto-batches is a later slice.)
///
/// `Cache` reads the clock (for TTLs), so these effects require a `Clock`
/// service; we run them with the built-in `Clock.live` context.
module EffSharp.AiDocs.Batching05

open Effect
open EffSharp.AiDocs.Demo

// ───────────────────────────────────────────────────────────────────────────
// 1. Cache — memoize a costly lookup by key
// ───────────────────────────────────────────────────────────────────────────

/// A counter so we can PROVE the lookup runs only on a miss. In real code this
/// would be a DB/HTTP call; here it just measures string length and logs.
let private lookups = ref 0

/// The lookup is itself an `Effect` (env = `Context`, error = `string`). The
/// cache invokes it on a miss, then stores the result.
let private lengthLookup (key: string) : Effect<int, string, Context> =
    Effect.sync (fun () ->
        lookups.Value <- lookups.Value + 1
        printfn "    [lookup] computing %s" key
        key.Length)

/// Authored with the `effect { }` CE — `let!` binds each effect's success, so
/// the cache wiring reads like straight-line code (vs. nested `flatMap`s).
let cacheProgram : Effect<int * int * int * int, string, Context> =
    effect {
        let! cache = Cache.make 100 lengthLookup // capacity 100, entries never expire

        let! a = Cache.get cache "alpha" // miss -> runs lookup
        let! b = Cache.get cache "alpha" // hit  -> NO lookup, returns memoized
        let! _ = Cache.get cache "beta" // miss -> runs lookup

        // Invalidate forces the next read to recompute.
        do! Cache.invalidate cache "alpha"
        let! a2 = Cache.get cache "alpha" // miss again -> runs lookup

        let! size = Cache.size cache
        return (a, b, a2, size)
    }

// ───────────────────────────────────────────────────────────────────────────
// 2. Request — the completion surface a resolver drives
// ───────────────────────────────────────────────────────────────────────────

/// A concrete request is just an F# record implementing the marker interface —
/// no prototype/builder machinery (F#'s nominal types replace upstream's
/// `Request.tagged`/`Class`). `Request<Success, Error, Services>`.
type UserById =
    { Id: int }
    interface Request<string, string, unit>

/// A resolver completes a pending `Entry` by calling one of `Request.succeed` /
/// `fail` / `complete`. Here we capture the outcome into a mutable cell and then
/// match the resulting `Exit` natively.
let completionDemo () : string =
    let mutable outcome : Exit<string, string> = Exit.fail "unset"
    let entry =
        Request.makeEntry
            ({ Id = 1 } :> Request<string, string, unit>)
            Context.empty
            false
            (fun exit -> outcome <- exit)

    // The resolver decided this user resolves to "Ada".
    Request.succeed entry "Ada" |> runUnit

    match outcome with
    | Success name -> sprintf "resolved user 1 -> %s" name
    | Failure cause -> sprintf "failed -> %s" (Cause.render cause)

let run () : unit =
    section "05 Batching & caching"

    lookups.Value <- 0
    let a, b, a2, size = run Clock.live cacheProgram
    printfn "alpha=%d (cached again=%d) alpha-after-invalidate=%d  size=%d" a b a2 size
    // 3 lookups total: alpha (miss), beta (miss), alpha-after-invalidate (miss).
    // The second `get "alpha"` was a cache HIT and ran no lookup.
    printfn "lookups actually run: %d (out of 4 gets)" lookups.Value

    printfn "request completion : %s" (completionDemo ())
