namespace Effect

open System.Collections.Generic

/// Port of repos/effect-smol/packages/effect/src/RequestResolver.ts — batched,
/// de-duplicated resolution of `Request` values.
///
/// A `RequestResolver<'A,'E,'R>` describes HOW to collect `Request.Entry` values,
/// group them into batches, run backend work, and complete each waiting entry
/// (success `'A`, typed error `'E`, services `'R`). It bundles three primitives:
///   * `BatchKey`  — groups entries (entries with equal keys batch together);
///   * `CollectWhile` — given the entries accumulated so far, whether to KEEP
///     collecting into the current batch (`false` cuts a batch — this is how
///     `batchN` bounds batch size);
///   * `RunAll`     — runs one batch and must complete every entry it receives.
///
/// Porting scope & deferrals (per CONVENTIONS — noted, not silent):
///   * The upstream batching *engine* lives in `Effect.request` + the fiber
///     runtime (it collects entries across concurrently-suspended fibers, applies
///     the `delay` window, then calls `runAll`). Our async `Effect` core has no
///     `Effect.request` integration, so this port supplies a **native driver** —
///     `resolveAll`/`resolve` — that takes the list of requests to resolve, makes
///     one `Entry` per request, groups them by `BatchKey`, chunks each group by
///     `CollectWhile`, and runs `RunAll` per batch. This captures "make a
///     resolver, batch, resolve" without the runtime's cross-fiber collection
///     window. Matching upstream's default, identical requests are batched but
///     **not** merged (each is its own entry — see the "preserves individual &
///     identical requests" test); request *de-duplication / caching* is a
///     separate cache layer, deferred.
///   * Deferred (need the `Effect.request` runtime / extra modules): the timed
///     `delay`/`setDelay`/`setDelayEffect` collection window, per-entry captured
///     *context* (entries here carry `Context.empty`; `RunAll` uses the ambient
///     environment), `around`/`aroundRequests`, `race`, tracing spans,
///     caching/persistence (`fromEffectTagged`, Persistence), and the
///     `preCheck` hook. The `Variance`/`TypeId`/`isRequestResolver` JS plumbing is
///     dropped (§3,§4). Combinators are curried, data-last (resolver last) so they
///     compose with `|>`.
/// A resolver for requests producing `'A` / failing with `'E` / needing `'R`.
type RequestResolver<'A, 'E, 'R> =
    internal
        { BatchKey: Entry<'A, 'E, 'R> -> obj
          CollectWhile: Entry<'A, 'E, 'R> list -> bool
          RunAll: Entry<'A, 'E, 'R> list -> Effect<unit, 'E, 'R> }

[<RequireQualifiedAccess>]
module RequestResolver =

    /// The single shared batch key used by `make` (all entries batch together).
    let private singletonKey: obj = obj ()

    // --- constructors ---

    /// Build a resolver from its batching primitives. (RequestResolver.makeWith)
    let makeWith
        (batchKey: Entry<'A, 'E, 'R> -> obj)
        (collectWhile: Entry<'A, 'E, 'R> list -> bool)
        (runAll: Entry<'A, 'E, 'R> list -> Effect<unit, 'E, 'R>)
        : RequestResolver<'A, 'E, 'R> =
        { BatchKey = batchKey
          CollectWhile = collectWhile
          RunAll = runAll }

    /// Build a resolver from a batch runner. All entries share one batch key and
    /// collection never cuts early (one batch per key). (RequestResolver.make)
    let make (runAll: Entry<'A, 'E, 'R> list -> Effect<unit, 'E, 'R>) : RequestResolver<'A, 'E, 'R> =
        makeWith (fun _ -> singletonKey) (fun _ -> true) runAll

    /// Build a resolver that groups requests by a calculated key, passing the key
    /// to the runner. (RequestResolver.makeGrouped)
    let makeGrouped
        (key: Entry<'A, 'E, 'R> -> 'K)
        (resolver: Entry<'A, 'E, 'R> list -> 'K -> Effect<unit, 'E, 'R>)
        : RequestResolver<'A, 'E, 'R> =
        makeWith (fun entry -> box (key entry)) (fun _ -> true) (fun entries ->
            match entries with
            | [] -> Effect.succeed ()
            | head :: _ -> resolver entries (key head))

    /// Build a resolver from a pure per-request function. (RequestResolver.fromFunction)
    let fromFunction (f: Entry<'A, 'E, 'R> -> 'A) : RequestResolver<'A, 'E, 'R> =
        make (fun entries ->
            Effect.sync (fun () -> entries |> List.iter (fun e -> e.CompleteUnsafe(Exit.succeed (f e)))))

    /// Build a resolver from a pure batched function returning one result per
    /// entry, in order. (RequestResolver.fromFunctionBatched)
    let fromFunctionBatched (f: Entry<'A, 'E, 'R> list -> seq<'A>) : RequestResolver<'A, 'E, 'R> =
        make (fun entries ->
            Effect.sync (fun () ->
                Seq.iter2
                    (fun (e: Entry<'A, 'E, 'R>) result -> e.CompleteUnsafe(Exit.succeed result))
                    entries
                    (f entries)))

    /// Build a resolver from an effectful per-request function. A typed failure
    /// completes that entry with `Exit.fail` (it does not fail the batch).
    /// (RequestResolver.fromEffect)
    let fromEffect (f: Entry<'A, 'E, 'R> -> Effect<'A, 'E, 'R>) : RequestResolver<'A, 'E, 'R> =
        make (fun entries ->
            entries |> Effect.forEach
            <| (fun e ->
                f e
                |> Effect.map (fun a -> e.CompleteUnsafe(Exit.succeed a))
                |> Effect.catchAll (fun err -> Effect.sync (fun () -> e.CompleteUnsafe(Exit.fail err))))
            |> Effect.map ignore)

    // --- combinators (data-last: the resolver is the final argument) ---

    /// Bound each batch to at most `n` entries. (RequestResolver.batchN)
    let batchN (n: int) (self: RequestResolver<'A, 'E, 'R>) : RequestResolver<'A, 'E, 'R> =
        { self with
            CollectWhile = fun entries -> List.length entries < n }

    /// Group requests into separate batches by a calculated key.
    /// (RequestResolver.grouped)
    let grouped (key: Entry<'A, 'E, 'R> -> 'K) (self: RequestResolver<'A, 'E, 'R>) : RequestResolver<'A, 'E, 'R> =
        { self with
            BatchKey = fun entry -> box (key entry) }

    /// Replace the collection-cutoff predicate. (RequestResolver.collectWhile)
    let collectWhile
        (predicate: Entry<'A, 'E, 'R> list -> bool)
        (self: RequestResolver<'A, 'E, 'R>)
        : RequestResolver<'A, 'E, 'R> =
        { self with CollectWhile = predicate }

    // --- the native batch/dedupe/resolve driver ---

    /// Split a group's entries into batches, cutting whenever `CollectWhile`
    /// reports the accumulated batch should stop growing.
    let private chunk (self: RequestResolver<'A, 'E, 'R>) (entries: Entry<'A, 'E, 'R> list) =
        let batches = ResizeArray<Entry<'A, 'E, 'R> list>()
        let mutable acc = []

        for e in entries do
            if not (List.isEmpty acc) && not (self.CollectWhile(List.rev acc)) then
                batches.Add(List.rev acc)
                acc <- [ e ]
            else
                acc <- e :: acc

        if not (List.isEmpty acc) then
            batches.Add(List.rev acc)

        List.ofSeq batches

    /// Resolve a list of requests, returning each request's `Exit` in order. One
    /// `Entry` is made per request, entries are grouped by `BatchKey` and chunked
    /// by `CollectWhile`, and `RunAll` runs per batch. Entries left uncompleted by
    /// the resolver fail with a defect; a `RunAll` failure fails its batch's
    /// entries. (the native analogue of `Effect.request` over a resolver)
    let resolveAll
        (self: RequestResolver<'A, 'E, 'R>)
        (requests: Request<'A, 'E, 'R> list)
        : Effect<Exit<'A, 'E> list, 'E2, 'R> =
        Effect(fun fib r ->
            async {
                // One result cell + Entry per request (no implicit de-dup).
                let cells = requests |> List.map (fun _ -> ref None)

                let entries =
                    List.map2
                        (fun req (cell: Exit<'A, 'E> option ref) ->
                            // First completion wins (the resolver completes once;
                            // the batch-failure path only fills what is still empty).
                            Request.makeEntry req Context.empty false (fun exit ->
                                if cell.Value.IsNone then
                                    cell.Value <- Some exit))
                        requests
                        cells

                // Group by batch key (preserving first-seen order), then chunk.
                for _, groupSeq in entries |> Seq.groupBy self.BatchKey do
                    for batch in chunk self (List.ofSeq groupSeq) do
                        let (Effect run) = self.RunAll batch
                        let! exit = run fib r

                        match exit with
                        | Success() -> ()
                        | Failure cause ->
                            // The batch run failed: each entry's completion sink
                            // is independent, so fail this batch's entries here.
                            for e in batch do
                                e.CompleteUnsafe(Failure cause)

                let notCompleted: Exit<'A, 'E> =
                    Exit.die (box (System.Exception "RequestResolver did not complete the request"))

                let results =
                    cells
                    |> List.map (fun cell ->
                        match cell.Value with
                        | Some exit -> exit
                        | None -> notCompleted)

                return Success results
            })

    /// Resolve a single request to its value, folding its `Exit` into the effect.
    let resolve (self: RequestResolver<'A, 'E, 'R>) (request: Request<'A, 'E, 'R>) : Effect<'A, 'E, 'R> =
        resolveAll self [ request ]
        |> Effect.flatMap (fun exits ->
            match List.head exits with
            | Success a -> Effect.succeed a
            | Failure cause -> Effect.failCause cause)
