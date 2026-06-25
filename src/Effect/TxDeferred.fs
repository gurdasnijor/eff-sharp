namespace Effect

/// Port of repos/effect-smol/packages/effect/src/TxDeferred.ts — a write-once
/// cell whose completion (`Result<'a,'e>`) lives in transactional state. Readers
/// `await` from inside a transaction: while empty the transaction `retry`s, and
/// when another transaction completes it the waiter resumes with the success
/// value or the typed failure.
///
/// Backed by a `TxRef<Result<'a,'e> option>` (upstream `TxRef<Option<Result>>`),
/// so completion + await compose atomically with other STM operations. `None`
/// means uncompleted; `Some (Ok a)` / `Some (Error e)` is the one-time result.
///
/// Naming note: upstream's `done` is a reserved word in F#'s verbose syntax, so
/// the general completion combinator is named `complete` here; `succeed`/`fail`
/// match upstream.
type TxDeferred<'a, 'e> =
    internal
        { Ref: TxRef<Result<'a, 'e> option> }

[<RequireQualifiedAccess>]
module TxDeferred =

    // --- constructors ---

    /// Create a new empty `TxDeferred`. (TxDeferred.make)
    let make () : Effect<TxDeferred<'a, 'e>, 'E, 'R> =
        TxRef.make (None: Result<'a, 'e> option)
        |> Effect.map (fun ref -> { Ref = ref })

    // --- getters ---

    /// Read the current completion state without retrying. (TxDeferred.poll)
    let poll (self: TxDeferred<'a, 'e>) : Stm<Result<'a, 'e> option> = TxRef.get self.Ref

    /// Read the value, retrying the transaction until completed, then surfacing
    /// the success value or the typed failure into the `Effect` error channel.
    /// (TxDeferred.await)
    let await (self: TxDeferred<'a, 'e>) : Effect<'a, 'e, 'R> =
        TxRef.atomically (
            stm {
                let! current = TxRef.get self.Ref

                match current with
                | None -> return! TxRef.retry
                | Some result -> return result
            }
        )
        |> Effect.flatMap (fun result ->
            match result with
            | Ok value -> Effect.succeed value
            | Error err -> Effect.fail err)

    // --- mutations (return whether this call was the first completion) ---

    /// Complete with a `Result`. Returns `true` on the first completion, `false`
    /// if already completed. (TxDeferred.done)
    let complete (self: TxDeferred<'a, 'e>) (result: Result<'a, 'e>) : Stm<bool> =
        TxRef.modify self.Ref (fun current ->
            match current with
            | Some _ -> (false, current)
            | None -> (true, Some result))

    /// Complete with a success value. (TxDeferred.succeed)
    let succeed (self: TxDeferred<'a, 'e>) (value: 'a) : Stm<bool> = complete self (Ok value)

    /// Complete with a typed failure. (TxDeferred.fail)
    let fail (self: TxDeferred<'a, 'e>) (error: 'e) : Stm<bool> = complete self (Error error)
