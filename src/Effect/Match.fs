namespace Effect

/// Small pattern-matching helpers inspired by Effect TS `Match`.
///
/// F# has native pattern matching, so this module intentionally does not copy the
/// TypeScript pipeable matcher machinery. It provides the pieces that are useful
/// when porting dynamic matching code: common predicates, literal equality, and
/// ordered first-match evaluation.
[<RequireQualifiedAccess>]
module Match =

    type Pattern<'A> = 'A -> bool
    type Case<'A, 'B> = 'A -> 'B option

    let value (input: 'A) : 'A = input

    let literal (expected: 'A) : Pattern<'A> = fun actual -> actual = expected

    let string (input: obj) : bool =
        match input with
        | :? string -> true
        | _ -> false

    let number (input: obj) : bool =
        match input with
        | :? int
        | :? float
        | :? decimal -> true
        | _ -> false

    let boolean (input: obj) : bool =
        match input with
        | :? bool -> true
        | _ -> false

    let ``when`` (pattern: Pattern<'A>) (evaluate: 'A -> 'B) : Case<'A, 'B> =
        fun input ->
            if pattern input then
                Some(evaluate input)
            else
                None

    let orElse (evaluate: 'A -> 'B) (input: 'A) : 'B = evaluate input

    let first (cases: Case<'A, 'B> list) (fallback: 'A -> 'B) (input: 'A) : 'B =
        cases
        |> List.tryPick (fun case -> case input)
        |> Option.defaultWith (fun () -> fallback input)
