namespace Effect

/// Small Option helper surface for consumers porting from Effect TS. The value
/// representation is native F# option; this module only provides familiar names.
[<RequireQualifiedAccess>]
module Option =

    type Option<'T> = 'T option

    let some (value: 'T) : 'T option = Some value

    let none<'T> () : 'T option = None

    let isSome (value: 'T option) : bool = Microsoft.FSharp.Core.Option.isSome value

    let isNone (value: 'T option) : bool = Microsoft.FSharp.Core.Option.isNone value

    let getOrElse (fallback: unit -> 'T) (value: 'T option) : 'T =
        match value with
        | Some v -> v
        | None -> fallback ()

    let getOrUndefined (value: 'T option) : 'T =
        match value with
        | Some v -> v
        | None -> Unchecked.defaultof<'T>

    let fromNullable (value: 'T) : 'T option =
        if obj.ReferenceEquals(box value, null) then None else Some value

    let fromNullishOr (value: 'T) : 'T option = fromNullable value

    let ``match`` (onNone: unit -> 'B) (onSome: 'T -> 'B) (value: 'T option) : 'B =
        match value with
        | Some v -> onSome v
        | None -> onNone ()
