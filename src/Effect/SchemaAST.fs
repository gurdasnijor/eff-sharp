namespace Effect

/// Introspection helpers over `SchemaAst`. This is intentionally a small,
/// concrete subset of Effect TS's SchemaAST surface: enough for F# consumers to
/// inspect primitive field shapes and annotations without exposing a second
/// schema representation.
[<RequireQualifiedAccess>]
module SchemaAST =

    type AST = SchemaAst

    let rec toType (ast: SchemaAst) : SchemaAst =
        match ast with
        | AAnnotated(inner, _) -> toType inner
        | ARefine(inner, _) -> toType inner
        | ATransform(_, into) -> toType into
        | AMeasured inner -> toType inner
        | AOptionalKey inner -> toType inner
        | other -> other

    let rec resolveAt<'T> (key: string) (ast: SchemaAst) : 'T option =
        match ast with
        | AAnnotated(inner, annotations) ->
            match Map.tryFind key annotations with
            | Some value -> Some(unbox<'T> value)
            | None -> resolveAt key inner
        | ARefine(inner, _) -> resolveAt key inner
        | ATransform(_, into) -> resolveAt key into
        | AMeasured inner -> resolveAt key inner
        | AOptionalKey inner -> resolveAt key inner
        | AOption inner -> resolveAt key inner
        | _ -> None

    let isString ast =
        match toType ast with
        | AString -> true
        | _ -> false

    let isNumber ast =
        match toType ast with
        | AInt
        | AFloat
        | ADecimal -> true
        | _ -> false

    let isBoolean ast =
        match toType ast with
        | ABool -> true
        | _ -> false

    let isLiteral ast =
        match toType ast with
        | ALiteral _ -> true
        | _ -> false

    let isUnion ast =
        match toType ast with
        | AUnion _
        | ATaggedUnion _ -> true
        | _ -> false

    let literalValue ast =
        match toType ast with
        | ALiteral value -> Some value
        | _ -> None
