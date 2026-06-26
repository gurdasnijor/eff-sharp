namespace Effect

/// Minimal transform helpers for generated schema code. Effect TS exposes these
/// as getter transformations; in eff-sharp they are just total functions assembled
/// into `SchemaTransform`.
[<RequireQualifiedAccess>]
module SchemaGetter =

    let transform (f: 'A -> 'B) : 'A -> 'B = f

    let make (decode: 'A -> 'B) (encode: 'B -> 'A) : SchemaTransform<'A, 'B> =
        { Decode = decode
          Encode = encode }
