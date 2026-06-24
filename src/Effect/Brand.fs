namespace Effect

/// Trait: `Brand` — validated/nominal construction of refined values.
///
/// Port of repos/effect-smol/packages/effect/src/Brand.ts. Branding is a
/// TypeScript type-system trick (a phantom `Brand<Key>` intersection) with no
/// runtime footprint beyond an optional validating constructor. F# has no
/// equivalent structural-brand type, so we port the *runtime* surface — the
/// `Constructor` with its throwing / `Option` / `Result` / predicate forms —
/// and let callers use ordinary F# types (or their own single-case DUs) for the
/// nominal distinction.
///
/// Porting notes (see CONVENTIONS.md):
///   * The phantom `Brand<Keys>`/`Branded`/`Unbranded` type-level machinery is
///     dropped — it carries no runtime behaviour.
///   * `make` takes a validator returning `Result<unit, string>` (`Ok ()` =
///     valid, `Error message` = invalid), the idiomatic F# form of upstream's
///     `true | string` filter output.
///   * `check`/`all` (built on the unported `Schema`/`SchemaAST` modules) are
///     skipped; `make` covers predicate-based validation.
///   * Upstream's throwing constructor raises the `BrandError` value itself
///     (`Result.getOrThrow`). .NET can only throw `exn` subtypes, so the
///     throwing `Construct` raises `BrandException`, which carries the
///     structured `BrandError` (recoverable via `BrandException e` /
///     `.Data0`) — faithful to the refinement error rather than a bare
///     `System.Exception`.
type BrandError =
    { Message: string }
    override this.ToString() = sprintf "BrandError(%s)" this.Message

/// Raised by `Constructor.Construct` when validation fails. Carries the
/// structured `BrandError` so callers can pattern-match and recover it.
exception BrandException of BrandError with
    override this.Message = string this.Data0

/// A constructor for a branded/refined value.
type Constructor<'A> =
    { /// Construct, throwing on invalid input.
      Construct: 'A -> 'A
      /// Construct, yielding `None` on invalid input.
      Option: 'A -> 'A option
      /// Construct, yielding `Error (BrandError ...)` on invalid input.
      Result: 'A -> Result<'A, BrandError>
      /// `true` when the input is valid.
      Is: 'A -> bool }

[<RequireQualifiedAccess>]
module Brand =

    /// A constructor that performs no validation (nominal brand).
    let nominal<'a> () : Constructor<'a> =
        { Construct = id
          Option = Some
          Result = Ok
          Is = fun _ -> true }

    /// A constructor that validates input with the given predicate. The
    /// validator returns `Ok ()` for valid values and `Error message` otherwise.
    let make (filter: 'a -> Result<unit, string>) : Constructor<'a> =
        let result (input: 'a) : Result<'a, BrandError> =
            match filter input with
            | Ok () -> Ok input
            | Error message -> Error { Message = message }
        { Construct =
            fun input ->
                match result input with
                | Ok value -> value
                | Error e -> raise (BrandException e)
          Option =
            fun input ->
                match result input with
                | Ok value -> Some value
                | Error _ -> None
          Result = result
          Is =
            fun input ->
                match result input with
                | Ok _ -> true
                | Error _ -> false }
