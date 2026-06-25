namespace Effect

open System
open System.Collections
open System.Globalization
open Microsoft.FSharp.Reflection

/// Port of repos/effect-smol/packages/effect/src/Formatter.ts.
///
/// `format` renders an arbitrary value into a human-readable (non-JSON) string
/// for logs and diagnostics; `formatJson` produces parseable JSON with redaction
/// and circular-reference handling. Upstream inspects untyped JS values at
/// runtime; the F# port mirrors that by inspecting `obj` via reflection, but
/// dispatches on the .NET type system instead of JS runtime guards (CONVENTIONS
/// §1, §3).
///
/// Type mapping (upstream JS construct -> F#):
///   plain object literal  -> anonymous record `{| ... |}`  (no class wrapper)
///   class instance        -> named record / class          (`Name({...})`)
///   Array.isArray         -> F# array / F# list            (bare `[...]`)
///   Map / Set             -> F# `Map` / `Set`              (`Map([[k,v]])` / `Set([...])`)
///   other iterables       -> any `IEnumerable`             (`Name([...])`)
///   Date                  -> `System.DateTime` (ISO 8601, UTC)
///   RegExp                -> `System.Text.RegularExpressions.Regex` (`/pattern/`)
///   Redactable            -> `IRedactable` (see Redactable.fs)
///   discriminated unions  -> `CaseName(args)` (F#-specific; covers Option etc.)
///
/// Omissions / divergences (noted per CONVENTIONS §3):
///   - JS-only inputs have no F# analogue and are dropped: `undefined`, `symbol`,
///     `bigint` is mapped to `System.Numerics.BigInteger` (rendered `123n`),
///     functions, null-prototype objects, `FormData`, typed arrays.
///   - `.NET` lacks JS's "has a non-default `toString`" discriminator (every type
///     overrides `ToString`). Known shapes are matched explicitly first; an
///     overridden `ToString` is then used as a fallback for leaf objects, and
///     reflection over public properties is the final fallback.
///   - Errors render as `TypeName: message`; a nested cause uses
///     `InnerException` (which must itself be an exception), not an arbitrary JS
///     value, so the rendering is `(cause: TypeName: message)`.
[<RequireQualifiedAccess>]
module Formatter =

#if FABLE_COMPILER
    // --- Fable / JS target -------------------------------------------------
    // The .NET implementation below renders via runtime reflection
    // (System.Type / PropertyInfo / FSharpReflection), which Fable erases at
    // compile time — that is the entire source of this module's Fable errors.
    // On JS we don't need it: Fable already renders F# values structurally
    // (records / unions / collections) via `%A`, and produces JSON via the
    // native `JSON.stringify`. Diagnostics fidelity differs slightly from the
    // .NET pretty-printer, but is faithful enough for logs, and redaction is
    // preserved by routing every node through `Redactable.redact`.
    open Fable.Core

    let formatPropertyKey (name: string) : string = JS.JSON.stringify name

    let formatPath (path: string list) : string =
        path |> List.map (fun k -> "[" + formatPropertyKey k + "]") |> String.concat ""

    let formatDate (date: DateTime) : string =
        date.ToUniversalTime().ToString("yyyy-MM-ddTHH:mm:ss.fffZ", CultureInfo.InvariantCulture)

    let format (input: obj) : string = sprintf "%A" input
    let formatWith (_space: int) (input: obj) : string = sprintf "%A" input
    let formatIgnoreToString (input: obj) : string = sprintf "%A" input

    // Redact each node as `JSON.stringify` walks the tree (preserves the .NET
    // path's redaction behavior on the JS target).
    let private redactReplacer (_key: string) (value: obj) : obj = Redactable.redact value

    let formatJson (input: obj) : string =
        JS.JSON.stringify (Redactable.redact input, redactReplacer)

    let formatJsonWith (space: int) (input: obj) : string =
        JS.JSON.stringify (Redactable.redact input, redactReplacer, space)
#else
    [<Literal>]
    let private CIRCULAR = "[Circular]"

    /// JSON-quote a string key (upstream `formatPropertyKey` for string keys).
    let formatPropertyKey (name: string) : string =
        System.Text.Json.JsonSerializer.Serialize(name)

    /// Render a property-key path in bracket notation. (`formatPath`)
    let formatPath (path: string list) : string =
        path |> List.map (fun k -> "[" + formatPropertyKey k + "]") |> String.concat ""

    /// Format a `DateTime` as an ISO 8601 (UTC) string. (`formatDate`)
    let formatDate (date: DateTime) : string =
        date.ToUniversalTime().ToString("yyyy-MM-ddTHH:mm:ss.fffZ", CultureInfo.InvariantCulture)

    let private numericCodes =
        set
            [ TypeCode.SByte
              TypeCode.Byte
              TypeCode.Int16
              TypeCode.UInt16
              TypeCode.Int32
              TypeCode.UInt32
              TypeCode.Int64
              TypeCode.UInt64
              TypeCode.Single
              TypeCode.Double
              TypeCode.Decimal ]

    let private isNumericType (t: Type) =
        numericCodes.Contains(Type.GetTypeCode t)

    let private formatNumber (v: obj) : string =
        Convert.ToString(v, CultureInfo.InvariantCulture)

    let private isGenericDef (def: Type) (t: Type) =
        t.IsGenericType && t.GetGenericTypeDefinition() = def

    let private isFSharpMap t = isGenericDef typedefof<Map<_, _>> t
    let private isFSharpSet t = isGenericDef typedefof<Set<_>> t
    let private isFSharpList t = isGenericDef typedefof<_ list> t
    let private isAnonymous (t: Type) = t.Name.Contains("AnonymousType")

    let private toObjArray (v: obj) : obj[] = [| for x in (v :?> IEnumerable) -> x |]

    let private kvParts (kv: obj) : obj * obj =
        let t = kv.GetType()
        t.GetProperty("Key").GetValue(kv), t.GetProperty("Value").GetValue(kv)

    let private hasCustomToString (t: Type) =
        let m = t.GetMethod("ToString", Type.EmptyTypes)

        not (isNull m)
        && m.DeclaringType <> typeof<obj>
        && m.DeclaringType <> typeof<ValueType>

    let private formatCore (space: int) (ignoreToString: bool) (input: obj) : string =
        let seen = System.Collections.Generic.HashSet<obj>(HashIdentity.Reference)
        let gap = if space <= 0 then "" else String(' ', space)
        let ind d = String.replicate d gap

        // Enter a reference container, guarding against cycles. Upstream never
        // clears `seen` within a single `format`, so a value reached twice on any
        // path renders as `[Circular]` the second time.
        let enter (v: obj) (body: unit -> string) : string =
            if not (isNull v) && seen.Contains v then
                CIRCULAR
            else
                if not (isNull v) then
                    seen.Add v |> ignore

                body ()

        let rec recur (v: obj) (d: int) : string =
            match v with
            | null -> "null"
            | :? string as s -> formatPropertyKey s
            | :? bool as b -> if b then "true" else "false"
            | :? System.Numerics.BigInteger as bi -> string bi + "n"
            | :? DateTime as dt -> formatDate dt
            | :? System.Text.RegularExpressions.Regex as re -> "/" + re.ToString() + "/"
            | :? IRedactable as r -> recur (r.Redact()) d
            | :? exn as e -> formatExn e d
            | _ ->
                let t = v.GetType()

                if isNumericType t then
                    formatNumber v
                elif t.IsArray || isFSharpList t then
                    bracket (toObjArray v) d v
                elif isFSharpMap t then
                    let entries =
                        toObjArray v
                        |> Array.map (fun kv -> let k, value = kvParts kv in box [| k; value |])

                    "Map(" + bracket entries d v + ")"
                elif isFSharpSet t then
                    "Set(" + bracket (toObjArray v) d v + ")"
                elif FSharpType.IsRecord(t, true) then
                    enter v (fun () ->
                        let fields = FSharpType.GetRecordFields(t, true)
                        let kvs = fields |> Array.map (fun p -> formatPropertyKey p.Name, p.GetValue v)
                        let body = objectBody kvs d
                        if isAnonymous t then body else t.Name + "(" + body + ")")
                elif FSharpType.IsUnion(t, true) then
                    let case, vals = FSharpValue.GetUnionFields(v, t, true)

                    if vals.Length = 0 then
                        case.Name
                    else
                        case.Name + "(" + String.Join(",", vals |> Array.map (fun x -> recur x d)) + ")"
                elif (v :? IEnumerable) then
                    t.Name + "(" + bracket (toObjArray v) d v + ")"
                elif not ignoreToString && hasCustomToString t then
                    v.ToString()
                else
                    // Reflect public readable instance properties (class instance).
                    enter v (fun () ->
                        let props =
                            t.GetProperties()
                            |> Array.filter (fun p -> p.CanRead && p.GetIndexParameters().Length = 0)

                        let kvs = props |> Array.map (fun p -> formatPropertyKey p.Name, p.GetValue v)
                        let body = objectBody kvs d
                        t.Name + "(" + body + ")")

        and bracket (items: obj[]) (d: int) (orig: obj) : string =
            enter orig (fun () ->
                if gap = "" || items.Length <= 1 then
                    "[" + String.Join(",", items |> Array.map (fun x -> recur x d)) + "]"
                else
                    let inner =
                        String.Join(",\n" + ind (d + 1), items |> Array.map (fun x -> recur x (d + 1)))

                    "[\n" + ind (d + 1) + inner + "\n" + ind d + "]")

        and objectBody (kvs: (string * obj)[]) (d: int) : string =
            if gap = "" || kvs.Length <= 1 then
                "{"
                + String.Join(",", kvs |> Array.map (fun (k, v) -> k + ":" + recur v d))
                + "}"
            else
                let inner =
                    String.Join(",\n", kvs |> Array.map (fun (k, v) -> ind (d + 1) + k + ": " + recur v (d + 1)))

                "{\n" + inner + "\n" + ind d + "}"

        and formatExn (e: exn) (d: int) : string =
            let head = e.GetType().Name + ": " + e.Message

            match e.InnerException with
            | null -> head
            | cause -> head + " (cause: " + recur cause d + ")"

        recur input 0

    /// Render a value as a compact human-readable string. (`format`)
    let format (input: obj) : string = formatCore 0 false input

    /// `format` with an indentation width (number of spaces). (`format` + `space`)
    let formatWith (space: int) (input: obj) : string = formatCore space false input

    /// `format` without invoking custom `ToString`. (`format` + `ignoreToString`)
    let formatIgnoreToString (input: obj) : string = formatCore 0 true input

    // --- formatJson -------------------------------------------------------

    let private isJsonContainer (t: Type) =
        FSharpType.IsRecord(t, true)
        || t.IsArray
        || isFSharpList t
        || (not (Type.GetTypeCode t = TypeCode.String)
            && typeof<IEnumerable>.IsAssignableFrom t
            && not (isNumericType t))

    let private formatJsonCore (space: int) (input: obj) : string =
        let gap = if space <= 0 then "" else String(' ', space)
        let ind d = String.replicate d gap
        // Ancestors along the *current* branch only: repeated non-circular
        // siblings are preserved; a value reached on its own path is omitted.
        let ancestors = System.Collections.Generic.List<obj>()

        let containsRef (v: obj) =
            ancestors |> Seq.exists (fun a -> obj.ReferenceEquals(a, v))

        let rec ser (v0: obj) (d: int) : string option =
            let v = Redactable.redact v0

            match v with
            | null -> Some "null"
            | :? string as s -> Some(formatPropertyKey s)
            | :? bool as b -> Some(if b then "true" else "false")
            | _ ->
                let t = v.GetType()

                if isNumericType t then
                    Some(formatNumber v)
                elif isJsonContainer t then
                    if containsRef v then
                        None // circular -> omit
                    else
                        ancestors.Add v

                        let result =
                            if
                                t.IsArray
                                || isFSharpList t
                                || (typeof<IEnumerable>.IsAssignableFrom t && not (FSharpType.IsRecord(t, true)))
                            then
                                jsonArray (toObjArray v) d
                            else
                                let fields = FSharpType.GetRecordFields(t, true)
                                let kvs = fields |> Array.map (fun p -> p.Name, p.GetValue v)
                                jsonObject kvs d

                        ancestors.RemoveAt(ancestors.Count - 1)
                        Some result
                elif hasCustomToString t then
                    Some(formatPropertyKey (v.ToString()))
                else if
                    // reflect a class instance into a JSON object
                    containsRef v
                then
                    None
                else
                    ancestors.Add v

                    let props =
                        t.GetProperties()
                        |> Array.filter (fun p -> p.CanRead && p.GetIndexParameters().Length = 0)

                    let kvs = props |> Array.map (fun p -> p.Name, p.GetValue v)
                    let result = jsonObject kvs d
                    ancestors.RemoveAt(ancestors.Count - 1)
                    Some result

        and jsonObject (kvs: (string * obj)[]) (d: int) : string =
            let parts =
                kvs
                |> Array.choose (fun (k, v) -> ser v (d + 1) |> Option.map (fun s -> formatPropertyKey k, s))

            if gap = "" then
                "{" + String.Join(",", parts |> Array.map (fun (k, s) -> k + ":" + s)) + "}"
            elif parts.Length = 0 then
                "{}"
            else
                let inner =
                    String.Join(",\n", parts |> Array.map (fun (k, s) -> ind (d + 1) + k + ": " + s))

                "{\n" + inner + "\n" + ind d + "}"

        and jsonArray (items: obj[]) (d: int) : string =
            let parts = items |> Array.map (fun x -> defaultArg (ser x (d + 1)) "null")

            if gap = "" then
                "[" + String.Join(",", parts) + "]"
            elif parts.Length = 0 then
                "[]"
            else
                let inner = String.Join(",\n", parts |> Array.map (fun s -> ind (d + 1) + s))
                "[\n" + inner + "\n" + ind d + "]"

        defaultArg (ser input 0) "null"

    /// Render a value as compact JSON with redaction and circular-reference
    /// omission. (`formatJson`)
    let formatJson (input: obj) : string = formatJsonCore 0 input

    /// `formatJson` with an indentation width. (`formatJson` + `space`)
    let formatJsonWith (space: int) (input: obj) : string = formatJsonCore space input
#endif
