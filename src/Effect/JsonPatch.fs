namespace Effect

open System
open System.Globalization
open System.Text

/// Port of repos/effect-smol/packages/effect/src/JsonPatch.ts.
///
/// Computes and applies deterministic patch documents for JSON values. A patch
/// is an ordered list of `add` / `remove` / `replace` operations addressed by
/// JSON Pointer paths (a subset of RFC 6902).
///
/// `get oldValue newValue` produces a patch that yields `newValue` when applied
/// to `oldValue`; `apply patch doc` replays it without mutating the input.
///
/// The upstream `Schema.Json` model lives in another module (owned elsewhere),
/// so the untyped JSON tree is defined locally here as the `Json` DU. F# DUs are
/// immutable and structurally equal, which replaces upstream's `Object.is`
/// short-circuit and the defensive copying done by `apply`.
///
/// Following the Cause/Exit house pattern, the data types live at namespace
/// level (so their cases are usable unqualified) and the operations live in a
/// `[<RequireQualifiedAccess>]` module.
/// An untyped JSON value (locally defined; mirrors `Schema.Json`).
/// Object members are a `Map`, giving order-independent structural equality and
/// sorted-key iteration — matching upstream's sorted-key diff output.
type Json =
    | JNull
    | JBool of bool
    | JNumber of float
    | JString of string
    | JArray of Json list
    | JObject of Map<string, Json>

/// A single JSON Patch operation. (JsonPatchOperation)
type Operation =
    | Add of path: string * value: Json
    | Remove of path: string
    | Replace of path: string * value: Json

[<RequireQualifiedAccess>]
module Json =

    type private Parser(text: string) =
        let mutable index = 0

        member private _.AtEnd = index >= text.Length

        member private _.Peek =
            if index >= text.Length then
                '\u0000'
            else
                text.[index]

        member private _.Next() =
            let ch = text.[index]
            index <- index + 1
            ch

        member private self.SkipWhitespace() =
            while not self.AtEnd && Char.IsWhiteSpace self.Peek do
                index <- index + 1

        member private _.Error(message: string) =
            Error(sprintf "%s at offset %d" message index)

        member private self.Expect(expected: string) =
            if index + expected.Length <= text.Length && text.Substring(index, expected.Length) = expected then
                index <- index + expected.Length
                Ok()
            else
                self.Error("Expected " + expected)

        member private self.ParseString() =
            let builder = StringBuilder()

            let rec loop () =
                if self.AtEnd then
                    self.Error "Unterminated string"
                else
                    match self.Next() with
                    | '"' -> Ok(JString(builder.ToString()))
                    | '\\' ->
                        if self.AtEnd then
                            self.Error "Unterminated escape sequence"
                        else
                            match self.Next() with
                            | '"' ->
                                builder.Append('"') |> ignore
                                loop ()
                            | '\\' ->
                                builder.Append('\\') |> ignore
                                loop ()
                            | '/' ->
                                builder.Append('/') |> ignore
                                loop ()
                            | 'b' ->
                                builder.Append('\b') |> ignore
                                loop ()
                            | 'f' ->
                                builder.Append('\f') |> ignore
                                loop ()
                            | 'n' ->
                                builder.Append('\n') |> ignore
                                loop ()
                            | 'r' ->
                                builder.Append('\r') |> ignore
                                loop ()
                            | 't' ->
                                builder.Append('\t') |> ignore
                                loop ()
                            | 'u' ->
                                if index + 4 > text.Length then
                                    self.Error "Invalid unicode escape"
                                else
                                    let hex = text.Substring(index, 4)
                                    let mutable code = 0

                                    if Int32.TryParse(hex, NumberStyles.HexNumber, CultureInfo.InvariantCulture, &code) then
                                        index <- index + 4
                                        builder.Append(char code) |> ignore
                                        loop ()
                                    else
                                        self.Error "Invalid unicode escape"
                            | other -> self.Error(sprintf "Invalid escape character '%c'" other)
                    | ch when int ch < 0x20 -> self.Error "Invalid control character in string"
                    | ch ->
                        builder.Append(ch) |> ignore
                        loop ()

            if self.Next() <> '"' then
                self.Error "Expected string"
            else
                loop ()

        member private self.ParseNumber() =
            let start = index

            let takeWhile predicate =
                while not self.AtEnd && predicate self.Peek do
                    index <- index + 1

            let readInteger () =
                if self.Peek = '-' then
                    index <- index + 1

                if self.AtEnd then
                    self.Error "Invalid number"
                else
                    match self.Peek with
                    | '0' ->
                        index <- index + 1
                        Ok()
                    | ch when ch >= '1' && ch <= '9' ->
                        takeWhile Char.IsDigit
                        Ok()
                    | _ -> self.Error "Invalid number"

            readInteger ()
            |> Result.bind (fun () ->
                if not self.AtEnd && self.Peek = '.' then
                    index <- index + 1

                    if self.AtEnd || not (Char.IsDigit self.Peek) then
                        self.Error "Invalid number"
                    else
                        takeWhile Char.IsDigit
                        Ok()
                else
                    Ok())
            |> Result.bind (fun () ->
                if not self.AtEnd && (self.Peek = 'e' || self.Peek = 'E') then
                    index <- index + 1

                    if not self.AtEnd && (self.Peek = '+' || self.Peek = '-') then
                        index <- index + 1

                    if self.AtEnd || not (Char.IsDigit self.Peek) then
                        self.Error "Invalid number"
                    else
                        takeWhile Char.IsDigit
                        Ok()
                else
                    Ok())
            |> Result.bind (fun () ->
                let raw = text.Substring(start, index - start)
                let mutable value = 0.0

                if Double.TryParse(raw, NumberStyles.Float, CultureInfo.InvariantCulture, &value) then
                    Ok(JNumber value)
                else
                    self.Error "Invalid number")

        member private self.ParseArray() =
            let values = ResizeArray<Json>()
            index <- index + 1
            self.SkipWhitespace()

            let rec loop () =
                self.SkipWhitespace()

                if not self.AtEnd && self.Peek = ']' then
                    index <- index + 1
                    Ok(JArray(List.ofSeq values))
                else
                    self.ParseValue()
                    |> Result.bind (fun value ->
                        values.Add value
                        self.SkipWhitespace()

                        if not self.AtEnd && self.Peek = ',' then
                            index <- index + 1
                            loop ()
                        elif not self.AtEnd && self.Peek = ']' then
                            index <- index + 1
                            Ok(JArray(List.ofSeq values))
                        else
                            self.Error "Expected ',' or ']'")

            loop ()

        member private self.ParseObject() =
            let fields = ResizeArray<string * Json>()
            index <- index + 1
            self.SkipWhitespace()

            let rec loop () =
                self.SkipWhitespace()

                if not self.AtEnd && self.Peek = '}' then
                    index <- index + 1
                    Ok(JObject(Map.ofSeq fields))
                elif self.AtEnd || self.Peek <> '"' then
                    self.Error "Expected object key"
                else
                    self.ParseString()
                    |> Result.bind (function
                        | JString key ->
                            self.SkipWhitespace()

                            if self.AtEnd || self.Next() <> ':' then
                                self.Error "Expected ':'"
                            else
                                self.SkipWhitespace()
                                self.ParseValue()
                                |> Result.bind (fun value ->
                                    fields.Add(key, value)
                                    self.SkipWhitespace()

                                    if not self.AtEnd && self.Peek = ',' then
                                        index <- index + 1
                                        loop ()
                                    elif not self.AtEnd && self.Peek = '}' then
                                        index <- index + 1
                                        Ok(JObject(Map.ofSeq fields))
                                    else
                                        self.Error "Expected ',' or '}'")
                        | _ -> self.Error "Expected object key")

            loop ()

        member self.ParseValue() =
            self.SkipWhitespace()

            if self.AtEnd then
                self.Error "Unexpected end of input"
            else
                match self.Peek with
                | 'n' -> self.Expect("null") |> Result.map (fun () -> JNull)
                | 't' -> self.Expect("true") |> Result.map (fun () -> JBool true)
                | 'f' -> self.Expect("false") |> Result.map (fun () -> JBool false)
                | '"' -> self.ParseString()
                | '[' -> self.ParseArray()
                | '{' -> self.ParseObject()
                | '-' -> self.ParseNumber()
                | ch when Char.IsDigit ch -> self.ParseNumber()
                | ch -> self.Error(sprintf "Unexpected character '%c'" ch)

        member self.Parse() =
            let result = self.ParseValue()
            self.SkipWhitespace()

            match result with
            | Error _ -> result
            | Ok value ->
                if self.AtEnd then
                    Ok value
                else
                    self.Error "Unexpected trailing input"

    let parse (text: string) : Result<Json, string> =
        Parser(text).Parse()

    let parseUnsafe (text: string) : Json =
        match parse text with
        | Ok value -> value
        | Error message -> failwith message

[<RequireQualifiedAccess>]
module JsonPatch =

    open System.Text.RegularExpressions

    /// A JSON Patch document: an ordered list of operations. (JsonPatch)
    type Patch = Operation list

    // ----------------------------------------------------------------------
    // get — structural diff
    // ----------------------------------------------------------------------

    /// Prefix an operation's path with a parent segment (the functional analogue
    /// of upstream's in-place `prefixPathInPlace`).
    let private prefixPath (parent: string) (op: Operation) : Operation =
        let pre (p: string) = if p = "" then parent else parent + p

        match op with
        | Add(p, v) -> Add(pre p, v)
        | Remove p -> Remove(pre p)
        | Replace(p, v) -> Replace(pre p, v)

    /// Compute a structural patch transforming `oldValue` into `newValue`.
    /// (JsonPatch.get)
    let rec get (oldValue: Json) (newValue: Json) : Patch =
        // Structural equality covers both upstream's `Object.is` fast path and
        // the "no differences found" recursive result.
        if oldValue = newValue then
            []
        else
            match oldValue, newValue with
            | JArray a, JArray b ->
                let av = List.toArray a
                let bv = List.toArray b
                let len1 = av.Length
                let len2 = bv.Length
                let shared = min len1 len2

                let prefixed =
                    [ for i in 0 .. shared - 1 do
                          let path = sprintf "/%d" i

                          for op in get av.[i] bv.[i] do
                              yield prefixPath path op ]

                // Remove from end to start so later indices do not shift.
                let removes = [ for i in len1 - 1 .. -1 .. len2 -> Remove(sprintf "/%d" i) ]

                // Add from beginning to end.
                let adds = [ for i in len1 .. len2 - 1 -> Add(sprintf "/%d" i, bv.[i]) ]

                prefixed @ removes @ adds

            | JObject a, JObject b ->
                let allKeys =
                    Set.union (a |> Map.toSeq |> Seq.map fst |> Set.ofSeq) (b |> Map.toSeq |> Seq.map fst |> Set.ofSeq)
                    |> Set.toList // Set<string> iterates in sorted (ordinal) order

                [ for key in allKeys do
                      let path = "/" + JsonPointer.escapeToken key

                      match Map.tryFind key a, Map.tryFind key b with
                      | Some va, Some vb ->
                          for op in get va vb do
                              yield prefixPath path op
                      | None, Some vb -> yield Add(path, vb)
                      | Some _, None -> yield Remove path
                      | None, None -> () ]

            | _ -> [ Replace("", newValue) ]

    // ----------------------------------------------------------------------
    // apply — replay a patch
    // ----------------------------------------------------------------------

    /// A step recorded while walking to a pointer's parent, used to rebuild the
    /// document immutably afterwards.
    type private StackEntry =
        | ArrEntry of Json list * int
        | ObjEntry of Map<string, Json> * string

    /// Tokenize a JSON Pointer into unescaped reference tokens. `""` -> root.
    let private tokenize (pointer: string) : string list =
        if pointer = "" then
            []
        elif pointer.[0] <> '/' then
            failwithf "Invalid JSON Pointer, it must start with \"/\": %s" pointer
        else
            pointer.Split('/')
            |> Array.toList
            |> List.tail
            |> List.map JsonPointer.unescapeToken

    /// Convert a reference token to a non-negative array index (rejects `-` and
    /// negatives).
    let private toIndex (token: string) : int =
        if Regex.IsMatch(token, @"^(0|[1-9]\d*)$") then
            int token
        else
            failwithf "Invalid array index: \"%s\"" token

    /// Walk to the parent of `pointer`, recording the path. Returns None if the
    /// parent path cannot be resolved.
    let private resolveParent (doc: Json) (pointer: string) : (StackEntry list * Json * string) option =
        match tokenize pointer with
        | [] -> None // caller handles root
        | tokens ->
            let lastToken = List.last tokens
            let parents = tokens |> List.take (tokens.Length - 1)

            let rec walk (stack: StackEntry list) (cur: Json) (rest: string list) =
                match rest with
                | [] -> Some(List.rev stack, cur, lastToken)
                | token :: tl ->
                    match cur with
                    | JArray xs ->
                        let idx = toIndex token

                        if idx < 0 || idx >= xs.Length then
                            None
                        else
                            walk (ArrEntry(xs, idx) :: stack) xs.[idx] tl
                    | JObject m ->
                        match Map.tryFind token m with
                        | Some v -> walk (ObjEntry(m, token) :: stack) v tl
                        | None -> None
                    | _ -> None

            walk [] doc parents

    /// Rebuild the document by writing `newParent` back through `stack`.
    let private rebuildFromStack (stack: StackEntry list) (newParent: Json) : Json =
        List.foldBack
            (fun entry acc ->
                match entry with
                | ArrEntry(xs, idx) -> JArray(List.updateAt idx acc xs)
                | ObjEntry(m, key) -> JObject(Map.add key acc m))
            stack
            newParent

    let private addAt (doc: Json) (pointer: string) (value: Json) : Json =
        if pointer = "" then
            value
        else
            match resolveParent doc pointer with
            | None -> failwithf "Cannot add at \"%s\" (parent not found or not a container)." pointer
            | Some(stack, parent, lastToken) ->
                match parent with
                | JArray xs ->
                    let idx = if lastToken = "-" then xs.Length else toIndex lastToken

                    if idx < 0 || idx > xs.Length then
                        failwithf "Array index out of bounds at \"%s\"." pointer

                    rebuildFromStack stack (JArray(List.insertAt idx value xs))
                | JObject m -> rebuildFromStack stack (JObject(Map.add lastToken value m))
                | _ -> failwithf "Cannot add at \"%s\" (parent not found or not a container)." pointer

    let private setAt (doc: Json) (pointer: string) (value: Json option) (mode: string) : Json =
        if pointer = "" then
            match mode, value with
            | "remove", _ -> failwith "Unsupported operation at the root"
            | _, None -> failwith "Unsupported operation at the root"
            | _, Some v -> v
        else
            match resolveParent doc pointer with
            | None -> failwithf "Cannot %s at \"%s\" (parent not found or not a container)." mode pointer
            | Some(stack, parent, lastToken) ->
                match parent with
                | JArray xs ->
                    if lastToken = "-" then
                        failwithf "\"-\" is not valid for %s at \"%s\"." mode pointer

                    let idx = toIndex lastToken

                    if idx < 0 || idx >= xs.Length then
                        failwithf "Array index out of bounds at \"%s\"." pointer

                    let updated =
                        if mode = "remove" then
                            List.removeAt idx xs
                        else
                            List.updateAt idx (Option.get value) xs

                    rebuildFromStack stack (JArray updated)
                | JObject m ->
                    if not (Map.containsKey lastToken m) then
                        failwithf "Property \"%s\" does not exist at \"%s\"." lastToken pointer

                    let updated =
                        if mode = "remove" then
                            Map.remove lastToken m
                        else
                            Map.add lastToken (Option.get value) m

                    rebuildFromStack stack (JObject updated)
                | _ -> failwithf "Cannot %s at \"%s\" (parent not found or not a container)." mode pointer

    /// Apply a patch to a document, sequentially and without mutation.
    /// (JsonPatch.apply)
    let apply (patch: Patch) (oldValue: Json) : Json =
        patch
        |> List.fold
            (fun doc op ->
                match op with
                | Replace(path, value) ->
                    if path = "" then
                        value
                    else
                        setAt doc path (Some value) "replace"
                | Add(path, value) -> addAt doc path value
                | Remove path -> setAt doc path None "remove")
            oldValue
