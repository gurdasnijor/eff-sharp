namespace Effect

/// Case-insensitive HTTP headers stored with lower-case keys.
type Headers = Map<string, string>

[<RequireQualifiedAccess>]
module Headers =

    let private normalizeKey (key: string) = key.Trim().ToLowerInvariant()

    let empty: Headers = Map.empty

    let isEmpty (headers: Headers) = Map.isEmpty headers

    let ofList (headers: (string * string) list) : Headers =
        headers
        |> List.fold (fun acc (key, value) -> Map.add (normalizeKey key) value acc) empty

    let ofSeq (headers: seq<string * string>) : Headers =
        headers
        |> Seq.fold (fun acc (key, value) -> Map.add (normalizeKey key) value acc) empty

    let toList (headers: Headers) : (string * string) list = Map.toList headers

    let toSeq (headers: Headers) : seq<string * string> = Map.toSeq headers

    let set (key: string) (value: string) (headers: Headers) : Headers =
        Map.add (normalizeKey key) value headers

    let get (key: string) (headers: Headers) : string option = Map.tryFind (normalizeKey key) headers

    let has (key: string) (headers: Headers) : bool = Map.containsKey (normalizeKey key) headers

    let remove (key: string) (headers: Headers) : Headers = Map.remove (normalizeKey key) headers

    let merge (left: Headers) (right: Headers) : Headers =
        right |> Map.fold (fun acc key value -> Map.add key value acc) left

    let contentType (value: string) (headers: Headers) : Headers = set "content-type" value headers

    let accept (value: string) (headers: Headers) : Headers = set "accept" value headers
