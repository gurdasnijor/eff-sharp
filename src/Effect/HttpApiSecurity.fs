namespace Effect

type HttpApiSecurityApiKeyLocation =
    | Header
    | Query
    | Cookie

type HttpApiSecurity =
    | Http of scheme: string
    | ApiKey of name: string * location: HttpApiSecurityApiKeyLocation
    | Basic

[<RequireQualifiedAccess>]
module HttpApiSecurity =

    let bearer = Http "Bearer"

    let basic = Basic

    let http scheme = Http scheme

    let apiKey name location = ApiKey(name, location)

    let apiKeyHeader name = apiKey name Header

    let apiKeyQuery name = apiKey name Query

    let apiKeyCookie name = apiKey name Cookie

    let scheme =
        function
        | Http s -> Some s
        | ApiKey _
        | Basic -> None

    let apiKeyLocation =
        function
        | ApiKey(_, location) -> Some location
        | _ -> None

    let apiKeyName =
        function
        | ApiKey(name, _) -> Some name
        | _ -> None
