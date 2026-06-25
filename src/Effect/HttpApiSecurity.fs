namespace Effect

type HttpApiSecurity =
    | Http of scheme: string
    | ApiKey of name: string * location: string

[<RequireQualifiedAccess>]
module HttpApiSecurity =

    let bearer = Http "Bearer"

    let http scheme = Http scheme

    let apiKey name location = ApiKey(name, location)

    let scheme =
        function
        | Http s -> Some s
        | ApiKey _ -> None
