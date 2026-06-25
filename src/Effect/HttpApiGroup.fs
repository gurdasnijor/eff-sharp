namespace Effect

type HttpApiGroup =
    { Identifier: string
      TopLevel: bool
      Endpoints: Map<string, HttpApiEndpoint> }

[<RequireQualifiedAccess>]
module HttpApiGroup =

    let make identifier =
        { Identifier = identifier
          TopLevel = false
          Endpoints = Map.empty }

    let makeTopLevel identifier =
        { Identifier = identifier
          TopLevel = true
          Endpoints = Map.empty }

    let add (endpoint: HttpApiEndpoint) (group: HttpApiGroup) =
        { group with Endpoints = group.Endpoints |> Map.add endpoint.Name endpoint }

    let addMany (endpoints: HttpApiEndpoint list) (group: HttpApiGroup) =
        endpoints |> List.fold (fun acc endpoint -> add endpoint acc) group

    let prefix (prefix: string) (group: HttpApiGroup) =
        { group with Endpoints = group.Endpoints |> Map.map (fun _ endpoint -> HttpApiEndpoint.prefix prefix endpoint) }

    let addError (error: HttpApiContent) (group: HttpApiGroup) =
        { group with Endpoints = group.Endpoints |> Map.map (fun _ endpoint -> HttpApiEndpoint.addError error endpoint) }

    let addErrors (errors: HttpApiContent list) (group: HttpApiGroup) =
        errors |> List.fold (fun acc error -> addError error acc) group

    let middleware (middleware: HttpApiMiddleware) (group: HttpApiGroup) =
        { group with Endpoints = group.Endpoints |> Map.map (fun _ endpoint -> HttpApiEndpoint.middleware middleware endpoint) }
