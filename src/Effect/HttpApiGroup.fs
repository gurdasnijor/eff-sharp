namespace Effect

type HttpApiGroup =
    { Identifier: string
      Endpoints: Map<string, HttpApiEndpoint> }

[<RequireQualifiedAccess>]
module HttpApiGroup =

    let make identifier =
        { Identifier = identifier
          Endpoints = Map.empty }

    let add (endpoint: HttpApiEndpoint) (group: HttpApiGroup) =
        { group with Endpoints = group.Endpoints |> Map.add endpoint.Name endpoint }

    let addMany (endpoints: HttpApiEndpoint list) (group: HttpApiGroup) =
        endpoints |> List.fold (fun acc endpoint -> add endpoint acc) group
