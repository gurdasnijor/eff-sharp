namespace Effect

type HttpApi =
    { Identifier: string
      Groups: Map<string, HttpApiGroup> }

[<RequireQualifiedAccess>]
module HttpApi =

    let make identifier =
        { Identifier = identifier
          Groups = Map.empty }

    let add (group: HttpApiGroup) (api: HttpApi) =
        { api with Groups = api.Groups |> Map.add group.Identifier group }

    let addMany (groups: HttpApiGroup list) (api: HttpApi) =
        groups |> List.fold (fun acc group -> add group acc) api
