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

    let prefix (prefix: string) (api: HttpApi) =
        { api with Groups = api.Groups |> Map.map (fun _ group -> HttpApiGroup.prefix prefix group) }

    let addError (error: HttpApiContent) (api: HttpApi) =
        { api with Groups = api.Groups |> Map.map (fun _ group -> HttpApiGroup.addError error group) }

    let addErrors (errors: HttpApiContent list) (api: HttpApi) =
        errors |> List.fold (fun acc error -> addError error acc) api

    let middleware (middleware: HttpApiMiddleware) (api: HttpApi) =
        { api with Groups = api.Groups |> Map.map (fun _ group -> HttpApiGroup.middleware middleware group) }
