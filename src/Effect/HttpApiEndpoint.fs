namespace Effect

type HttpMethod =
    | GET
    | POST
    | PUT
    | PATCH
    | DELETE
    | HEAD

type HttpApiEndpointOptions =
    { Params: obj option
      Query: obj option
      Payload: obj option
      Headers: obj option
      Success: HttpApiContent list
      Error: HttpApiContent list }

type HttpApiEndpoint =
    { Name: string
      Method: HttpMethod
      Path: string
      Options: HttpApiEndpointOptions }

[<RequireQualifiedAccess>]
module HttpApiEndpoint =

    let empty =
        { Params = None
          Query = None
          Payload = None
          Headers = None
          Success = [ HttpApiSchema.noContent ]
          Error = [] }

    let private methodName =
        function
        | GET -> "GET"
        | POST -> "POST"
        | PUT -> "PUT"
        | PATCH -> "PATCH"
        | DELETE -> "DELETE"
        | HEAD -> "HEAD"

    let private validateSuccesses method_ (successes: HttpApiContent list) =
        if method_ = HEAD && successes |> List.exists HttpApiSchema.isStream then
            invalidArg "success" "HEAD endpoints cannot use streaming success schemas"

        for schema in successes do
            if HttpApiSchema.hasReservedFailureEvent schema then
                invalidArg "success" "SSE events cannot use the reserved HttpApi stream failure event name"

        let byStatus =
            successes
            |> List.groupBy (fun schema -> schema.Status)

        for status, schemas in byStatus do
            let streams = schemas |> List.filter HttpApiSchema.isStream
            let noContent = schemas |> List.exists HttpApiSchema.isNoContent
            let buffered = schemas |> List.filter (fun schema -> not (HttpApiSchema.isStream schema) && not (HttpApiSchema.isNoContent schema))

            if List.length streams > 1 then
                invalidArg "success" (sprintf "Only one streaming success schema is allowed for status %d" status)

            if not (List.isEmpty streams) && noContent then
                invalidArg "success" (sprintf "Streaming and no-content successes cannot share status %d" status)

            match streams with
            | stream :: _ ->
                let streamType = HttpApiSchema.baseContentType stream.ContentType

                for schema in buffered do
                    if HttpApiSchema.baseContentType schema.ContentType = streamType then
                        invalidArg "success" (sprintf "Streaming and buffered successes cannot share content type %s at status %d" streamType status)
            | [] -> ()

    let private make method_ name path options =
        if options.Error |> List.exists HttpApiSchema.isStream then
            invalidArg "error" "Streaming schemas are only supported as successes"

        validateSuccesses method_ options.Success

        { Name = name
          Method = method_
          Path = path
          Options = options }

    let get name path options = make GET name path options
    let post name path options = make POST name path options
    let put name path options = make PUT name path options
    let patch name path options = make PATCH name path options
    let delete name path options = make DELETE name path options
    let head name path options = make HEAD name path options

    let methodString endpoint = methodName endpoint.Method
