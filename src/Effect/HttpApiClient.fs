namespace Effect

open System

type HttpApiClientUrlOptions = { BaseUrl: string }

type HttpApiClientUrlInput =
    { Params: Map<string, string>
      Query: UrlParams }

type HttpApiClientEndpointInput =
    { Params: Map<string, string>
      Query: UrlParams
      Headers: Headers
      Payload: HttpBody option }

type HttpApiUrlBuilder =
    { Api: HttpApi
      Options: HttpApiClientUrlOptions
      Endpoints: Map<string, Map<string, HttpApiClientUrlInput -> string>> }

type HttpApiClient =
    { Api: HttpApi
      Options: HttpApiClientUrlOptions
      Endpoints: Map<string, Map<string, HttpApiClientEndpointInput -> Effect<HttpClientResponse, HttpClientError, Context>>> }

[<RequireQualifiedAccess>]
module HttpApiClient =

    let relativeOptions = { BaseUrl = "" }

    let emptyInput =
        { Params = Map.empty
          Query = UrlParams.empty }

    let emptyEndpointInput =
        { Params = Map.empty
          Query = UrlParams.empty
          Headers = Headers.empty
          Payload = None }

    let private encodePathSegment (value: string) = Uri.EscapeDataString(value)

    let private trimBaseUrl (baseUrl: string) = baseUrl.TrimEnd('/')

    let private pathSegments (path: string) =
        path.Trim('/').Split('/')
        |> Array.toList
        |> List.filter (fun segment -> segment <> "")

    let private buildPath (endpoint: HttpApiEndpoint) (input: HttpApiClientUrlInput) =
        let segments =
            pathSegments endpoint.Path
            |> List.choose (fun segment ->
                if segment.StartsWith(":") then
                    let rawName = segment.Substring(1)
                    let optional = rawName.EndsWith("?")
                    let name = if optional then rawName.Substring(0, rawName.Length - 1) else rawName

                    match Map.tryFind name input.Params with
                    | Some value when value <> "" -> Some(encodePathSegment value)
                    | Some _ when optional -> None
                    | Some _ -> invalidArg "params" ("Missing required path parameter: " + name)
                    | None when optional -> None
                    | None -> invalidArg "params" ("Missing required path parameter: " + name)
                else
                    Some segment)

        "/" + String.concat "/" segments

    let buildUrl (options: HttpApiClientUrlOptions) (endpoint: HttpApiEndpoint) (input: HttpApiClientUrlInput) : string =
        let path = buildPath endpoint input
        let baseUrl = trimBaseUrl options.BaseUrl
        let url = if baseUrl = "" then path else baseUrl + path
        UrlParams.appendToUrl url input.Query

    let urlBuilder (api: HttpApi) (options: HttpApiClientUrlOptions) : HttpApiUrlBuilder =
        let endpoints =
            api.Groups
            |> Map.map (fun _ group ->
                group.Endpoints
                |> Map.map (fun _ endpoint -> fun input -> buildUrl options endpoint input))

        { Api = api
          Options = options
          Endpoints = endpoints }

    let urlBuilderRelative (api: HttpApi) : HttpApiUrlBuilder =
        urlBuilder api relativeOptions

    let endpointUrl (group: string) (endpoint: string) (input: HttpApiClientUrlInput) (builder: HttpApiUrlBuilder) : string =
        match Map.tryFind group builder.Endpoints with
        | None -> invalidArg "group" ("Unknown HttpApi group: " + group)
        | Some endpoints ->
            match Map.tryFind endpoint endpoints with
            | Some build -> build input
            | None -> invalidArg "endpoint" ("Unknown HttpApi endpoint: " + group + "." + endpoint)

    let topLevelEndpointUrl (endpoint: string) (input: HttpApiClientUrlInput) (builder: HttpApiUrlBuilder) : string =
        builder.Api.Groups
        |> Map.toList
        |> List.tryPick (fun (groupName, group) ->
            if group.TopLevel && Map.containsKey endpoint group.Endpoints then
                Some(endpointUrl groupName endpoint input builder)
            else
                None)
        |> Option.defaultWith (fun () -> invalidArg "endpoint" ("Unknown top-level HttpApi endpoint: " + endpoint))

    let private endpointRequest (options: HttpApiClientUrlOptions) (endpoint: HttpApiEndpoint) (input: HttpApiClientEndpointInput) =
        let url =
            buildUrl
                options
                endpoint
                { Params = input.Params
                  Query = input.Query }

        let body = defaultArg input.Payload HttpBody.empty

        { HttpClientRequest.make (HttpApiEndpoint.methodString endpoint) url with
            Headers = input.Headers
            Body = body }

    let executeEndpoint
        (options: HttpApiClientUrlOptions)
        (endpoint: HttpApiEndpoint)
        (input: HttpApiClientEndpointInput)
        : Effect<HttpClientResponse, HttpClientError, Context> =
        endpointRequest options endpoint input |> HttpClient.execute

    let make (api: HttpApi) (options: HttpApiClientUrlOptions) : HttpApiClient =
        let endpoints =
            api.Groups
            |> Map.map (fun _ group ->
                group.Endpoints
                |> Map.map (fun _ endpoint -> fun input -> executeEndpoint options endpoint input))

        { Api = api
          Options = options
          Endpoints = endpoints }

    let makeRelative (api: HttpApi) : HttpApiClient =
        make api relativeOptions

    let endpoint
        (group: string)
        (endpoint: string)
        (input: HttpApiClientEndpointInput)
        (client: HttpApiClient)
        : Effect<HttpClientResponse, HttpClientError, Context> =
        match Map.tryFind group client.Endpoints with
        | None -> invalidArg "group" ("Unknown HttpApi group: " + group)
        | Some endpoints ->
            match Map.tryFind endpoint endpoints with
            | Some execute -> execute input
            | None -> invalidArg "endpoint" ("Unknown HttpApi endpoint: " + group + "." + endpoint)

    let topLevelEndpoint
        (endpointName: string)
        (input: HttpApiClientEndpointInput)
        (client: HttpApiClient)
        : Effect<HttpClientResponse, HttpClientError, Context> =
        client.Api.Groups
        |> Map.toList
        |> List.tryPick (fun (groupName, group) ->
            if group.TopLevel && Map.containsKey endpointName group.Endpoints then
                Some(endpoint groupName endpointName input client)
            else
                None)
        |> Option.defaultWith (fun () -> invalidArg "endpoint" ("Unknown top-level HttpApi endpoint: " + endpointName))
