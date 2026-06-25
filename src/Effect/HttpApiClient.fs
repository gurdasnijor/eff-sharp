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
        let url = trimBaseUrl options.BaseUrl + path
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

    let endpointUrl (group: string) (endpoint: string) (input: HttpApiClientUrlInput) (builder: HttpApiUrlBuilder) : string =
        match Map.tryFind group builder.Endpoints with
        | None -> invalidArg "group" ("Unknown HttpApi group: " + group)
        | Some endpoints ->
            match Map.tryFind endpoint endpoints with
            | Some build -> build input
            | None -> invalidArg "endpoint" ("Unknown HttpApi endpoint: " + group + "." + endpoint)

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
