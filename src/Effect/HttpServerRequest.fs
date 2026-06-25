namespace Effect

type Cookies = Map<string, string>

/// Server-side representation of an incoming HTTP request.
type HttpServerRequest =
    { Method: string
      Url: string
      OriginalUrl: string
      Headers: Headers
      Cookies: Cookies
      Body: HttpBody
      Source: obj option
      RemoteAddress: string option }

[<RequireQualifiedAccess>]
module Cookies =

    let empty: Cookies = Map.empty

    let parse (header: string) : Cookies =
        header.Split(';')
        |> Array.toList
        |> List.choose (fun part ->
            let trimmed = part.Trim()
            let index = trimmed.IndexOf('=')

            if index <= 0 then
                None
            else
                Some(trimmed.Substring(0, index), trimmed.Substring(index + 1)))
        |> Map.ofList

[<RequireQualifiedAccess>]
module HttpServerRequest =

    let private splitFragment (url: string) =
        let index = url.IndexOf('#')

        if index < 0 then
            url, None
        else
            url.Substring(0, index), Some(url.Substring(index + 1))

    let private splitQuery (url: string) =
        let index = url.IndexOf('?')

        if index < 0 then
            url, UrlParams.empty
        else
            url.Substring(0, index), UrlParams.parseQueryString (url.Substring(index + 1))

    let private authorityEnd (url: string) =
        let scheme = url.IndexOf("://")

        if scheme < 0 then
            None
        else
            let afterScheme = scheme + 3
            let slash = url.IndexOf('/', afterScheme)

            if slash < 0 then Some url.Length else Some slash

    let private pathQueryHash (url: string) =
        match authorityEnd url with
        | Some index when index < url.Length -> url.Substring(index)
        | Some _ -> "/"
        | None -> url

    let private absoluteWithoutQueryHash (url: string) =
        let noHash, _ = splitFragment url
        let path, _ = splitQuery noHash
        path

    let private cookiesFromHeaders (headers: Headers) : Cookies =
        match Headers.get "cookie" headers with
        | Some cookie -> Cookies.parse cookie
        | None -> Cookies.empty

    let make (method: string) (url: string) (headers: Headers) (body: HttpBody) : HttpServerRequest =
        let normalizedUrl = pathQueryHash url

        { Method = method.ToUpperInvariant()
          Url = normalizedUrl
          OriginalUrl = url
          Headers = headers
          Cookies = cookiesFromHeaders headers
          Body = body
          Source = None
          RemoteAddress = None }

    let fromClientRequest (request: HttpClientRequest) : HttpServerRequest =
        let originalUrl = HttpClientRequest.urlWithParams request

        { make request.Method originalUrl request.Headers request.Body with
            Source = Some(box request) }

    let toClientRequest (request: HttpServerRequest) : HttpClientRequest =
        let noHash, hash = splitFragment request.OriginalUrl
        let baseUrl, queryParams = splitQuery noHash

        { Method = request.Method
          Url = absoluteWithoutQueryHash baseUrl
          UrlParams = queryParams
          Hash = hash
          Headers = request.Headers
          Body = request.Body }

    let searchParams (request: HttpServerRequest) : UrlParams =
        let noHash, _ = splitFragment request.Url
        let _, queryParams = splitQuery noHash
        queryParams

    let path (request: HttpServerRequest) : string =
        let noHash, _ = splitFragment request.Url
        let path, _ = splitQuery noHash
        path

    let modify
        (url: string option)
        (headers: Headers option)
        (remoteAddress: string option option)
        (request: HttpServerRequest)
        : HttpServerRequest =
        let nextUrl = defaultArg url request.Url
        let nextHeaders = defaultArg headers request.Headers
        let nextRemoteAddress = defaultArg remoteAddress request.RemoteAddress

        { request with
            Url = nextUrl
            Headers = nextHeaders
            Cookies = cookiesFromHeaders nextHeaders
            RemoteAddress = nextRemoteAddress }
