namespace Effect

/// Structured HTTP client request metadata.
type HttpClientRequest =
    { Method: string
      Url: string
      UrlParams: UrlParams
      Hash: string option
      Headers: Headers
      Body: HttpBody }

[<RequireQualifiedAccess>]
module HttpClientRequest =

    let empty: HttpClientRequest =
        { Method = "GET"
          Url = ""
          UrlParams = UrlParams.empty
          Hash = None
          Headers = Headers.empty
          Body = HttpBody.empty }

    let make (method: string) (url: string) : HttpClientRequest =
        { empty with
            Method = method.ToUpperInvariant()
            Url = url }

    let get (url: string) = make "GET" url

    let post (url: string) = make "POST" url

    let put (url: string) = make "PUT" url

    let patch (url: string) = make "PATCH" url

    let delete (url: string) = make "DELETE" url

    let head (url: string) = make "HEAD" url

    let setMethod (method: string) (request: HttpClientRequest) : HttpClientRequest =
        { request with Method = method.ToUpperInvariant() }

    let setUrl (url: string) (request: HttpClientRequest) : HttpClientRequest = { request with Url = url }

    let prependUrl (prefix: string) (request: HttpClientRequest) : HttpClientRequest =
        { request with Url = prefix.TrimEnd('/') + "/" + request.Url.TrimStart('/') }

    let appendUrlParam (key: string) (value: string) (request: HttpClientRequest) : HttpClientRequest =
        { request with UrlParams = UrlParams.append key value request.UrlParams }

    let setUrlParam (key: string) (value: string) (request: HttpClientRequest) : HttpClientRequest =
        { request with UrlParams = UrlParams.set key value request.UrlParams }

    let setHash (hash: string) (request: HttpClientRequest) : HttpClientRequest = { request with Hash = Some hash }

    let removeHash (request: HttpClientRequest) : HttpClientRequest = { request with Hash = None }

    let setHeader (key: string) (value: string) (request: HttpClientRequest) : HttpClientRequest =
        { request with Headers = Headers.set key value request.Headers }

    let setHeaders (headers: Headers) (request: HttpClientRequest) : HttpClientRequest =
        { request with Headers = Headers.merge request.Headers headers }

    let accept (value: string) (request: HttpClientRequest) : HttpClientRequest = setHeader "accept" value request

    let acceptJson (request: HttpClientRequest) : HttpClientRequest = accept "application/json" request

    let setBody (body: HttpBody) (request: HttpClientRequest) : HttpClientRequest =
        let headers =
            match HttpBody.contentType body with
            | Some contentType -> Headers.contentType contentType request.Headers
            | None -> request.Headers

        { request with
            Body = body
            Headers = headers }

    let bodyText (body: string) (request: HttpClientRequest) : HttpClientRequest = setBody (HttpBody.text body) request

    let bodyTextWithContentType (contentType: string) (body: string) (request: HttpClientRequest) : HttpClientRequest =
        setBody (HttpBody.textWithContentType contentType body) request

    let bodyJson (body: Json) (request: HttpClientRequest) : HttpClientRequest = setBody (HttpBody.json body) request

    let bodyBytes (body: byte array) (request: HttpClientRequest) : HttpClientRequest = setBody (HttpBody.bytes body) request

    let urlWithParams (request: HttpClientRequest) : string =
        let withQuery = UrlParams.appendToUrl request.Url request.UrlParams

        match request.Hash with
        | Some hash when hash.StartsWith("#") -> withQuery + hash
        | Some hash -> withQuery + "#" + hash
        | None -> withQuery
