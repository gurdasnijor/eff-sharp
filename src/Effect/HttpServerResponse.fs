namespace Effect

/// Immutable response returned by HTTP handlers.
type HttpServerResponse =
    { Status: int
      StatusText: string option
      Headers: Headers
      Cookies: Cookies
      Body: HttpBody }

type HttpServerResponseOptions =
    { Status: int option
      StatusText: string option
      Headers: Headers
      Cookies: Cookies
      ContentType: string option }

[<RequireQualifiedAccess>]
module HttpServerResponseOptions =

    let empty =
        { Status = None
          StatusText = None
          Headers = Headers.empty
          Cookies = Cookies.empty
          ContentType = None }

[<RequireQualifiedAccess>]
module HttpServerResponse =

    let private headersWithBodyContentType (options: HttpServerResponseOptions) (body: HttpBody) =
        match options.ContentType, HttpBody.contentType body with
        | Some contentType, _ -> Headers.contentType contentType options.Headers
        | None, Some contentType when not (Headers.has "content-type" options.Headers) -> Headers.contentType contentType options.Headers
        | _ -> options.Headers

    let make (options: HttpServerResponseOptions) (body: HttpBody) : HttpServerResponse =
        { Status = defaultArg options.Status 200
          StatusText = options.StatusText
          Headers = headersWithBodyContentType options body
          Cookies = options.Cookies
          Body = body }

    let emptyWith (options: HttpServerResponseOptions) : HttpServerResponse =
        make { options with Status = Some(defaultArg options.Status 204) } HttpBody.empty

    let empty: HttpServerResponse = emptyWith HttpServerResponseOptions.empty

    let textWith (options: HttpServerResponseOptions) (body: string) : HttpServerResponse =
        make options (HttpBody.text body)

    let text (body: string) : HttpServerResponse = textWith HttpServerResponseOptions.empty body

    let html (body: string) : HttpServerResponse =
        textWith { HttpServerResponseOptions.empty with ContentType = Some "text/html" } body

    let jsonWith (options: HttpServerResponseOptions) (body: Json) : HttpServerResponse =
        make options (HttpBody.json body)

    let json (body: Json) : HttpServerResponse = jsonWith HttpServerResponseOptions.empty body

    let bytesWith (options: HttpServerResponseOptions) (body: byte array) : HttpServerResponse =
        make options (HttpBody.bytes body)

    let bytes (body: byte array) : HttpServerResponse = bytesWith HttpServerResponseOptions.empty body

    let streamBytesWith (options: HttpServerResponseOptions) (body: Stream<byte[], obj, Context>) : HttpServerResponse =
        make options (HttpBody.streamBytes body)

    let streamBytes (body: Stream<byte[], obj, Context>) : HttpServerResponse =
        streamBytesWith HttpServerResponseOptions.empty body

    let redirect (location: string) : HttpServerResponse =
        emptyWith
            { HttpServerResponseOptions.empty with
                Status = Some 302
                Headers = Headers.set "location" location Headers.empty }

    let setStatus (status: int) (response: HttpServerResponse) : HttpServerResponse = { response with Status = status }

    let setHeader (key: string) (value: string) (response: HttpServerResponse) : HttpServerResponse =
        { response with Headers = Headers.set key value response.Headers }

    let setCookie (key: string) (value: string) (response: HttpServerResponse) : HttpServerResponse =
        { response with Cookies = Map.add key value response.Cookies }

    let toClientResponse (request: HttpClientRequest) (response: HttpServerResponse) : HttpClientResponse =
        HttpClientResponse.make request response.Status response.Headers response.Body

    let fromClientResponse (response: HttpClientResponse) : HttpServerResponse =
        { Status = response.Status
          StatusText = None
          Headers = response.Headers
          Cookies = Cookies.empty
          Body = response.Body }
