namespace Effect

/// Structured HTTP client response metadata.
type HttpClientResponse =
    { Request: HttpClientRequest
      Status: int
      Headers: Headers
      Body: HttpBody }

[<RequireQualifiedAccess>]
module HttpClientResponse =

    let make (request: HttpClientRequest) (status: int) (headers: Headers) (body: HttpBody) : HttpClientResponse =
        { Request = request
          Status = status
          Headers = headers
          Body = body }

    let text (request: HttpClientRequest) (status: int) (headers: Headers) (body: string) : HttpClientResponse =
        make request status headers (TextBody(body, Headers.get "content-type" headers))

    let json (request: HttpClientRequest) (status: int) (headers: Headers) (body: Json) : HttpClientResponse =
        make request status headers (HttpBody.json body)

    let streamBytes (request: HttpClientRequest) (status: int) (headers: Headers) (body: Stream<byte[], obj, Context>) : HttpClientResponse =
        make request status headers (HttpBody.streamBytesWithContentType (Headers.get "content-type" headers |> Option.defaultValue "application/octet-stream") body)

    let isSuccess (response: HttpClientResponse) : bool = response.Status >= 200 && response.Status < 300

    let isClientError (response: HttpClientResponse) : bool = response.Status >= 400 && response.Status < 500

    let isServerError (response: HttpClientResponse) : bool = response.Status >= 500 && response.Status < 600

    let bodyText (response: HttpClientResponse) : string option = HttpBody.asText response.Body

    let bodyStream (response: HttpClientResponse) : Stream<byte[], obj, Context> option = HttpBody.asStream response.Body
