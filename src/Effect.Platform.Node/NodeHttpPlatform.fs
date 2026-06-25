namespace Effect.Platform.Node

open Effect

type NodeHttpPlatform =
    { FileResponse: string -> int -> string option -> Headers -> int option -> int option -> int -> HttpServerResponse
      FileWebResponse: string -> int64 -> int -> string option -> Headers -> HttpServerResponse }

[<RequireQualifiedAccess>]
module NodeHttpPlatform =

    let private withFileHeaders (path: string) (contentLength: int64) (headers: Headers) =
        let headers =
            if Headers.has "content-type" headers then
                headers
            else
                Headers.set "content-type" (Mime.getTypeOrDefault path) headers

        Headers.set "content-length" (string contentLength) headers

    let make: NodeHttpPlatform =
        { FileResponse =
            fun path status statusText headers _start _end contentLength ->
                HttpServerResponse.emptyWith
                    { HttpServerResponseOptions.empty with
                        Status = Some status
                        StatusText = statusText
                        Headers = withFileHeaders path (int64 contentLength) headers }
          FileWebResponse =
            fun name size status statusText headers ->
                HttpServerResponse.emptyWith
                    { HttpServerResponseOptions.empty with
                        Status = Some status
                        StatusText = statusText
                        Headers = withFileHeaders name size headers } }

    let tag: Tag<NodeHttpPlatform> = Tag.make<NodeHttpPlatform> "effect/platform-node/NodeHttpPlatform"

    let layer<'E, 'RIn> : Layer<'E, 'RIn> = Layer.succeed tag make

    let liveContext: Context = Context.make tag make
