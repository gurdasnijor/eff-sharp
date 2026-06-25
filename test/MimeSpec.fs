module MimeSpec

open Effect.Platform.Node
open Effect.Vitest

describe "Mime" (fun () ->
    test "resolves common content types from paths" (fun () ->
        toBe (Mime.getType "index.html") (Some "text/html")
        toBe (Mime.getType "/assets/app.js?v=1") (Some "text/javascript")
        toBe (Mime.getType "unknown.ext") None)

    test "defaults unknown paths to application/octet-stream" (fun () ->
        toBe (Mime.getTypeOrDefault "archive.bin") "application/octet-stream"))
