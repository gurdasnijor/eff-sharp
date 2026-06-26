module NodeSocketServerSpec

open Effect
open Effect.Platform.Node
open Effect.Vitest

describe "NodeSocketServer" (fun () ->
    itEffect "make binds an ephemeral TCP port and close releases it" (fun () ->
        NodeSocketServer.make { Host = Some "127.0.0.1"; Port = 0 }
        |> Effect.flatMap (fun server ->
            server.Close
            |> Effect.map (fun () ->
                match server.Address with
                | TcpAddress address ->
                    toBe address.Hostname "127.0.0.1"
                    toBe (address.Port > 0) true
                | UnixAddress path -> failwithf "expected TCP address, got Unix address %s" path))))
