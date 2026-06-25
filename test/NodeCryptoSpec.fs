module NodeCryptoSpec

open Effect
open Effect.Platform.Node
open Effect.Vitest

let private runCrypto eff =
    eff |> Layer.provide NodeCrypto.layer

describe "NodeCrypto" (fun () ->
    itEffectIn Clock.live "provides secure random bytes" (fun () ->
        effect {
            let! crypto = Crypto.service
            let! bytes = crypto.RandomBytes 8
            toBe bytes.Length 8
        }
        |> runCrypto)

    itEffectIn Clock.live "digests bytes with WebCrypto" (fun () ->
        effect {
            let! crypto = Crypto.service
            let! digest = crypto.Digest SHA256 [| 1uy; 2uy; 3uy |]
            toBe digest.Length 32
        }
        |> runCrypto))
