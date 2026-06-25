namespace Effect.Platform.Node

open Effect
open Fable.Core

/// Node implementation of the `Crypto` service.
[<RequireQualifiedAccess>]
module NodeCrypto =

    [<Emit("globalThis.crypto.getRandomValues(new Uint8Array($0))")>]
    let private randomBytesJs (_size: int) : byte[] = jsNative

    [<Emit("globalThis.crypto.subtle.digest($0, $1).then(function(buffer) { return new Uint8Array(buffer); })")>]
    let private digestJs (_algorithm: string) (_data: byte[]) : JS.Promise<byte[]> = jsNative

    let private randomBytesUnsafe (size: int) : byte[] = randomBytesJs size

    let private digest (algorithm: DigestAlgorithm) (data: byte[]) : Effect<byte[], PlatformError, Context> =
        Effect.tryPromiseJS
            (fun () -> digestJs algorithm.Name data)
            (fun ex ->
                PlatformError.systemError
                    { Tag = SystemErrorTag.Unknown
                      Module = "NodeCrypto"
                      Method = "digest"
                      Description = Some ex.Message
                      Syscall = None
                      PathOrDescriptor = None
                      Cause = Some(box ex) })

    let crypto: Crypto = Crypto.make randomBytesUnsafe digest

    let layer<'E, 'RIn> : Layer<'E, 'RIn> = Layer.succeed Crypto.tag crypto

    let liveContext: Context = Context.make Crypto.tag crypto
