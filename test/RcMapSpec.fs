module RcMapSpec

open Effect
open Effect.Vitest

let private run (eff: Effect<'A, 'E, unit>) : 'A =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure cause -> failwithf "unexpected failure: %s" (Cause.render cause)

let private mkLog () =
    let gate = obj ()
    let items = ResizeArray<string>()
    let add (x: string) = lock gate (fun () -> items.Add x)
    let snapshot () = lock gate (fun () -> List.ofSeq items)
    add, snapshot

type private CountLatch() =
    let gate = obj ()
    let items = ResizeArray<string>()

    member _.Add(x: string) =
        lock gate (fun () -> items.Add x)

    member _.Snapshot() = lock gate (fun () -> List.ofSeq items)

let private yieldFiber: Effect<unit, string, unit> = Effect.sleep 1

let private trackingLookup (addAcq: string -> unit) (addRel: string -> unit) =
    fun (key: string) (scope: Scope<string, unit>) ->
        Scope.acquireRelease
            scope
            (Effect.sync (fun () ->
                addAcq key
                key))
            (fun _ _ -> Effect.sync (fun () -> addRel key))

let private newScope () : Scope<string, unit> = Scope.make ()

type private Key = { Id: int }

describe "RcMap" (fun () ->
    test "deallocation" (fun () ->
        let addAcq, snapAcq = mkLog ()
        let addRel, snapRel = mkLog ()

        let map =
            run (
                effect {
                    let mapScope = newScope ()
                    let! map = RcMap.make mapScope (trackingLookup addAcq addRel)

                    let! foo = Scope.scoped (fun s -> RcMap.get map "foo" s)
                    toBe foo "foo"
                    toBe (snapAcq () = [ "foo" ]) true
                    toBe (snapRel () = [ "foo" ]) true

                    let scopeA = newScope ()
                    let scopeB = newScope ()
                    do! RcMap.get map "bar" scopeA |> Effect.map ignore
                    do! Scope.scoped (fun s -> RcMap.get map "bar" s) |> Effect.map ignore
                    do! RcMap.get map "baz" scopeB |> Effect.map ignore
                    do! Scope.scoped (fun s -> RcMap.get map "baz" s) |> Effect.map ignore
                    toBe (snapAcq () = [ "foo"; "bar"; "baz" ]) true
                    toBe (snapRel () = [ "foo" ]) true

                    do! Scope.close scopeB (Success(box ()))
                    toBe (snapRel () = [ "foo"; "baz" ]) true
                    do! Scope.close scopeA (Success(box ()))
                    toBe (snapRel () = [ "foo"; "baz"; "bar" ]) true

                    let scopeC = newScope ()
                    do! RcMap.get map "qux" scopeC |> Effect.map ignore
                    do! Scope.close mapScope (Success(box ()))
                    toBe (snapRel () = [ "foo"; "baz"; "bar"; "qux" ]) true
                    return map
                }
            )

        match Effect.runSync () (Scope.scoped (fun s -> RcMap.get map "boom" s)) with
        | Failure cause ->
            toBe (cause.Reasons |> List.exists Cause.isInterruptReason) true
        | Success _ -> failwith "expected an interrupt on a closed map")

    test "complex key and keys lookup" (fun () ->
        let map =
            run (
                effect {
                    let mapScope = newScope ()

                    return!
                        RcMap.makeWith mapScope (Some 1) (fun _ -> Duration.zero) (fun (k: Key) _ ->
                            Effect.succeed k.Id)
                }
            )

        let pair =
            run (
                Scope.scoped (fun s ->
                    effect {
                        let! a = RcMap.get map { Id = 1 } s
                        let! b = RcMap.get map { Id = 1 } s
                        return (a, b)
                    })
            )

        toBe (pair = (1, 1)) true

        let keyMap =
            run (
                effect {
                    let mapScope = newScope ()
                    return! RcMap.make mapScope (fun (k: string) _ -> Effect.succeed k)
                }
            )

        let keys =
            run (
                Scope.scoped (fun s ->
                    effect {
                        do! RcMap.get keyMap "foo" s |> Effect.map ignore
                        do! RcMap.get keyMap "bar" s |> Effect.map ignore
                        do! RcMap.get keyMap "baz" s |> Effect.map ignore
                        return! RcMap.keys keyMap
                    })
            )

        toBe (List.sort keys = [ "bar"; "baz"; "foo" ]) true))
